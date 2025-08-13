
# ITERATIONS ----
# this all needs to be done each iteration, so that new individual-level compensations, elasticities, etc. are drawn

for (iteration in 1:nIterations){
  
  print(iteration)
  
  set.seed(2345 + iteration)
  
  
  ## Prepare baseline data  ----
  tidy_simulation <- nhanes_adults_include %>%
  
    ### Variables that are constant across policies ----
    # these are all relatively straightforward to (1) abstract out if we want to modify it in a sensitivity analysis
    # and (2) use an if statement/case_when to create group-specific
    # for group specific estimates, I expect it would be easier to assign people a base value matching their group...
    # and then add th individual-specific draws into the rowwise/mutate statements below
  
    mutate(compensationFactor = runif(min=comp_min,max=comp_max, n=n()),
           
           # price per ounce from discrete distribution created above
           pricePerOunce = sample(x=c(sodaPPOz, sportsPPOz, energyPPOz, readyPPOz, juicePPOz),
                                  prob = c(sodaFreq/totalFreq, sportsFreq/totalFreq, energyFreq/totalFreq, readyFreq/totalFreq, juiceFreq/totalFreq), # probs calculated from frequencies in Leider 2019
                                  size=n(), replace=TRUE),
           
           # grams to ounces: needed if we are calculating tax REVENUE since the units are price per ounce
           ssbOz = affectedSSBgramsTotal*0.035274, # 1 gram  = 0.03 oz; grams * (0.03 oz / grams) = oz
           ssb8OzServings = ssbOz/8, 
           
           # baseline weight setup for weight change modeling (note no longer needed in diabetes model)
           Yr_0_Wt = weight, 
           
           # baseline diabetes
           Yr_0_Diab = diabYN, 
           
           # used to hold people at the same relative level of diabetes risk as they age
           percentileDiabThings = runif(n=n(), min=0, max=1), 
           
           # proportion passed through, from Andreyevna review
           passThrough = runif(n=n(), min=pt_min, max=pt_max), # proportion passed through
           
           # price elasticity of demand (% change in consumption for % change in price) from Andreyevna review
           elasticityAverage = -rexp(rate = 1/elasticity, n=n()), 
           
           # take input qaly and convert to disutility (same across everyone)
           diabetesDisutility = -rbeta(shape1=qalyShape1, 
                                       shape2=qalyShape2, 
                                       n=n()), 
           
           # draw RR using lognormal values
           # exponentiate to get it back on a ratio scale
           ssbRR = exp(rnorm(n=n(), 
                             mean=lnSsbOnDiabRR,
                             sd=SElnSsbOnDiabRR))
           
           ) %>%
    
    rowwise() %>%
    
    # draw diabetes not using runit() but qunif() since we are using the percentile to keep relative risk constant within person
    mutate(diab1YrInc = qunif(p = percentileDiabThings, 
                              min = incidenceLL, 
                              max = incidenceUL),
           
           # convert rate to probability
           diab1YrProb = (1-exp(-(diab1YrInc/1000)*1)), # these are 1 year rates per 1,000 people
           
           # this is useful to compare the change in probabilities to
           diab1YrProb_START = diab1YrProb) %>%
    
    ungroup()
  

  # head(tidy_simulation %>% select(ageSim, incidence, incidenceLL, incidenceUL, diab1YrInc, diab1YrProb))
  
  
  ## Policy specific datasets ----
  
  # all datsets must have consistent sim-required inputs, including: 
  # Yr_0_Wt, Yr_0_Diab, ssbOz, risk ratios, incidences (all included in tidy_simulation)
  # pctChangeSSBs is what is required from each policy scenario, along with the implementation vector to denote repeals, etc. 
  
  fullImplementation = c(rep(1, yearsSim))
  
  ### Status Quo ----
  statusQuo <- tidy_simulation %>%
    mutate(policy = "sq", 
           pctChangeSSBs = 0)

  
  ### 2 cent tax ----
  # direct effect from Kaplan et al., 2024
  # have to use triangular distribution because the CI crosses 0 and -1 (beta is out)
  # normal distribution works but end up with people on tails with changes that are just too extreme
  tx2c <- tidy_simulation %>%
    mutate(policy="tx2c",
           pctChangeSSBs = EnvStats::rtri(min = -1.048,
                                          mode = -0.415,
                                          max = 0.219,
                                          n = nrow(tidy_simulation))
    )
  
  
  ### 2 cent tax, repealed after 1 year ----
  # input dataset is the same as tax2c
  tx2cRepealImplementationVector = c(1,0,0,0,0,0,0,0,0,0,0)
  
  
  # based on Anna's email and benchmarking a distribution of 
  # average reductions of about 0.18
  # with a CI spread of 0.08 (8 pp)
  # textWlVar < textWlAvg*(1-textWlAvg)
  
  textWlAvg = 0.18
  textWlSpread = 0.08
  textWlVar = (textWlSpread/1.96)^2
  
  textShape1 = textWlAvg*((textWlAvg*(1-textWlAvg)/textWlVar)-1)
  textShape2 = (1-textWlAvg)*((textWlAvg*(1-textWlAvg)/textWlVar)-1)
  
  ### TEXT warning label ----
  textwl <- tidy_simulation %>%
    mutate(policy="textwl", 
           pctChangeSSBs = -rbeta(shape1=textShape1, 
                                  shape2=textShape2, 
                                  n=nrow(tidy_simulation)))
  
  ### Text warning label, delayed ----
  wlDelayImplementationVector = c(0, 0, 0.5, 0.5, 1, 1, 1, 1, 1, 1)

  
  ### GRAPHIC warning label with Marissa's study ----
  # (51.7-82.1)/82.1
  graphicWlAvg = 0.37 # informed by Marissa study
  graphicWlSpread = 0.15 # bigger spread because we are uncertain/newer policy
  graphicWlVar = (graphicWlSpread/1.96)^2
  
  graphicShape1 = graphicWlAvg*((graphicWlAvg*(1-graphicWlAvg)/graphicWlVar)-1)
  graphicShape2 = (1-graphicWlAvg)*((graphicWlAvg*(1-graphicWlAvg)/graphicWlVar)-1)
  
  graphicwl <- tidy_simulation %>%
    mutate(policy="graphicwl",
           pctChangeSSBs = -rbeta(shape1=graphicShape1, 
                                  shape2=graphicShape2, 
                                  n=nrow(tidy_simulation)))
  
  
  noMoreSSBs <- tidy_simulation %>%
    mutate(policy="nossbs", 
           pctChangeSSBs = -1)

  
# Initialize functions ----
  ## Diabetes natual history model ----
  source(file=here("analysis", "00b1_diabetes-model.R"))
  
  ## Survey weighting/estimates ----
  source(file=here("analysis", "00b2_survey-estimates.R"))
  
  
  
# Apply functions to policy datasets ----
  # apply that function to simulate changes/change over time for each policy
  tibbleOfTibbles <- tribble(~policy, ~abbrev, ~dataset, ~impVector,  
                             "Status Quo", "sq", statusQuo, fullImplementation, 
                             "2 Cent Excise Tax", "tx2c", tx2c, fullImplementation,
                             "2 Cent Excise Tax, repeal", "tx2cRepeal", tx2c, tx2cRepealImplementationVector,
                             "Text Warning Label", "textwl", textwl, fullImplementation,
                             "Graphic Warning Label", "graphicwl", graphicwl, fullImplementation, 
                             "No More SSBs", "noMoreSSBs", noMoreSSBs, fullImplementation)

  # do the simulation for the policy datasets created above
    # these are just adults 18-65, so will re-bind with their other sample friends before we do any survey statistics
  doingSim <- tibbleOfTibbles %>%
    rowwise() %>%
    mutate(simOutput = list(DO_THE_SIM(policySpecificDataset=dataset, 
                                       inputImplementationVector=impVector)), 
           ncols = list(ncol(simOutput)))

  # bind the analytic dataset people to the children, older adults, and excluded adults
  output <- doingSim %>%
    rowwise() %>%
    mutate(rbindedData = list(bind_rows(simOutput, nhanes_children_over65, nhanes_adults_exclude)))
  
  # should be the same number of rows in all datasets
  #lapply(output$rbindedData, nrow)
  
  # apply the survey analysis function to each of the output datasets
  outputSurveyAnalysis <- output %>%
    rowwise() %>%
    mutate(results = list(SURVEY_ANALYSIS(rbindedData)))
  
  # save iteration results with appropriate identifiers and only the needed results
  iterationResults <- outputSurveyAnalysis %>%
    select(policy, abbrev, results) %>%
    mutate(iteration = iteration)
  
  # save into a folder for pulling into the output analytics 
  saveRDS(iterationResults, file=here("analysis", "output", "iterations", paste0("iter", iteration, "_", simName, ".RDS")))
  
# Clean up and get ready to repeat ----
  rm(tidy_simulation, tibbleOfTibbles, doingSim, output, outputSurveyAnalysis, iterationResults)
  
  gc()
  
  iteration = iteration + 1
  
}




