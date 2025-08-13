library(tidyverse)
#library(dplyr)
library(here)
library(survey)
library(haven)
#library(fuzzyjoin)



sim_id <- as.integer(1)
set.seed(1235689 + sim_id) # this should be reset to the same seed each time so that the differences in our results are due to changes in the parameters alone
simName <- "validation"

nIterations <- 1200 # how many times new baseline distribution data at the person-level (elasticities, etc.) will be drawn
yearsSim <- 10

diabMeans_ageSpecific <- tibble(iteration = numeric(), means=list())


ssbOnDiabRR_input = 1.19
ssbOnDiabRRLL_input = 1.12 
ssbOnDiabRRUL_input = 1.27

diabIncidence <- readRDS(file = here("analysis", "data", "diabIncidenceClean.RDS")) %>% filter(year==2009)

nhanes <- readRDS(file = here("analysis", "data", "nhanes_0910_cleaned.RDS")) 


  nhanes_children_over65 <- nhanes %>% filter(x18to64==0)
  #stopifnot(nrow(nhanes_children_over65)==3879)
  
  nhanes_adults_exclude <- nhanes %>% filter(x18to64==1 & analyticSampleIndicator==0)
  stopifnot(nrow(nhanes_adults_exclude)==0) # in this situation! in the more restricted analytic, it should be equal to 46
  
  nhanes_adults_include <- nhanes %>% filter(x18to64==1 & analyticSampleIndicator==1) %>%
    mutate(ageSim = age, 
           
           # generated to match with the datasets that contain info used in modeling
           incAgeRange = case_when(age>=18 & age<45 ~ 1, 
                                   age>=45 & age<65 ~ 2, 
                                   age>=65 ~ 3, # should never be used
                                   TRUE ~ as.double(NA)), # should never be used
           
           ssbOnDiabRR = ssbOnDiabRR_input,
           ssbOnDiabRRLL = ssbOnDiabRRLL_input, 
           ssbOnDiabRRUL = ssbOnDiabRRUL_input, 
           
           lnSsbOnDiabRR = log(ssbOnDiabRR), 
           SElnSsbOnDiabRR = (log(ssbOnDiabRRUL) - log(ssbOnDiabRRLL))/(2*1.96)
           
    ) %>%
    
    # diabetes incidence rates
    left_join(diabIncidence %>% select(-minInterval, -maxInterval), by="incAgeRange") 
  
  
  stopifnot(nrow(nhanes) == nrow(nhanes_children_over65) + nrow(nhanes_adults_include) + nrow(nhanes_adults_exclude))


# ITERATIONS ----
# this all needs to be done each iteration, so that new individual-level compensations, elasticities, etc. are drawn

for (iteration in 1:nIterations){
  
  print(iteration)
  
  ## Prepare baseline data  ----
  tidy_simulation <- nhanes_adults_include %>%
    
  mutate(compensationFactor = runif(min=0.63,max=1.00, n=n()),
         ssbOz = affectedSSBgramsTotal*0.035274, # needed for tax revenue calculations: 1 gram  = 0.03 oz; grams * (0.03 oz / grams) = oz
         ssb8OzServings = ssbOz/8, 
         
         # baseline weight setup for weight change modeling (note no longer needed)
         Yr_0_Wt = weight, 
         
         # baseline diabetes
         Yr_0_Diab = diabYN, 

         # for keeping risk consistent across age brackets
         percentileDiabThings = runif(n=n(), min=0, max=1), 
         
         # risk ratio to connect ssb consumption and diabetes risk
         ssbRR = exp(rnorm(n=n(), 
                           mean=lnSsbOnDiabRR,
                           sd=SElnSsbOnDiabRR))
         
  ) %>%
    
    rowwise() %>%
    
    mutate(diab1YrInc = qunif(p = percentileDiabThings, 
                              min = incidenceLL, 
                              max = incidenceUL),
           
           diab1YrProb = (1-exp(-(diab1YrInc/1000)*1)), # these are 1 year rates per 1,000 people
           diab1YrProb_START = diab1YrProb) %>%
    
    ungroup()
  
  
  # head(tidy_simulation %>% select(ageSim, incidence, incidenceLL, incidenceUL, diab1YrInc, diab1YrProb))
  
  
  
  
  ## Policy specific datasets ----
  
  # all datsets must have consistent sim-required inputs, including: 
  # Yr_0_Wt, Yr_0_Diab, ssbOz, risk ratios, incidences (all included in tidy_simulation)
  # pctChangeSSBs is what is required from each policy scenario, along with the implementation vector to denote repeals, etc. 
  
  fullImplementation = c(rep(1, yearsSim))
  
  statusQuo <- tidy_simulation %>%
    mutate(policy = "sq", 
           pctChangeSSBs = 0)
  

  
  # previously policy-specific, now just status quo
    tmp <- statusQuo %>%
      mutate("gramsChange" = (affectedSSBgramsTotal*pctChangeSSBs),
             "x250mLChange" = gramsChange/250,
             "Yr_0_x250mLChange" = 0,
      )
    
    
    yearlyMeans <- tibble(year = numeric(), data=list())
    
    year0 <- bind_rows(tmp, nhanes_children_over65, nhanes_adults_exclude)
    
    nhanesDesign <- 
      svydesign(id =~psu, 
                strata = ~stratum, 
                weight = ~wtdrd1, 
                data = year0, 
                nest=TRUE)
    
    subpop_design <-subset(nhanesDesign, analyticSampleIndicator==1)
    
    overallAvg <- svyby(formula = ~ diabYN, 
                        by = ~analyticSampleIndicator, 
                        FUN = svymean, 
                        design=subpop_design, 
                        na.rm = TRUE, 
                        keep.var = FALSE)
    
    # take that year's diabetes y/n variable
    averages <- svyby(formula = ~ diabYN, 
                      by = ~young, 
                      FUN = svymean, 
                      design=subpop_design, 
                      na.rm = TRUE, 
                      keep.var = FALSE)
    
    yearlyMeans <- yearlyMeans %>%
      add_row(year = 0, data=list(as_tibble(bind_rows(overallAvg, averages))))
    
    
    for (i in 1:yearsSim){
      
      #print(i)
      
      set.seed(iteration + i + sim_id)
      
      
      tmp <- tmp %>%
        mutate(
          
          # NOTE that the implementation vector is a GLOBAL adjustment, which adjusts *each persons* 250mL change by that value
          # this is NOT the same as only 50% of folks getting a policy in year 1 due to slower rollout
          "Yr_{i}_x250mLChange" := x250mLChange*fullImplementation[i],
          
          ssbRRnew := ssbRR^(.data[[paste0("Yr_", i, "_x250mLChange")]]), 
          
          diabetesAdjustedProb := (diab1YrProb*ssbRRnew)
          
        ) %>%
        
        rowwise() %>%
        
        mutate("Yr_{i}_Diab" := if_else(condition = diabYN==0, # if you don't currently have diabetes...
                                        true = as.double(Rlab::rbern(n=1, prob=diabetesAdjustedProb)), # use your adjusted prob to draw a new status, some people will develop diabetes
                                        false = diabYN), # otherwise, keep your current diabetes status (which is yes)
               
               # update running diabetes indicator for next year
               diabYN := .data[[paste0("Yr_", i, "_Diab")]]
        ) 
        

      # updates for subsequent year of sim  
      tmp <- tmp %>%  
        
        # age people
        mutate(ageSim = ageSim+1, 
               AGED = if_else(ageSim %in% c(45, 65), "yes", "no")) %>%
        group_by(AGED)
      
      
      split_aging <- group_split(tmp)
      
      # no changes for these peeps (have confirmed the way this is output)
      no_aging <- split_aging[[1]]
      
      
      
      # for these ones, we want to draw new values for their risk ratios and incidence rates
      aged <- split_aging[[2]] %>%
        
        # drop the old values from their younger lives
        select(-incidence, -incidenceLL, -incidenceUL) %>%
        
        # give them a new age range to merge on with
        mutate(incAgeRange = case_when(ageSim>=18 & ageSim<45 ~ 1, 
                                       ageSim>=45 & ageSim<65 ~ 2, 
                                       ageSim>=65 ~ 3, # some people DO age up in this situation
                                       TRUE ~ as.double(NA))) %>%
        
        
        # diabetes incidence rates
        left_join(diabIncidence %>% select(-minInterval, -maxInterval), by="incAgeRange") %>%
        
        # for each person, re-draw their incidence rate
        rowwise() %>%
        
        mutate(diab1YrInc = qunif(p = percentileDiabThings, 
                                  min = incidenceLL, 
                                  max = incidenceUL), 
               diab1YrProb = (1-exp(-(diab1YrInc/1000)*1)))
      
      #print(paste0(nrow(aged), " people aged this round"))
      
      # they all got back together again
      tmp <- bind_rows(no_aging, aged)
      
      stopifnot(nrow(tmp) == nrow(tidy_simulation))
      

      # bind with kids and excluded folks for appropriate re-weighting
      means <- bind_rows(tmp, nhanes_children_over65, nhanes_adults_exclude)
      
      nhanesDesign <- 
        svydesign(id =~psu, 
                  strata = ~stratum, 
                  weight = ~wtdrd1, 
                  data = means, 
                  nest=TRUE)
      
      # take means within the analytic sample subpop only
      subpop_design <-subset(nhanesDesign, analyticSampleIndicator==1)
      
      overallAvg <- svyby(formula = ~ diabYN, 
                          by = ~analyticSampleIndicator, 
                          FUN = svymean, 
                          design=subpop_design, 
                          na.rm = TRUE, 
                          keep.var = FALSE)
      
      # take that year's diabetes y/n variable
      averages <- svyby(formula = ~ diabYN, 
                        by = ~young, 
                        FUN = svymean, 
                        design=subpop_design, 
                        na.rm = TRUE, 
                        keep.var = FALSE)
      
      yearlyMeans <- yearlyMeans %>%
        add_row(year = i, data=list(as_tibble(bind_rows(overallAvg, averages))))

      
    } # end of for/years sim
    
    diabMeans_ageSpecific <- diabMeans_ageSpecific %>%
      add_row(iteration = iteration, means=list(yearlyMeans))
    
    
  }
  
  
  

  
  validationData <- diabMeans_ageSpecific %>% 
    unnest(means) %>%
    unnest(data) %>%
    mutate(cohortLevel = case_when(young==0 ~ "45-64 at baseline", 
                                   young==1 ~ "18-44 at baseline", 
                                   is.na(young) ~ "All adults 18-64 at baseline")) %>%
    group_by(year, cohortLevel) %>%
    summarise(prev = median(statistic), 
              prevLL=quantile(statistic, probs=0.025), 
              prevUL=quantile(statistic, probs=0.975),
              .groups="drop") 
  
  saveRDS(validationData, file = here("analysis", "validation", "simOutput_validation_2009irOnly.RDS"))
  
  
  
