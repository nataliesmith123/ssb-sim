## (FX) Summarize simulated output ----
SURVEY_ANALYSIS <- function(policySpecificOutputDataset){
  
  # set up survey design
  nhanesDesign <- 
    svydesign(id =~psu, 
              strata = ~stratum, 
              weight = ~wtdrd1, 
              data = policySpecificOutputDataset, 
              nest=TRUE)
  
  nhanesDesign
  
  ### Survey means ----
  varsToAnalyze = as.formula("~ 
      heightMeters + 
      weight + 
      compensationFactor + 
      pricePerOunce + 
      # elasticityAverage + # have to take out now because it's done in the specific tax datasets
      # passThrough + 
      consumerSSB + 
      affectedSSBkcalTotal + 
      affectedSSBgramsTotal + 
      diabYN + 
      pctChangeSSBs + 
      changeTEI + 
      totalChangeWt + 
      gramsChange + 
      ozChange + 
      afterPolicyOz + 
      x250mL_servings + 
      x250mLChange + 
      x250mlChange_alt + 
      Yr_0_x250mLChange + 
      Yr_1_x250mLChange + 
      Yr_2_x250mLChange + 
      Yr_3_x250mLChange + 
      Yr_4_x250mLChange + 
      Yr_5_x250mLChange + 
      Yr_6_x250mLChange + 
      Yr_7_x250mLChange + 
      Yr_8_x250mLChange + 
      Yr_9_x250mLChange + 
      Yr_10_x250mLChange +
      diab1YrProb + 
      diab1YrProb_START + 
      diabetesAdjustedProb + 
      ssbRR + 
      ssbRRnew + 
      Yr_0_Diab + 
      Yr_1_Diab + 
      Yr_2_Diab + 
      Yr_3_Diab + 
      Yr_4_Diab + 
      Yr_5_Diab + 
      Yr_6_Diab + 
      Yr_7_Diab + 
      Yr_8_Diab + 
      Yr_9_Diab + 
      Yr_10_Diab + 
      Yr_1_DiabDisutility + 
      Yr_2_DiabDisutility + 
      Yr_3_DiabDisutility + 
      Yr_4_DiabDisutility + 
      Yr_5_DiabDisutility + 
      Yr_6_DiabDisutility + 
      Yr_7_DiabDisutility + 
      Yr_8_DiabDisutility + 
      Yr_9_DiabDisutility + 
      Yr_10_DiabDisutility")
  
  
  # for each of these...
  # analyze the list of variables above
  # make sure to do this BY analytic sample so that the included people are appropriately reweighted
  # take the mean of each variable (good for dichotomous and continuous; we have no categorical)
  # use the design specified above
  # add a cohort identifier
  # keep only the output for those in the analytic sample (those not in the analytic sample will have all 0/NA for the output)
  total <- svyby(formula = varsToAnalyze, 
                 by = ~analyticSampleIndicator, 
                 FUN = svymean, 
                 design=nhanesDesign, 
                 na.rm = TRUE, 
                 keep.var = FALSE) %>%
    mutate(cohort="all") %>%
    filter(analyticSampleIndicator==1)
  
  
  young <- svyby(formula = varsToAnalyze, 
                 by = ~analyticSampleIndicator + young, 
                 FUN = svymean, 
                 design=nhanesDesign, 
                 na.rm = TRUE, 
                 keep.var = FALSE) %>%
    mutate(cohort="age") %>%
    filter(analyticSampleIndicator==1)
  
  female <- svyby(formula = varsToAnalyze, 
                  by = ~analyticSampleIndicator + female, 
                  FUN = svymean, 
                  design=nhanesDesign, 
                  na.rm = TRUE, 
                  keep.var = FALSE) %>%
    mutate(cohort="gender") %>%
    filter(analyticSampleIndicator==1)
  
  raceEthn <- svyby(formula = varsToAnalyze, 
                    by = ~analyticSampleIndicator + labelled::to_character(raceEthn), 
                    FUN = svymean, 
                    design=nhanesDesign, 
                    na.rm = TRUE, 
                    keep.var = FALSE) %>%
    mutate(cohort="raceEthn") %>%
    rename(raceEthn=`labelled::to_character(raceEthn)`) %>%
    filter(analyticSampleIndicator==1)
  
  loweduc <- svyby(formula = varsToAnalyze, 
                   by = ~analyticSampleIndicator + loweduc, 
                   FUN = svymean, 
                   design=nhanesDesign, 
                   na.rm = TRUE, 
                   keep.var = FALSE) %>%
    mutate(cohort="educ") %>%
    filter(analyticSampleIndicator==1)
  
  
  ### Survey sums ----
  varsToSum = as.formula(paste0("~ ", "wtdrd1 + ", 
                                paste0("Yr_", 0:10, "_Diab", collapse="+"), 
                                "+", 
                                paste0("Yr_", 1:10, "_DiabDisutility", collapse="+")))
  
  # this process is the same as above, but takes the SUMS of variables instead of the means
  # allows us to track TOTAL people with diabetes and TOTAL disutility
  # same overall analytic approach, add a cohort identifier, and keep just output from analytic sample
  sumTotal <- svyby(formula = varsToSum, 
                    by = ~analyticSampleIndicator, 
                    FUN = svytotal, 
                    design=nhanesDesign, 
                    na.rm = TRUE, 
                    keep.var = FALSE) %>%
    mutate(cohort="all") %>%
    filter(analyticSampleIndicator==1)
  
  sumYoung <- svyby(formula = varsToSum, 
                    by = ~analyticSampleIndicator + young, 
                    FUN = svytotal, 
                    design=nhanesDesign, 
                    na.rm = TRUE, 
                    keep.var = FALSE) %>%
    mutate(cohort="age") %>%
    filter(analyticSampleIndicator==1)
  
  
  sumFemale <- svyby(formula = varsToSum, 
                     by = ~analyticSampleIndicator + female, 
                     FUN = svytotal, 
                     design=nhanesDesign, 
                     na.rm = TRUE, 
                     keep.var = FALSE) %>%
    mutate(cohort="gender") %>%
    filter(analyticSampleIndicator==1)
  
  
  sumRaceEthn <- svyby(formula = varsToSum, 
                       by = ~analyticSampleIndicator + labelled::to_character(raceEthn), 
                       FUN = svytotal, 
                       design=nhanesDesign, 
                       na.rm = TRUE, 
                       keep.var = FALSE) %>%
    mutate(cohort="raceEthn") %>%
    rename(raceEthn=`labelled::to_character(raceEthn)`) %>%
    filter(analyticSampleIndicator==1)
  
  
  sumEduc <- svyby(formula = varsToSum, 
                   by = ~analyticSampleIndicator + loweduc, 
                   FUN = svytotal, 
                   design=nhanesDesign, 
                   na.rm = TRUE, 
                   keep.var = FALSE) %>%
    mutate(cohort="educ") %>%
    filter(analyticSampleIndicator==1)
  
  
  
  # put everything together in a big output dataset
  means <- bind_rows(total, young, female, raceEthn, loweduc)
  sums <- bind_rows(sumTotal, sumYoung, sumFemale, sumRaceEthn, sumEduc)
  outputTmp <- bind_rows(means %>% mutate(svy="mean"), 
                         sums %>% mutate(svy="sums"))
  
  return(outputTmp)
  
}