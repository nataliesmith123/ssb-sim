# NHANES base data / population ----
nhanes <- readRDS(file = here("analysis", "data", "nhanes_1718_cleaned.RDS")) 

# Diabetes age-specific incidence rates ----
diabIncidence <- readRDS(file = here("analysis", "data", "diabIncidenceClean.RDS")) %>% filter(year==2017)
#riskRatios <- readRDS(file=("data/rrModeling.RDS"))


# SSB on diabetes RR ----
  # this RR is from the SR and was within the 9 studies that did NOT adjust for BMI (intl and US studies)
  ssbOnDiabRR_input = 1.19
  ssbOnDiabRRLL_input = 1.12 
  ssbOnDiabRRUL_input = 1.27
  # these will be logged and the bounds used to calculate an SE, to simulate a lognormal distribution for RRs


# Merge incidence and RR data by age ranges ----
# Existing variables in nhanes dataset: 
# weight (weight in kg)
# height (height in m)
# consumerSSB (yes/no do they consume SSBs)
# affectedSSBkcalTotal (calories of potentially affected SSBs consumed)
# affectedSSBgramsTotal (grams of potentially affected SSBs consumed)
# diabYN (1=ever told has diabetes, 0=does not)

# counts should be: 
# > nhanes %>% count(x18to64, analyticSampleIndicator, analyticSampleIndicator_cohortHeightWeight)
# # A tibble: 3 × 4
# x18to64 analyticSampleIndicator analyticSampleIndicator_cohortHeightWeight     n
# <dbl>                   <dbl>                                      <dbl> <int>
# 1       0                0                                          0  3879
# 2       1                1                                          0    46
# 3       1                1                                          1  3715
# kids AND adults over 65
nhanes_children_over65 <- nhanes %>% filter(x18to64==0)
stopifnot(nrow(nhanes_children_over65)==3879)

# people in target pop but not in the analytic sample (expected 0)
nhanes_adults_exclude <- nhanes %>% filter(x18to64==1 & analyticSampleIndicator==0)
stopifnot(nrow(nhanes_adults_exclude)==0) # in this situation! in the more restricted analytic, it should be equal to 46

nhanes_adults_include <- nhanes %>% filter(x18to64==1 & analyticSampleIndicator==1) %>%
  mutate(ageSim = age, 
         
         # generated to match with the datasets that contain info used in modeling
         incAgeRange = case_when(age>=18 & age<45 ~ 1, 
                                 age>=45 & age<65 ~ 2, 
                                 age>=65 ~ 3, # should never be used in the initial merge (will be used as people age)
                                 TRUE ~ as.double(NA)), # should never be used
         
         # redundant but takes input and puts into a column that we will use to take draws from lognormal distribution
         ssbOnDiabRR = ssbOnDiabRR_input,
         ssbOnDiabRRLL = ssbOnDiabRRLL_input, 
         ssbOnDiabRRUL = ssbOnDiabRRUL_input, 
         
         lnSsbOnDiabRR = log(ssbOnDiabRR), 
         SElnSsbOnDiabRR = (log(ssbOnDiabRRUL) - log(ssbOnDiabRRLL))/(2*1.96)
         
  ) %>%
  
  # diabetes incidence rates
  left_join(diabIncidence %>% select(-minInterval, -maxInterval), by="incAgeRange")


stopifnot(nrow(nhanes) == nrow(nhanes_children_over65) + nrow(nhanes_adults_include) + nrow(nhanes_adults_exclude))

# Price per ounce ----
  # Leider 2019, overall mean and SD
  
  sodaPPOz <- 0.034
  sodaFreq <- 0.54
  
  sportsPPOz <- 0.048
  sportsFreq <- 0.117
  
  energyPPOz <- 0.199
  energyFreq <- 0.028
  
  readyPPOz <- 0.078
  readyFreq <- 0.082
  
  juicePPOz <- 0.052
  juiceFreq <- 0.233
  
  totalFreq <- 1
  
  stopifnot(sodaFreq + sportsFreq + energyFreq + readyFreq + juiceFreq == totalFreq)


# QALY decrement / disutility of diabetes ----
  # From Sullivan et al. 2006
  qalyDecrement = 0.0351
  qalyDecrement_var = (sqrt(2778)*0.0001)^2
  # want the SD not the SE, because we are modeling the variability among individuals (SD) not the variability in the parameter (SE)
  
  qalyShape1 = qalyDecrement*((qalyDecrement*(1-qalyDecrement)/qalyDecrement_var)-1)
  qalyShape2 = (1-qalyDecrement)*((qalyDecrement*(1-qalyDecrement)/qalyDecrement_var)-1)



# Compensation factor ----

  comp_min = 0.63
  comp_max = 1.00
  
  
# Tax pass through ----
  
  pt_min = 0.66
  pt_max = 0.98
  
  
# Tax elasticity
  
  elasticity = 1.59


