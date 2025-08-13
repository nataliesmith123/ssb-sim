# setup ----


young <- c(0,1)
female <- c(0,1)
raceEthn <- c(1,2,3,4,5)
loweduc <- c(0,1)

# Create a dataframe that has one row for each cohort combination   
cohortsOrig <- expand.grid(young, female, raceEthn, loweduc) %>%
  rownames_to_column(var="cohort") %>%
  mutate(cohort = as.numeric(cohort), 
         x18to64 = 1)
names(cohortsOrig) <- c("cohort", "young", "female", "raceEthn", "loweduc", "x18to64")

(40 == nrow(cohortsOrig))

orig <- haven::read_dta(file=here("analysis", "data", "GFRP NHANES", "foodgroups2_nhanes_0910_nosand.dta"), col_select = c(-eattime))
diab <- haven::read_xpt(file=here("analysis", "data", "nhanes diab", "DIQ_F.XPT"))

allssbs <- c(110103, 110202, 110302, 110002, 112002, 112502)

nonssbs <- c(110101, 110102, 110201, 110301, 110400, 110500,
             110600, 110703, 110701, 110702, 110704, 110803,
             110801, 110802, 110804, 110900, 110001, 112001,
             112100, 112200, 112300, 112400, 112501)

# diet data cleaning ----
nhanes <- orig %>%
  
  mutate(# note that case_when() is vectorized if_else() statements
         # so they are evaluated in order!
         anySSB = case_when(
           # there are 6 beverage codes that are SSBs
           # 1. caloric water
           # 110103 Water, plain or flavored, caloric           
           gfrpfg2 == 110103 ~ 1, 
           
           # 2. coffee, tea caloric WITH added sugar
           # tsug would not be right here -- milk has natural sugar
           # so would pick up incorrect beverages
           # 110202 Coffee/tea, caloric (sugar and/or milk)     
           gfrpfg2 == 110202 & asug > 0 & !is.na(asug) ~ 1,
           
           # 3. traditional "SSBs"
           # if tsug=0, then they don't have sugar, so shouldn't be an SSB
           # all asug is good here, because the food group includes juice drinks
           # 110302 Sugar sweetened beverage, caloric           
           gfrpfg2 == 110302 & tsug > 0 & !is.na(tsug) ~ 1,
           
           # 4. caloric sweetened sports drinks
           # 111002 Sports drinks, caloric                      
           gfrpfg2 == 111002 & tsug > 0 & !is.na(tsug) ~ 1,
           
           # 5. caloric sweetened energy drinks
           # 112002 Energy drinks, caloric                      
           gfrpfg2 == 112002 & tsug > 0 & !is.na(tsug) ~ 1,
           
           # other beverages that have ADDED sweeteners
           # 112502 Other beverages, caloric                    
           gfrpfg2 == 112502 & asug > 0 & !is.na(asug) ~ 1,
           
           # any food that is in the nonSSBs bev list should get a 0
           gfrpfg2 %in% nonssbs ~ 0, 
           
           # any obs remaining that is a food should also get set to 0
           !is.na(gfrpfg) ~ 0,
           
           # anything else should be NA here (this is the demographic info ONLY)
           # demo info is also contained in every row
           TRUE ~ as.numeric(NA)), 
         
         
         # This variable parses out horchata, coffee that people added sugar to, and tea that people added sugar too
         # plus other homemade type beverages
         # it isn't going to be perfect -- e.g., someone could make lemonade themselves
         # but it will remove certain bevs that we KNOW wouldn't be affected
         affectedSSB = case_when(
           
           # anything that is not an SSB should similarly not be an affected SSB
           anySSB==0 ~ 0,
           
           # if its an SSB and comes from a... 
           # store / grocery / supermarket
           # vending machine
           # store - convenience
           # store - no additional info
           anySSB==1 & source %in% c(1, 14, 27, 28) ~ 1, 
           
           # SSB, caloric / flavored caloric water / sports drink / energy drink
           anySSB==1 & gfrpfg2 %in% c(110302, 110103, 112002, 111002) ~ 1, 
           
           # probably affected if it is an SSB and has the following words in its description
           anySSB==1 & str_detect(foodlab, paste(c("PRESWEET", 
                                                   "PRE-SWEET", 
                                                   "FRAPPUCCINO", 
                                                   "COCONUT CREAM, CANNED, SWEETENED", 
                                                   "ICED LATTE, FLAVORED", 
                                                   "LATTE, FLAVORED", 
                                                   "LATTE, NONFAT, FLAVORED", 
                                                   "MOCHA", 
                                                   "LEMONADE", 
                                                   "COCONUT WATER, SWEET"), collapse="|")) ~ 1), 
         
         
         # we don't know if this drink is affected if source is other or don't know
         # leaving this out of my code -- it is redundant and not needed.
         # these bevs will get recoded to affected=0 anyways
         #anySSB==1 & source %in% c(91, 99) ~ as.numeric(NA)), 
         
         # recode those bevs that are SSBs but could not be classified as affected
         affectedSSB = if_else(condition = anySSB==1 & is.na(affectedSSB), 
                               true = 0, 
                               false = affectedSSB), 
         
         anySSBkcal = if_else(condition = anySSB==1, 
                              true = kcal, 
                              false = as.numeric(NA)), 
         
         
         affectedSSBkcal = if_else(condition = affectedSSB==1, 
                                   true = kcal, 
                                   false = as.numeric(NA)), 
         
         affectedSSBgrams = if_else(condition = affectedSSB==1, 
                                    true = gramscon, 
                                    false = as.numeric(NA)), 
         
         affectedSSBtsug = if_else(condition = affectedSSB==1, 
                                   true = tsug, 
                                   false = as.numeric(NA)),
         
         
         affectedSSBasug = if_else(condition = affectedSSB==1, 
                                   true = asug, 
                                   false = as.numeric(NA))
         
  ) 

# diabetes
nhanesDiab <- diab %>%
  select(seqn=SEQN, DIQ010) %>%
  mutate(diabYN = if_else(DIQ010==1, 1, 0))



# demographics ----
nhanesDemo <- nhanes %>%
  
  select(seqn, day, wtdrd1, wtdr2d, stratum, psu, subpop,
         age, gender, ethnicity, lt12, hs, somecoll, ba, povinc,
         weight, height) %>%
  
  filter(day==1) %>%
  
  distinct(seqn, .keep_all=TRUE)


# ssb info ----
nhanesSSB <- nhanes %>%
  
  group_by(seqn, day) %>%
  
  summarise(affectedSSBkcalTotal = sum(affectedSSBkcal, na.rm = TRUE),
            affectedSSBgramsTotal = sum(affectedSSBgrams, na.rm = TRUE), 
            affectedSSBtsugTotal = sum(affectedSSBtsug, na.rm = TRUE), 
            affectedSSBasugTotal = sum(affectedSSBasug, na.rm = TRUE),
            .groups = "drop") %>%
  
  filter(day==1) 




nhanesByPerson <- nhanesDemo %>%
  
  left_join(nhanesSSB, by=c("seqn", "day")) %>%
  
  left_join(nhanesDiab, by="seqn") %>%
  
  mutate(x18to64 = if_else(condition = age >= 18 & age < 65, 
                         true = 1, 
                         false = 0), 
         
         young = if_else(condition = x18to64==1 & age < 45, 
                         true = 1, 
                         false = 0), 
         
         female = if_else(condition = gender==2, 
                          true = 1, 
                          false = 0),
         
         raceEthn = ethnicity, 
         
         # white = if_else(condition = ethnicity==3, 
         #                 true = 1, 
         #                 false = 0), 
         # 
         # black = if_else(condition = ethnicity==4, 
         #                 true = 1, 
         #                 false = 0), 
         # 
         # hispanic = if_else(condition = ethnicity %in% c(1,2), 
         #                    true = 1, 
         #                    false = 0), 
         # 
         # raceEthn = case_when(white==1 ~ 1, 
         #                      black==1 ~ 2, 
         #                      hispanic==1 ~ 3, 
         #                      TRUE ~ as.numeric(NA)), 
         
         loweduc = if_else(condition = lt12==1 | hs==1 | somecoll==1, 
                           true = 1, 
                           false = 0), 
         
         # lowinc = if_else(condition = povinc <= 1.85, 
         #                  true = 1, 
         #                  false = 0), 
         
         heightMeters = height/100, 
         
         consumerSSB = if_else(condition = affectedSSBkcalTotal>0, 
                               true = 1, 
                               false = 0)
         
  ) %>%

  left_join(cohortsOrig, by = c("x18to64", "young", "female", "raceEthn", "loweduc")) %>%

  # targetPopulation: who we are trying to GENERALIZE to: adults aged 18-64 in the 2017-2018 years
  # analyticSampleIndicator: those who are in the targetPopulation who also fulfill requirements for modeling
  mutate(analyticSampleIndicator = if_else(condition = 
                                             subpop==1 & # have good diet data
                                             x18to64==1 & # are between 18 and 64
                                             # !is.na(cohort) & # have all of your relevant demo information -- removed because see note at the bottom of this script. missing data on demos isn't relevant... they can still be modeled. 
                                             !is.na(wtdrd1) & # have a diet weight 
                                             # !is.na(weight) & # have a reported kilograms of weight -- while not relevant to current sim procedures, we will likely want/care about weight at some point and so will wnat complete height/weight data
                                             # !is.na(heightMeters) & # have a reported height -- while not relevant to current sim procedures, we will likely want/care about weight at some point and so will wnat complete height/weight data
                                             !is.na(diabYN), # have a diabetes status
                                           true = 1,
                                           false = 0), 
         
         
         analyticSampleIndicator_cohortHeightWeight = if_else(condition = 
                                                                subpop==1 & # have good diet data
                                                                x18to64==1 & # are between 18 and 64
                                                                !is.na(cohort) & # have all of your relevant demo information -- removed because see note at the bottom of this script. missing data on demos isn't relevant... they can still be modeled. 
                                                                !is.na(wtdrd1) & # have a diet weight 
                                                                !is.na(weight) & # have a reported kilograms of weight -- while not relevant to current sim procedures, we will likely want/care about weight at some point and so will wnat complete height/weight data
                                                                !is.na(heightMeters) & # have a reported height -- while not relevant to current sim procedures, we will likely want/care about weight at some point and so will wnat complete height/weight data
                                                                !is.na(diabYN), # have a diabetes status
                                                              true = 1,
                                                              false = 0))

nhanesByPerson %>% count(x18to64)
nhanesByPerson %>% count(x18to64, analyticSampleIndicator)
nhanesByPerson %>% count(x18to64, analyticSampleIndicator, analyticSampleIndicator_cohortHeightWeight)

source(file = here("analysis", "99_nhanes-checks.R"))

saveRDS(nhanesByPerson, file = here("analysis", "data", "nhanes_0910_cleaned.RDS"))



