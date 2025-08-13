
# uses nhanesByPerson because this is called after the nhanes prep work

# check recodes

# DIQ010 - Doctor told you have diabetes
# Code or Value	Value Description	Count	Cumulative	Skip to Item
# 1	Yes	893	893	
# 2	No	7816	8709	DIQ159
# 3	Borderline	184	8893	DIQ159
# 7	Refused	0	8893	DIQ159
# 9	Don't know	4	8897	DIQ159
# .	Missing	0	8897	

xtabs(~DIQ010 + diabYN, data=nhanesDiab)


# cohort variables
nrow(nhanesByPerson %>% filter(x18to64==1 & age < 18)) # good
nrow(nhanesByPerson %>% filter(x18to64==0 & (age >= 18 & age <= 64))) # good

# this is ok, all those not 18-64 will be removed from analytic sample, so it's alright to have them coded as 0 in the cohort variables (this is true for all proceeding vars too)
xtabs(~x18to64 + young, data=nhanesByPerson, addNA=TRUE) 
nhanesByPerson %>% group_by(x18to64, young) %>% summarise(ageMean=mean(age))

# no missing, but gender/sex used incorrectly
nhanesByPerson %>% count(gender, female)

# no missing
nhanesByPerson %>% count(ethnicity, raceEthn)

# 157 missing on education
nhanesByPerson %>% count(lt12, hs, somecoll, ba, loweduc)

nhanesByPerson %>% group_by(consumerSSB) %>% summarise(mean=mean(affectedSSBkcalTotal))


# missing data checks
nhanesByPerson %>% count(x18to64)
nhanesByPerson %>% count(x18to64, analyticSampleIndicator)
nhanesByPerson %>% count(x18to64, analyticSampleIndicator, analyticSampleIndicator_cohortHeightWeight)

# all folks in the target population are in the analytic sample
# this is because we are NOT restricting on having all the demographic variables, height, and weight
# they just need to have good diet data, a diet weight, and have a baseline diabetes status

# with the more restricted sample based on having a cohort, height, and weight variable in addition to the other requirements, there are 46 people who would be dropped

# where do those 46 people come from?
# library(naniar)
# miss_var_summary(nhanesByPerson)
# tmp <- miss_case_table(nhanesByPerson %>% group_by(x18to64))
# tmp %>% group_by(x18to64) %>% summarise(n_miss = sum(n_miss_in_case))
# 
# gg_miss_var(nhanesByPerson %>% filter(x18to64==1) %>% select(subpop, cohort, young, female, raceEthn, loweduc, weight, height, wtdrd1, diabYN), show_pct = TRUE)
# 
# vis_miss(nhanesByPerson %>% filter(x18to64==1) %>% select(subpop, cohort, young, female, raceEthn, loweduc, weight, height, wtdrd1, diabYN), cluster = TRUE, sort_miss = TRUE, show_perc_col = FALSE)
# 
# nhanesByPerson %>% count(subpop)
# 
# # race and poverty variables are the most commonly missing. 

# codebooks etc: https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017


mice::md.pattern(nhanesByPerson %>% filter(analyticSampleIndicator==1) %>% select(subpop, x18to64, cohort, weight, height, wtdrd1, diabYN))
(46 == 5+2+23+16)
# when we use a more restricted analytic sample indicator, we expect 46 people to NOT make it into that because of missing data
# currently, this is OK, because we don't actually need cohort info, weight, or height to do the modeling. these folks will be in the overall stats, and dropped/appropriate reweighting done when we do stratified results

mice::md.pattern(nhanesByPerson %>% filter(analyticSampleIndicator_cohortHeightWeight==1) %>% select(subpop, x18to64, cohort, weight, height, wtdrd1, diabYN))
# yay, no missing data


library(survey)
nhanesDesign_validate <- 
  svydesign(id =~psu, 
            strata = ~stratum, 
            weight = ~wtdrd1, 
            data = nhanesByPerson, 
            nest=TRUE)

nhanesDesign_validate

svyby(formula = ~diabYN, 
      by = ~x18to64, 
      FUN = svymean, 
      design=nhanesDesign_validate, 
      na.rm = TRUE, 
      keep.var = FALSE)



