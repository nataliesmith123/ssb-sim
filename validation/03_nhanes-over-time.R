
library(tools)
# loop over the data

library(here)
demo <- list.files(path = here("analysis", "data", "nhanes diab"), pattern = "^DEMO_[A-Z]\\.xpt", all.files = TRUE, full.names = TRUE)
diab <- list.files(path = here("analysis", "data", "nhanes diab"), pattern = "^DIQ_[A-Z]\\.xpt", all.files = TRUE, full.names = TRUE)

minAge <- c(18, 20, 22, 24, 26, 30)
midAge <- c(45, 47, 49, 51, 53, 57)
maxAge <- c(64, 66, 68, 70, 72, 76)

nhanes_year <- c(2009.5, 2011.5, 2013.5, 2015.5, 2017.5, 2022)

# 09/10: 18-64
# 11/12: 20-66
# 13/14: 22-68
# 15/16: 24-70
# 17/18: 26-72
# 21/23: 30-76



nhanes_means <- tibble(year = numeric(), data=list())

for (i in 1:length(demo)){
  
  print(i)
  print(demo[i])
  print(minAge[i])
  print(maxAge[i])
  
  diabCheck <- haven::read_xpt(file=demo[i]) %>%
    left_join(haven::read_xpt(file=diab[i]), by="SEQN") %>%
    mutate(diabYN = if_else(DIQ010==1, 1, 0), 
           subpop = if_else(!is.na(DIQ010) & RIDAGEYR>= minAge[i] & RIDAGEYR<maxAge[i], 1, 0), 
           youngCohort = if_else(subpop==1 & RIDAGEYR < midAge[i], 1, 0))
  
  nrow(demo) - nrow(diab)
  # plus 1 missing from NHANES documentation
  
  diabCheck %>% count(DIQ010)
  diabCheck %>% count(diabYN)

  diabCheck %>% count(subpop)
  
  #means <- diabCheck %>% group_by(subpop, youngCohort) %>% summarise(mean=mean(RIDAGEYR))
  #print(means)
  
  # I'm definitely setting up the survey design right
  # https://wwwn.cdc.gov/nchs/nhanes/tutorials/varianceestimation.aspx
  library(survey)
  nhanesDesign_validate <- 
    svydesign(id =~SDMVPSU, 
              strata = ~SDMVSTRA, 
              weight = ~WTINT2YR, 
              data = diabCheck, 
              nest=TRUE)
  
  nhanesDesign_validate
  subpop_design <-subset(nhanesDesign_validate, subpop==1)
  subpop_design
  
( tmp <- svymean(~diabYN, 
          #by = ~x18to64, 
          #FUN = svymean, 
          design=subpop_design, 
          na.rm = TRUE, 
          keep.var = FALSE))
  print(as_tibble(tmp))
  
  subpop_young <- subset(subpop_design, youngCohort==1)
  ( tmpY <- svymean(~diabYN, 
                  design=subpop_young, 
                  na.rm = TRUE, 
                  keep.var = FALSE))
  print(as_tibble(tmpY))
  
  
  subpop_old <- subset(subpop_design, youngCohort==0)
  ( tmpO <- svymean(~diabYN, 
                    design=subpop_old, 
                    na.rm = TRUE, 
                    keep.var = FALSE))
  print(as_tibble(tmpO))
  
  
  # ( tmp1 <- svyby(~diabYN, 
  #                  by = ~youngCohort, 
  #                  FUN = svymean, 
  #                  design=subpop_design, 
  #                  na.rm = TRUE, 
  #                  keep.var = FALSE))
  # print(as_tibble(tmp1))
  
  nhanes_means <- nhanes_means %>%
    add_row(year = nhanes_year[i], data=list(bind_rows(as_tibble(tmp) %>% mutate(cohortLevel="All adults 18-64 at baseline"), 
                                                       as_tibble(tmpY) %>% mutate(cohortLevel="18-44 at baseline"), 
                                                       as_tibble(tmpO) %>% mutate(cohortLevel="45-64 at baseline"))))
  
# (  tmp2 <- svyby(~diabYN, 
#         by = ~subpop, 
#         FUN = svymean, 
#         design=nhanesDesign_validate, 
#         na.rm = TRUE, 
#         keep.var = FALSE))
#   #print(tmp2)
#   
}

# MEAN AND SE ARE OUTPUT
# SEE HOW IT'S OUTPUT HERE
svymean(~diabYN, 
        #by = ~x18to64, 
        #FUN = svymean, 
        design=subpop_design, 
        na.rm = TRUE, 
        keep.var = FALSE)











  













