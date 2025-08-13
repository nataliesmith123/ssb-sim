
library(tidyverse)
library(here)
library(survey)
library(haven)
library(flextable)
library(officer)

set_flextable_defaults(
  font.family = "Times", 
  padding.top = 2, 
  padding.bottom = 2
)

nhanes <- readRDS(file = here("analysis", "data", "nhanes_1718_cleaned.RDS")) %>%
  
  # copied from sim code
  mutate(x250mLServings = affectedSSBgramsTotal/250)

nhanes %>% count(x18to64, analyticSampleIndicator)


nhanesDesign <- 
  svydesign(id =~psu, 
            strata = ~stratum, 
            weight = ~wtdrd1, 
            data = nhanes, 
            nest=TRUE)

nhanesDesign


descriptive_stats <- svyby(formula = ~ x18to64 + young + female + labelled::to_character(raceEthn) + loweduc + affectedSSBkcalTotal + x250mLServings + diabYN, 
                  by = ~analyticSampleIndicator, 
                  FUN = svymean, 
                  design=nhanesDesign, 
                  na.rm = TRUE, 
                  keep.var = TRUE) 

descriptive_table <- descriptive_stats %>%
  filter(analyticSampleIndicator==1) %>%
  select(-analyticSampleIndicator, -x18to64, -se.x18to64) %>%
  pivot_longer(cols = everything()) %>%
  mutate(var_length = str_length(name),
         stat = if_else(str_detect(name, "se."), "se", "mean"),
         name = str_remove(name, "se."),
         nameNice = case_when(str_detect(name, "raceEthn") ~ str_sub(name, 33), 
                          TRUE ~ name), 
         valueNice = case_when(stat == "mean" & value < 1 ~ scales::label_number(accuracy=0.01)(value), 
                               stat == "mean" & value > 1 ~ scales::label_number(accuracy=0.1)(value), 
                               stat == "se" ~ scales::label_number(accuracy=0.001)(value))) %>%
  pivot_wider(id_cols = "nameNice", 
              names_from = "stat", 
              values_from = "valueNice") %>%
  mutate(meanSE = paste0(mean, " (", se, ")"), 
         race = str_detect(nameNice, "White|Black|Other|Mexican"))

young <- descriptive_table %>% filter(nameNice=="young") %>% mutate(Variable="18-44 years old")
female <- descriptive_table %>% filter(nameNice=="female") %>% mutate(Variable="Female")
raceEthn <- descriptive_table %>% filter(race==TRUE) %>% mutate(Variable = case_when(nameNice == "Mexican Amer" ~ "Mexican American", 
                                                                                     nameNice == "Non-Hisp Black" ~ "Non-Hispanic Black", 
                                                                                     nameNice == "Non-Hisp White" ~ "Non-Hispanic White", 
                                                                                     nameNice == "Other" ~ "Other", 
                                                                                     nameNice == "Other Hispanic" ~ "Other Hispanic"))
loweduc <- descriptive_table %>% filter(nameNice=="loweduc") %>% mutate(Variable="Less than college education")
kcal <- descriptive_table %>% filter(nameNice=="affectedSSBkcalTotal") %>% mutate(Variable="Total SSB calories")
x250 <- descriptive_table %>% filter(nameNice=="x250mLServings") %>% mutate(Variable="250 mL servings of SSBs")
diab <- descriptive_table %>% filter(nameNice=="diabYN") %>% mutate(Variable="Baseline diabetes")



(ft <- bind_rows(young, 
                 female, 
                 tribble(~Variable, "Race and ethnicity"), 
                 raceEthn, 
                 loweduc, kcal, x250, diab) %>%
    select(Variable, `Mean (SE)` = meanSE) %>%
    flextable() %>%
    width(1, 2) %>%
    width(2, 1) %>%
    padding(i=4:8, padding.left = 10))

read_docx(path = here("analysis", "output", "template-landscape.docx")) %>% 
  # body_add_fpar(fpar(ftext("Developmental costs"))) %>%
  body_add_flextable(ft, align="left") %>%
  print(target=here("analysis", "output", paste0("TableDescriptive", ".docx")))



# serving size breakdown by demo groups --- 

young_strata <- svyby(formula = ~ affectedSSBkcalTotal + x250mLServings + diabYN, 
               by = ~analyticSampleIndicator + young, 
               FUN = svymean, design=nhanesDesign, na.rm = TRUE, keep.var = TRUE) %>%
  filter(analyticSampleIndicator==1) %>%
  select(-analyticSampleIndicator) %>%
  pivot_longer(cols = -young) %>%
  mutate(stat = if_else(str_detect(name, "se."), "se", "mean"),
         name = str_remove(name, "se."),
         nameNice = case_when(name == "affectedSSBkcalTotal" ~ "Total SSB calories", 
                              name == "x250mLServings" ~ "250 mL servings of SSBs", 
                              name == "diabYN" ~ "Baseline diabetes"), 
         valueNice = case_when(stat == "mean" & value < 1 ~ scales::label_number(accuracy=0.01)(value), 
                               stat == "mean" & value > 1 ~ scales::label_number(accuracy=0.1)(value), 
                               stat == "se" ~ scales::label_number(accuracy=0.001)(value))) %>%
  pivot_wider(id_cols = c("young", "nameNice"), 
              names_from = "stat", 
              values_from = "valueNice") %>%
  mutate(meanSE = paste0(mean, " (", se, ")"), 
         Variable = if_else(young==1, "18-44 years old", "45-64 years old")) %>%
  pivot_wider(id_cols = "Variable", 
              names_from = "nameNice", 
              values_from = "meanSE")


female_strata <- svyby(formula = ~ affectedSSBkcalTotal + x250mLServings + diabYN, 
               by = ~analyticSampleIndicator + female, 
               FUN = svymean, design=nhanesDesign, na.rm = TRUE, keep.var = TRUE) %>%
  filter(analyticSampleIndicator==1) %>%
  select(-analyticSampleIndicator) %>%
  pivot_longer(cols = -female) %>%
  mutate(stat = if_else(str_detect(name, "se."), "se", "mean"),
         name = str_remove(name, "se."),
         nameNice = case_when(name == "affectedSSBkcalTotal" ~ "Total SSB calories", 
                              name == "x250mLServings" ~ "250 mL servings of SSBs", 
                              name == "diabYN" ~ "Baseline diabetes"), 
         valueNice = case_when(stat == "mean" & value < 1 ~ scales::label_number(accuracy=0.01)(value), 
                               stat == "mean" & value > 1 ~ scales::label_number(accuracy=0.1)(value), 
                               stat == "se" ~ scales::label_number(accuracy=0.001)(value))) %>%
  pivot_wider(id_cols = c("female", "nameNice"), 
              names_from = "stat", 
              values_from = "valueNice") %>%
  mutate(meanSE = paste0(mean, " (", se, ")"), 
         Variable = if_else(female==1, "Female", "Male")) %>%
  pivot_wider(id_cols = "Variable", 
              names_from = "nameNice", 
              values_from = "meanSE")


raceEthn_strata <- svyby(formula = ~ affectedSSBkcalTotal + x250mLServings + diabYN, 
               by = ~analyticSampleIndicator + labelled::to_character(raceEthn), 
               FUN = svymean, design=nhanesDesign, na.rm = TRUE, keep.var = TRUE) %>%
  filter(analyticSampleIndicator==1) %>%
  select(-analyticSampleIndicator) %>%
  pivot_longer(cols = -"labelled::to_character(raceEthn)") %>%
  mutate(stat = if_else(str_detect(name, "se."), "se", "mean"),
         name = str_remove(name, "se."),
         nameNice = case_when(name == "affectedSSBkcalTotal" ~ "Total SSB calories", 
                              name == "x250mLServings" ~ "250 mL servings of SSBs", 
                              name == "diabYN" ~ "Baseline diabetes"), 
         valueNice = case_when(stat == "mean" & value < 1 ~ scales::label_number(accuracy=0.01)(value), 
                               stat == "mean" & value > 1 ~ scales::label_number(accuracy=0.1)(value), 
                               stat == "se" ~ scales::label_number(accuracy=0.001)(value))) %>%
  pivot_wider(id_cols = c("labelled::to_character(raceEthn)", "nameNice"), 
              names_from = "stat", 
              values_from = "valueNice") %>%
  mutate(meanSE = paste0(mean, " (", se, ")"), 
         Variable = `labelled::to_character(raceEthn)`) %>%
  pivot_wider(id_cols = "Variable", 
              names_from = "nameNice", 
              values_from = "meanSE")


loweduc_strata <- svyby(formula = ~ affectedSSBkcalTotal + x250mLServings + diabYN, 
                         by = ~analyticSampleIndicator + loweduc, 
                         FUN = svymean, design=nhanesDesign, na.rm = TRUE, keep.var = TRUE) %>%
  filter(analyticSampleIndicator==1) %>%
  select(-analyticSampleIndicator) %>%
  pivot_longer(cols = -loweduc) %>%
  mutate(stat = if_else(str_detect(name, "se."), "se", "mean"),
         name = str_remove(name, "se."),
         nameNice = case_when(name == "affectedSSBkcalTotal" ~ "Total SSB calories", 
                              name == "x250mLServings" ~ "250 mL servings of SSBs", 
                              name == "diabYN" ~ "Baseline diabetes"), 
         valueNice = case_when(stat == "mean" & value < 1 ~ scales::label_number(accuracy=0.01)(value), 
                               stat == "mean" & value > 1 ~ scales::label_number(accuracy=0.1)(value), 
                               stat == "se" ~ scales::label_number(accuracy=0.001)(value))) %>%
  pivot_wider(id_cols = c("loweduc", "nameNice"), 
              names_from = "stat", 
              values_from = "valueNice") %>%
  mutate(meanSE = paste0(mean, " (", se, ")"), 
         Variable = if_else(loweduc==1, "Less than college education", "At least college education")) %>%
  pivot_wider(id_cols = "Variable", 
              names_from = "nameNice", 
              values_from = "meanSE")


(ft <- bind_rows(tribble(~Variable, "Age group"), 
                 young_strata , 
                 tribble(~Variable, "Sex"), 
                 female_strata, 
                 tribble(~Variable, "Race and ethnicity"), 
                 raceEthn_strata, 
                 tribble(~Variable, "Education"), 
                 loweduc_strata) %>%
    flextable() %>%
    width(1, 2) %>%
    width(2:4, 1.25) %>%
    padding(i=c(2,3, 5, 6, 8, 9, 10, 11, 12, 14, 15), padding.left = 10))

read_docx(path = here("analysis", "output", "template-landscape.docx")) %>% 
  # body_add_fpar(fpar(ftext("Developmental costs"))) %>%
  body_add_flextable(ft, align="left") %>%
  print(target=here("analysis", "output", paste0("TableDescriptive_InputsByDemos", ".docx")))



