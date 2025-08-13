
diabIncidence <- readr::read_csv(here("analysis", "data", "DiabetesAtlas_NationalData_incidence_3yr_2000-2023.csv"), skip = 2)

diabIncClean <- diabIncidence %>%
  janitor::clean_names() %>%
  select(-x11) %>%
  filter(!str_detect(year, "US Diabetes Surveillance System")) %>%
  pivot_longer(cols=c(-year),
               names_to = "variable", 
               values_to = "number") %>%
  mutate(minInterval = case_when(str_detect(variable, "x18_44") ~ 18,
                                 str_detect(variable, "x45_64") ~ 45, 
                                 str_detect(variable, "x65") ~ 65), 
         
         # make the max one year higher than it actually is to catch the people who are, 
         # for example, 44.6 years old. 
         # use ( vs. [ to account for this in the code (< vs. <=)
         maxInterval = case_when(str_detect(variable, "x18_44") ~ 45,
                                 str_detect(variable, "x45_64") ~ 65, 
                                 str_detect(variable, "x65") ~ 100), 
         
         value = case_when(str_detect(variable, "rate_per_1000") ~ "incidence", 
                           str_detect(variable, "lower_limit") ~ "incidenceLL", 
                           str_detect(variable, "upper_limit") ~ "incidenceUL"), 
         
         incAgeRange = case_when(minInterval==18 & maxInterval==45 ~ 1, 
                                 minInterval==45 & maxInterval==65 ~ 2, 
                                 minInterval==65 ~ 3, 
                                 TRUE ~ as.double(NA)),) %>%
  select(-variable) %>%
  pivot_wider(id_cols = c("year", "incAgeRange", "minInterval", "maxInterval"), 
              names_from = "value", 
              values_from = "number")

saveRDS(diabIncClean, file = here("analysis", "data", "diabIncidenceClean.RDS"))


