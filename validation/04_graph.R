

# pull in averages in nhanes over time
nhanesMeans_overall <- nhanes_means %>%
  unnest(data) %>% 
  rename(prev=mean, se=diabYN) %>%
  mutate(prevLL=prev-(1.96*se), 
         prevUL=prev+(1.96*se), 
         source="NHANES average, \n matched to 2009-2010 cohort current age", 
         source2="actual",
         yearGraph=year)

# output from sim
validationData_overall_2009 <- readRDS(file = here("analysis", "validation", "simOutput_validation_2009irOnly.RDS")) %>%
  mutate(source="NHANES 2009-2010 cohort of 18-64 year olds \n simulated over 10 years using 2009 diabetes incidence rates", 
         source2="sim",
         yearGraph = year + 2009.5)

# combine
validationGraph <- bind_rows(validationData_overall_2009, nhanesMeans_overall) %>%
  filter(yearGraph<2019)


(VALIDATION <- ggplot(data=validationGraph, aes(x=yearGraph, y=prev, colour=source)) + 
    geom_line() +
    geom_errorbar(aes(ymin=prevLL, ymax=prevUL), colour="darkgray", width=0.2) + 
    geom_point(aes(shape=source), size=1.5) + 
    guides(color = guide_legend(override.aes = list(size = 3))) +
    
    theme_bw() + 
    scale_y_continuous(breaks = c(seq(0, 0.22, 0.02)), labels = scales::label_percent())+
    scale_x_continuous(breaks = c(seq(2009, 2022, 1))) + 
    facet_wrap(~cohortLevel) + 
    labs(title = "Model Validation", 
         #subtitle = "Simulated diabetes prevalence compared to age-matched diabetes prevalence over NHANES 2009-10 to NHANES 2017-18", 
         caption = "Simulated data shown is the median and 95% uncertainty interval from 400 model iterations.
       Simulated data starts at 2009.5 to reflect the 2009-10 NHANES starting cohort used.", 
         y="Diabetes Prevalence", 
         x = "Year") + 
    theme(legend.position = "top", 
          legend.title = element_blank(), 
          #axis.text = element_text(size=14), 
          strip.text = element_text(face="bold", size=10), 
          axis.text.x = element_text(angle=45, vjust=0.8), 
          #legend.text = element_text(size=14), 
          panel.spacing.x = unit(0.6, "cm"), 
          axis.ticks.length.x = unit(0.25, "cm"), 
          panel.grid.minor = element_blank(),
          #axis.title = element_text(size=14),
          #plot.title = element_text(size=20), 
          #plot.subtitle = element_text(size=14),
          #plot.caption = element_text(size=12),
          legend.box.background = element_rect(colour="black", linewidth = 1))) 


ggsave(filename=here("analysis", "output", "figs", "nhanesCohortValidation.png"), 
       plot=VALIDATION, 
       device="png", 
       width=8, 
       height=5, 
       units="in")  




# percent differences
(tmp <- validationGraph %>%
  mutate(across(.cols = c(starts_with("prev")), 
                .fns = ~.x*100)) %>%
  pivot_wider(id_cols = c("cohortLevel", "yearGraph"), 
              values_from = c("prev", "prevLL", "prevUL"), 
              names_from = "source2") %>%
  filter(!is.na(prev_actual)) %>%
  mutate(error = (prev_sim)-(prev_actual), 
         sqError = error*error, 
         pct_diff = (error/prev_actual)*100, 
         within_bounds = if_else(prev_sim < prevUL_actual & prev_sim > prevLL_actual, 
                                 "in bounds", "")) %>%
  group_by(cohortLevel) %>%
  mutate(rmse = sqrt(mean(sqError)), 
         mae = mean(abs(error)), 
         across(.cols = c(starts_with("prev"), "error", "sqError", "rmse", "mae", "pct_diff"), 
                .fns = ~ signif(.x, digits=2))) %>%
  select(cohortLevel, year=yearGraph, prev_sim, prevLL_sim, prevUL_sim, prev_actual, prevLL_actual, prevUL_actual, error, sqError, pct_diff, within_bounds, rmse, mae) %>%
  arrange(desc(cohortLevel), year)
)

library(openxlsx)
validation <- createWorkbook()
addWorksheet(validation, "Sheet 1")
writeData(validation, sheet="Sheet 1", x = tmp)
saveWorkbook(validation, file = here("analysis", "validation", "Validation Output.xlsx"), overwrite = TRUE)



