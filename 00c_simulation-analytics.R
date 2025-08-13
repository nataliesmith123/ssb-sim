
###############################################################################
# ANALYTIC SETUP
###############################################################################

# need to have nIterations and simName specified from the 00_run-model.R file
# but do not have to run the entire simulation beforehand - can use saved results

library(flextable)
library(officer)

beautifulColors <- ggsci::pal_d3()(5)
beautifulColors <- beautifulColors[-4]
beautifulColors3  <- beautifulColors[-1]

digitsRounding <- 4
digitsRoundingBigNum <- 0

set_flextable_defaults(
  font.family = "Times", 
  padding.top = 2, 
  padding.bottom = 2
)


# Pull results in and organize ----
simResults <- NULL

for (iteration in c(1:nIterations)){
  
  tmp <- readRDS(file=here("analysis", "output", "iterations", paste0("iter", iteration, "_", simName, ".RDS")))
  
  simResults <- rbind(simResults, tmp)
  
  rm(tmp)
}


# policies are separate rows
# iterations are separate rows
# pivot longer for those results that start with "statistic." (all of them)

longAndTidyResults <- simResults %>%
  
  unnest(results) %>%
  
  pivot_longer(cols=c(starts_with("statistic.")),
               names_to="variable",
               values_to="number") %>%
  
  mutate(variable = str_remove_all(variable, "statistic."), 
         cohortLevel = case_when(cohort=="all" ~ "All",
                                 cohort=="age" & young==1 ~ "18-44",
                                 cohort=="age" & young==0 ~ "45-64",
                                 
                                 cohort=="gender" & female==1 ~ "Female",
                                 cohort=="gender" & female==0 ~ "Male",
                                 
                                 cohort=="raceEthn" ~ raceEthn,
                                 
                                 cohort=="educ" & loweduc==1 ~ "Less than college education",
                                 cohort=="educ" & loweduc==0 ~ "At least college education"),
         cohort = factor(cohort, 
                         levels = c("all", "age", "gender", "raceEthn", "educ"), 
                         labels = c("all" = "Overall", "age" = "Age", "gender" = "Gender", "raceEthn" = "Race/Ethnicity", "educ" = "Education")), 
         policy = factor(abbrev, 
                         levels = c("sq", "noMoreSSBs", "graphicwl", "textwl", "tx2c", "tx2cRepeal"), 
                         labels = c("sq"="Status Quo", 
                                    "noMoreSSBs" = "Full Removal of SSBs", 
                                    "graphicwl" = "Graphic Warning Label", 
                                    "textwl" = "Text Warning Label", 
                                    "tx2c" = "Two Cent Excise Tax", 
                                    "tx2cRepeal" = "Two Cent Excise Tax, repealed after one year")))



# BASELINE CHANGE OUTCOMES - pctChange SSBs, x250mL change, 250mL Change
ssbChange <- longAndTidyResults %>%
  select(abbrev, policy, cohort, cohortLevel, svy, iteration, outcome=variable, number) %>%
  filter(outcome %in% c("affectedSSBkcalTotal", "affectedSSBgramsTotal", "x250mL_servings", "pctChangeSSBs", 
                        "gramsChange", "ozChange", "x250mLChange", "x250mlChange_alt", "compensationFactor") & 
           !is.na(number) & abbrev != "sq") %>%
  group_by(policy, cohort, cohortLevel, outcome) %>%
  summarise(median=median(number), 
            q25=quantile(number, probs=0.025), 
            q975=quantile(number, probs=0.975),
            .groups="drop") 

saveRDS(ssbChange, 
        file = here("analysis", "output", "run data", paste0("ssbChange_", simName, ".rds")))




# 
#   mutate(across(.cols = c(median, q25, q975), 
#                 .fns = ~ scales::label_number(style_positive = "space", big.mark = ",", accuracy = 0.0001)(.x), 
#                 .names = "nice_{.col}"), 
#          
#          display_diff = if_else(q25==q975, 
#                                 true = paste0(nice_median, 
#                                               " (constant across iterations)"),
#                                 false = paste0(nice_median, 
#                                                "\n (", 
#                                                nice_q25, 
#                                                ", ", 
#                                                nice_q975, 
#                                                ")"))
#   )



  






# DIABETES OUTCOMES - OVER TIME
keyOutcomesTidy <- longAndTidyResults %>%
  select(abbrev, policy, cohort, cohortLevel, svy, iteration, variable, number) %>%
  filter(variable %in% c(paste0("Yr_", 0:10, "_Diab"), 
                         paste0("Yr_", 1:10, "_DiabDisutility"))) %>%
  mutate(year = as.numeric(str_extract(variable, "[:digit:]+")), 
         outcome = word(variable, -1, sep="_"))

saveRDS(keyOutcomesTidy, 
        file = here("analysis", "output", "run data", paste0("keyOutcomesTidy_", simName, ".rds")))




  

  # INCREMENTAL RESULTS
  # this is very raw and all of the results might not be useful... just getting to a slightly nicer dataset that is tidied up and 
  # contains the variables that we care about (diabetes and (dis)utilities)
  incrementalResults <- 
    
    # all policies, all iterations
    keyOutcomesTidy %>% 
  
    # join on status quo data to calculate incremental results
    left_join((keyOutcomesTidy %>% 
                 filter(abbrev=="sq") %>%
                 select(-policy)), 
               
              by=c("cohort", "cohortLevel", "svy", "iteration", "variable", "year", "outcome"), 
              
              # suffix to clarify which are the policy numbers and which are the status quo numbers
              suffix=c("_policy", "_sq"), 
              
              # allows status quo data to be joined to the multiple policies in the first dataset
              multiple="all") %>%
    
    # create the difference!
    mutate(diff = number_policy - number_sq, 
           diff = if_else(svy=="sums" & outcome=="Diab", diff*-1, diff), # invert life years to be a positive/gain
           pct_change = diff/number_sq) # divide the difference by the sq to get the percent change in prev




  # MEDIAN + UI's: Diabetes prevalence, cases, and qol results over time, overall and by cohort
  # these are still broken out over time, so this information is relevant if we want to: 
    # 1. graph results over time
    # 2. show prevalence at 10 years or fewer cases at year 10
  # need to do more work if we want to show cumulative numbers. 
  incrementalResultsSummarizedAcrossIterations <- incrementalResults %>%
    
    filter(policy != "Status Quo") %>%
    
    group_by(policy, cohort, cohortLevel, year, outcome, svy) %>%
    
    summarise(median=median(diff), 
              q25=quantile(diff, probs=0.025), 
              q975=quantile(diff, probs=0.975), 
              median_pct = median(pct_change), 
              q25_pct=quantile(pct_change, probs=0.025), 
              q975_pct=quantile(pct_change, probs=0.975), 
              
              .groups="drop") 
    
  saveRDS(incrementalResultsSummarizedAcrossIterations, 
          file = here("analysis", "output", "run data", paste0("incrementalResultsSummarizedAcrossIterations_", simName, ".rds")))
  

    # CUMULATIVE INCREMENTAL RESULTS
    cumulativeIncrementalResults <- incrementalResults %>%
      
      filter(svy=="sums" & policy!="Status Quo") %>%
      
      # for each policy, cohort level, outcome, and model iteration, sum the incremental change in the outcome of interest
      # this is done because we modeled yearly incremental differences in QOL because of diabetes, and have a yearly tracker for diabetes prevalence
      group_by(policy, cohort, cohortLevel, outcome, iteration) %>%
      summarise(cumulativeValue = sum(diff), 
                .groups = "drop") %>%
      
      # then, across the iterations, get the median and UI of those cumulative sums
      group_by(policy, cohort, cohortLevel, outcome) %>%
      summarise(median=median(cumulativeValue), 
                q25 = quantile(cumulativeValue, probs=0.025), 
                q975 = quantile(cumulativeValue, probs=0.975), 
                .groups="drop") 
      
    saveRDS(cumulativeIncrementalResults, 
            file = here("analysis", "output", "run data", paste0("cumulativeIncrementalResults_", simName, ".rds")))
    
  
  




# table should include: 
# prevalence change
# diabetes cases averted
# diabetes free years
# total utility gained




# incrementalResultsSummarizedAcrossIterations_display <- incrementalResultsSummarizedAcrossIterations %>%
#   
#   mutate(across(.cols = c(median_diff, q25_diff, q975_diff), 
#                 .fns = ~ if_else(svy == "mean", 
#                                  scales::label_number(accuracy = 0.0001)(.x), 
#                                  scales::label_comma(style_positive = "space")(.x)), 
#                 .names = "nice_{.col}"), 
#          
#          display_diff = if_else(q25_diff==q975_diff, 
#                                 true = paste0(nice_median_diff, 
#                                               "(constant across iterations)"),
#                                 false = paste0(nice_median_diff, 
#                                                "\n (", 
#                                                nice_q25_diff, 
#                                                ", ", 
#                                                nice_q975_diff, 
#                                                ")")),
#          cohortLevel = str_wrap(cohortLevel, width=10), 
#          policyFig = str_wrap(policy, width=10)) 
# 
# 
# # and create a nice display for those
# mutate(across(.cols = c(median_cumulativeValue, q25_cumulativeValue, q975_cumulativeValue), 
#               .fns = ~ scales::label_comma(style_positive = "space")(.x), 
#               .names = "nice_{.col}"), 
#        
#        display_diff = if_else(q25_cumulativeValue==q975_cumulativeValue, 
#                               true = paste0(nice_median_cumulativeValue, 
#                                             "(constant across iterations)"),
#                               false = paste0(nice_median_cumulativeValue, 
#                                              "\n (", 
#                                              nice_q25_cumulativeValue, 
#                                              ", ", 
#                                              nice_q975_cumulativeValue, 
#                                              ")"))
# )



# OVERALL RESULTS TABLE ----
allOutcomes <- 
    bind_rows(
      
      ssbChange %>%
        filter(cohort=="Overall") %>% mutate(data="ssbChange") %>%
        filter(outcome %in% c("pctChangeSSBs", "x250mLChange")) %>%
        mutate(across(.cols = c(median, q25, q975), 
                      .fns = ~ case_when(outcome == "pctChangeSSBs" ~ scales::label_percent()(signif(.x, digits=2)),
                                         outcome == "x250mLChange" ~ scales::label_number()(signif(.x, digits=2))),
                      .names = "nice_{.col}") ), 
      
      incrementalResultsSummarizedAcrossIterations %>% 
        filter(cohort=="Overall" & year==10 & outcome=="Diab") %>% 
        mutate(data="summary", 
               across(.cols = c(median, q25, q975), 
                      .fns = ~ case_when(outcome == "Diab" & svy=="mean" ~ scales::label_number(accuracy=0.01)(.x*100), # multiply by 100 to get into percentage points 
                                         #outcome == "Diab" & svy=="mean" ~ scales::label_percent(accuracy=0.01)(.x), 
                                         outcome == "Diab" & svy=="sums" ~ scales::label_number(big.mark=",")(round(signif(.x/1000, digits=3), digits=0))
                      ), 
                      .names = "nice_{.col}") ), 
      
      # pct change (instead of percentage POINT)
      incrementalResultsSummarizedAcrossIterations %>% 
        filter(cohort=="Overall" & year==10 & outcome=="Diab" & svy=="mean") %>% 
        select(-median, -q25, -q975) %>%
        rename(median=median_pct, q25=q25_pct, q975=q975_pct) %>%
        mutate(data="summary pct", 
               across(.cols = c(median, q25, q975), 
                      .fns = ~ case_when(outcome == "Diab" & svy=="mean" ~ scales::label_percent(accuracy=0.01)(.x), 
                      ), 
                      .names = "nice_{.col}") ),
      
      cumulativeIncrementalResults %>%
        filter(cohort=="Overall") %>% 
      mutate(data="cumulative", 
             across(.cols = c(median, q25, q975), 
                    .fns = ~ case_when(outcome == "Diab" ~ scales::label_number(big.mark=",")(round(signif(.x/1000, digits=2), digits=0)), 
                                       outcome == "DiabDisutility"  ~ scales::label_number(big.mark=",")(round(signif(.x/1000, digits=2), digits=0))
                    ), 
                    .names = "nice_{.col}") )
    ) %>%
      
      mutate(results = case_when(svy=="mean" & data=="summary" ~ "Percentage point change in diabetes prevalence at 10 years", 
                                 svy=="sums" & data=="summary" ~ "Total cases of diabetes averted (in thousands)", 
                                 
                                 svy=="mean" & data=="summary pct" ~ "Percentage change in diabetes prevalence at 10 years",
                                 
                                 data=="cumulative" & outcome=="Diab" ~ "Total diabetes-free life years gained over 10 years (in thousands)", 
                                 data=="cumulative" & outcome=="DiabDisutility" ~ "Total quality-adjusted life years gained over 10 years (in thousands)", 
                                 
                                 outcome=="pctChangeSSBs" ~ "Percent change in SSBs", 
                                 outcome=="x250mLChange" ~ "Change in 250mL SSB servings consumed"),
             
             
             # across(.cols = c(median, q25, q975), 
             #        .fns = ~ case_when(abs(.x) <= 1 ~ scales::label_number(accuracy = 0.01)(.x), 
             #                           abs(.x) > 1 & abs(.x) < 1000 ~ scales::label_number(accuracy = 0.1)(.x), 
             #                           abs(.x) > 1000 ~ scales::label_number(big.mark=",")(round(signif(.x, digits=3), digits=0))),
             #        # 
             #        # .fns = ~ if_else(abs(.x) < 1, 
             #        #                  scales::label_number(accuracy = 0.001)(.x), 
             #        #                  scales::label_number(big.mark = ",")(.x)),
             #        .names = "nice_{.col}"), 
             
             display_diff = if_else(q25==q975, 
                                    true = paste0(nice_median,
                                                  " (constant)"),
                                    false = paste0(nice_median,
                                                   "\n (",
                                                   nice_q25,
                                                   ", ",
                                                   nice_q975,
                                                   ")"))
      )
      
      
      




(ft <- allOutcomes %>%
    pivot_wider(id_cols = "results", 
                names_from = policy, 
                values_from = display_diff) %>%
    mutate(results = factor(results, levels = c("Percent change in SSBs", 
                                                "Change in 250mL SSB servings consumed", 
                                                "Total cases of diabetes averted (in thousands)", 
                                                "Percentage point change in diabetes prevalence at 10 years", 
                                                "Percentage change in diabetes prevalence at 10 years",
                                                "Total diabetes-free life years gained over 10 years (in thousands)", 
                                                "Total quality-adjusted life years gained over 10 years (in thousands)"))) %>%
    arrange(results) %>%
    flextable() %>%
    width(j=1, width=1.5) %>%
    width(j=2:6, width=1.8) %>%
    set_header_labels(values = list(results="Results"))
  
)


read_docx(path = here("analysis", "output", "template-landscape.docx")) %>% 
  body_add_flextable(ft, align="left") %>%
  body_end_section_landscape() %>%
  print(target=here("analysis", "output", paste0("Table2_", simName, "_EVERYTHING.docx")))

read_docx(path = here("analysis", "output", "template-landscape_1inch-margins.docx")) %>% 
  body_add_flextable(ft %>% 
                       delete_rows(1:2) %>%
                       width(j=2:6, width=1.3), align="left") %>%
  body_end_section_landscape() %>%
  print(target=here("analysis", "output", paste0("Table2_", simName, "_presentation.docx")))



# SUBGROUP RESULTS TABLE ----

(table <- 
   bind_rows(
     incrementalResultsSummarizedAcrossIterations %>% 
       filter(year==10 & outcome=="Diab")  %>%
       mutate(results = if_else(svy=="mean", 
                                "Change in Diabetes Prevalence", 
                                "Incremental Cases of Diabetes at Year 10")), 
     # %>%
     #   select(policy, results, display_policy=display_diff), 
     
     cumulativeIncrementalResults %>%
       mutate(results = case_when(outcome=="Diab" ~ "Cumulative Diabetes-Free Life Years Gained", 
                                  outcome=="DiabDisutility" ~ "Cumulative Disutility Avoided")) 
     # %>%
     #   select(policy, results, display_policy=display_diff)
   )
)





# BAR PLOT, DIABETES CASES AVERTED ----

diabAverted_graph <- incrementalResultsSummarizedAcrossIterations %>% 
  filter(year==10 & outcome=="Diab" & svy=="sums")  %>%
  mutate(results = "Incremental Cases of Diabetes at Year 10") 


(casesAverted_subgroups <- ggplot(data=diabAverted_graph, 
       aes(x=cohortLevel), 
       label=scales::label_number(big.mark=",")(median)) + 
  geom_bar(aes(y=median*-1), stat="identity", width=0.7, 
           colour = "darkgray", 
           fill = "lightgray") + 
  geom_errorbar(aes(ymin=q25*-1, ymax=q975*-1), width=0.2) +
  scale_y_continuous(labels = scales::format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  facet_grid(row = vars(policy), 
             cols = vars(cohort), 
             scales = "free_x", 
             space = "free", 
             switch = "y",
             labeller = label_wrap_gen(width=15)) + 
  scale_x_discrete(labels = scales::label_wrap(10)) +
  ylab("") +
  xlab("") +
  theme_bw() + 
  theme(strip.text = element_text(face="bold", size=12),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0),
        strip.background = element_blank()) + 
  labs(title = "Total Cases of Diabetes Averted at 10 years",
       subtitle = "Overall and By Subgroup, Compared to status quo, among adults aged 18-64 in NHANES 2017-18 cohort",
       caption = paste0("Difference in total diabetes cases at 10 years across ", nIterations, " reps of the model"),
       colour = ""))

ggsave(filename = here("analysis", "output", "figs", paste0("casesAverted_subgroups_", simName, ".png")), 
       plot = casesAverted_subgroups, 
       device="png", 
       width=10, 
       height=7)

saveRDS(casesAverted_subgroups, 
        file = here("analysis", "output", "figs", paste0("casesAverted_subgroups_", simName, ".rds")))


# BAR PLOT, PREVALENCE CHANGE ----

prevChange_graph <- incrementalResultsSummarizedAcrossIterations %>% 
  filter(year==10 & outcome=="Diab" & svy=="mean")


(prevChange_subgroups <- ggplot(data=prevChange_graph, 
       aes(x=cohortLevel), 
       label=scales::label_number(big.mark=",")(median)) + 
  geom_bar(aes(y=median*100), stat="identity", width=0.7, 
           colour = "darkgray", 
           fill = "lightgray") + 
    geom_errorbar(aes(ymin=q25*100, ymax=q975*100), width=0.2, alpha = 0.75) +
  scale_y_continuous(labels = scales::format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  facet_grid(row = vars(policy), 
             cols = vars(cohort), 
             scales = "free_x", 
             space = "free", 
             switch = "y", 
             labeller = label_wrap_gen(width=15)) + 
  scale_x_discrete(labels = scales::label_wrap(10)) +
  ylab("") +
  xlab("") +
  theme_bw() + 
  theme(strip.text = element_text(face="bold", size=12),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0),
        strip.background = element_blank()) + 
  labs(title = "Percentage point change in diabetes prevalence at 10 Years",
       subtitle = "Overall and By Subgroup, compared to status quo, among adults aged 18-64 in NHANES 2017-18 cohort",
       caption = paste0("Percentage point change in diabetes prevalence at 10 years across ", nIterations, " replications of the model"),
       colour = ""))

ggsave(filename = here("analysis", "output", "figs", paste0("prevChange_subgroups_", simName, ".png")), 
       plot = prevChange_subgroups, 
       device="png", 
       width=10, 
       height=7)

saveRDS(prevChange_subgroups, 
        file = here("analysis", "output", "figs", paste0("prevChange_subgroups_", simName, ".rds")))


# LINE GRAPH
lineGraph <- incrementalResults %>%
  filter(policy != "Status Quo" & cohortLevel == "All" & outcome == "Diab" & svy == "sums")

(lineGraphg <- ggplot(data = lineGraph, 
       aes(x = year, y = diff, colour = policy, fill = policy)) + 
  ggdist::stat_lineribbon(.width = c(0.95), alpha = 1/4) + 
  facet_wrap(~policy) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 1,2,3,4,5,6,7,8,9,10)) +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank()) + 
  ylab("Cases of diabetes, relative to status quo") + 
  xlab("Year of simulation") + 
  labs(caption = ""))

ggsave(filename = here("analysis", "output", "figs", paste0("DiabCases-Overall-Policy_", simName, ".png")), 
       plot=lineGraphg,
       device="png")

saveRDS(lineGraphg, 
        file = here("analysis", "output", "figs", paste0("lineGraphg_", simName, ".rds")))



# Not differences, but rather just the raw values
lineGraph2 <- incrementalResults %>%
  filter(cohortLevel == "All" & outcome == "Diab" & svy == "mean")

(lineGraph2g <- ggplot(data = lineGraph2, 
       aes(x = year, y = number_policy, colour = policy, fill = policy)) + 
  # geom_line() + 
  ggdist::stat_lineribbon(.width = c(0), alpha = 1) +
  # facet_wrap(~policy) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 1,2,3,4,5,6,7,8,9,10)) +
  theme(panel.grid.minor = element_blank()) + 
  ylab("Prevalence of diabetes") + 
  xlab("Year of simulation") + 
  labs(caption = "Model is run with 2017-18 NHANES, so 10 years is ~2027-2028 (if we are concerned about specific predictions)"))

ggsave(filename = here("analysis", "output", "figs", paste0("DiabCases2-Overall-Policy_", simName, ".png")), 
       plot = lineGraph2g, 
       device="png")

saveRDS(lineGraph2g, 
        file = here("analysis", "output", "figs", paste0("lineGraph2g_", simName, ".rds")))


