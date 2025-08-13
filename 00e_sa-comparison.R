
baseCase <- readRDS(here("analysis", "output", "run data", paste0("incrementalResultsSummarizedAcrossIterations_baseCase.rds"))) %>% mutate(model="base")
rr <- readRDS(here("analysis", "output", "run data", paste0("incrementalResultsSummarizedAcrossIterations_SA_ssbRR.rds"))) %>% mutate(model = "rr")
policy <- readRDS(here("analysis", "output", "run data", paste0("incrementalResultsSummarizedAcrossIterations_SA_policyEffects.rds"))) %>% mutate(model="effects")



tmp <- rbind(baseCase, rr, policy) %>% 
  filter(cohort=="Overall" & outcome=="Diab" & svy=="mean" & year==10) %>%
  mutate(across(.cols = c(median, q25, q975), 
                .fns = ~ scales::label_number(accuracy=0.01)(.x*100), # multiply by 100 to get into percentage points 
                 
                .names = "nice_{.col}"), 
         display_diff = paste0(nice_median,
                        "\n (",
                        nice_q25, ", ", nice_q975, ")")
         ) %>%
  pivot_wider(id_cols = "policy", 
              names_from="model", 
              values_from = "display_diff")



(ft <- tmp %>%
    flextable() %>%
    width(j=c(1:4), 1.5) %>%
    set_header_labels(values = list(policy="Policy Scenario", 
                                    base = "Base Case", 
                                    rr = "SA: SSB-diabetes risk ratio", 
                                    effects = "SA: alternative policy effects"))
  
)


read_docx(path = here("analysis", "output", "template.docx")) %>% 
  body_add_flextable(ft, align="left") %>%
  print(target=here("analysis", "output", paste0("TableA5_SA_comparison.docx")))

