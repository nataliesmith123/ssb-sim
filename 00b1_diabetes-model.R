

## (FX) Simulate over time ----
DO_THE_SIM <- function(policySpecificDataset, inputImplementationVector){
  
  print(paste0("Policy: ", policySpecificDataset[1, "policy"]))
  
  # consistent calculations done using the 'pctChangeSSBs' variable for EACH POLICY (via each dataset)
  # note pctChangeSSBs is NEGATIVE to the 'change' variables will always be a negative value of grams, ml, etc. 
  tmp <- policySpecificDataset %>%
    mutate("changeTEI" = (affectedSSBkcalTotal*pctChangeSSBs),
           
           # using relatively conservative hall model heuristic
           "totalChangeWt" = changeTEI*(1/(55*0.453592)), 
           
           # this assumes a density of 1 gram/milliliter
           "x250mL_servings" = affectedSSBgramsTotal/250,
           
           # pct change by compensation
           #"gramsChange" = (affectedSSBgramsTotal*pctChangeSSBs)*compensationFactor,
           "gramsChange" = (affectedSSBgramsTotal*pctChangeSSBs),
           
           # this assumes 1 gram : 1 mL by density (as above)
           # coverts it to 250 mL change
           "x250mLChange" = gramsChange/250,
           
           # mark this as the year 0 change - matters only to the extent that the change CAN go away in the case of repealed policies, etc. 
           "Yr_0_x250mLChange" = x250mLChange,
           
           # an alternative way of calculating just for checking purposes
           "x250mlChange_alt" = (x250mL_servings*pctChangeSSBs),
           
           # change in ounches calculated the same way
           "ozChange" = (ssbOz*pctChangeSSBs), 
           
           # after policy ounces is total + negative oz change (used if we want to collect yearly tax revenue - need oz consumed per year)
           "afterPolicyOz" = ssbOz + ozChange
           
    )
  
  for (i in 1:yearsSim){
    
    set.seed(iteration + i + sim_id)
    
    tmp <- tmp %>%
      mutate(
        
        # NOTE that the implementation vector is a GLOBAL adjustment, which adjusts *each persons* 250mL change by that value
        # this is NOT the same as only 50% of folks getting a policy in year 1 due to slower rollout
        
        "Yr_{i}_x250mLChange" := x250mLChange*inputImplementationVector[i],
        
        # using that year's 250 mL change, adjust the base risk ratio to that power
        # for most 'full implementation' scenarios this is redundant to do yearly, but allows for us to change other things yearly and have it populated in the risk ratio again
        ssbRRnew := ssbRR^(.data[[paste0("Yr_", i, "_x250mLChange")]]), 
        
        # adjust probability by risk ratio!
        diabetesAdjustedProb := (diab1YrProb*ssbRRnew)
        
      ) %>%
      
      rowwise() %>%
      
      # draw a diabetes status for each person
      # this should only happen if they do NOT have diabetes via the overall tracker variable (diabYN)
      # if you don't, then draw a bernoulli random variable using the adjusted probability as the input
      mutate("Yr_{i}_Diab" := if_else(condition = diabYN==0, # if you don't currently have diabetes...
                                      true = as.double(Rlab::rbern(n=1, prob=diabetesAdjustedProb)), # use your adjusted prob to draw a new status, some people will develop diabetes
                                      false = diabYN), # otherwise, keep your current diabetes status (which is yes)
             
             # update running diabetes indicator for next year
             diabYN := .data[[paste0("Yr_", i, "_Diab")]]
      ) %>%
      ungroup() %>%
      
      # track the amount of disutility that year based on the number of people who have diabetes (will be 0 if not)
      # could also do this with diabYN since we just updated it to be the same
      mutate("Yr_{i}_DiabDisutility" := diabetesDisutility*.data[[paste0("Yr_", i, "_Diab")]]
             
      )
    
    #print(summary(tmp$diab1YrProb))
    #print(summary(tmp$ssbRRnew))
    #print(summary(tmp$diabetesAdjustedProb))
    #print(tmp %>% count(diabYN))
    
    # updates for subsequent year of sim  
    tmp <- tmp %>%  
      
      # age people
      mutate(ageSim = ageSim+1, 
             
             # will overwrite last years AGED variable
             AGED = if_else(ageSim %in% c(45, 65), "yes", "no")) %>%
      group_by(AGED)
    
    # split into a list based on the grouping variable in tmp, which is the AGED dichotomous variable
    split_aging <- group_split(tmp)
    
    # no changes for these peeps (have confirmed the way this is output)
    no_aging <- split_aging[[1]]
    # and double check to confirm this contains the NON-AGED people, who should keep their same incidence rate
    stopifnot(all(no_aging$AGED == "no"))
    
    # for these ones, we want to draw new values for their risk ratios and incidence rates
    aged <- split_aging[[2]] %>%
      
      # drop the old values from their younger lives
      select(-incidence, -incidenceLL, -incidenceUL) %>%
      
      # give them a new age range to merge on with
      mutate(incAgeRange = case_when(ageSim>=18 & ageSim<45 ~ 1, 
                                     ageSim>=45 & ageSim<65 ~ 2, 
                                     ageSim>=65 ~ 3, # some people DO age up in this situation
                                     TRUE ~ as.double(NA))) %>%
      
      
      # diabetes incidence rates
      left_join(diabIncidence %>% select(-minInterval, -maxInterval), by="incAgeRange") %>%
      
      # for each person, re-draw their incidence rate
      rowwise() %>%
      
      # the same way using the same percentile, but new lower and upper bounds for diabetes
      mutate(diab1YrInc = qunif(p = percentileDiabThings, 
                                min = incidenceLL, 
                                max = incidenceUL), 
             diab1YrProb = (1-exp(-(diab1YrInc/1000)*1)))
    
    #print(paste0(nrow(aged), " people aged this round"))
    
    # confirm we only added new incidence rates for people who aged
    stopifnot(all(aged$AGED == "yes"))
    
    
    # they all got back together again
    tmp <- bind_rows(no_aging, aged)
    
    # confirm the new dataset has the right number of rows
    stopifnot(nrow(tmp) == nrow(tidy_simulation))
    
    i = i+1
  } # end of for/years sim
  
  return(tmp)
  
}
