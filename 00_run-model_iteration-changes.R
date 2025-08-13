

###############################################################################
# Libraries etc.
###############################################################################
library(tidyverse)
library(here)
library(survey)
library(haven)



yearsSim <- 10


# Run a few different iteration numbers

#iterationValues <- c(75, 150, 300, 600)
iterationValues <- c(1200)
compTimes <- vector("list", length=length(iterationValues))
compTimes <- setNames(compTimes, iterationValues)

for (p in seq_along(iterationValues)){
  
  start_everything <- Sys.time()
  
  nIterations <- iterationValues[p] # how many times new baseline distribution data at the person-level (elasticities, etc.) will be drawn
  
  # Seeds and simulation parameters
  sim_id <- as.integer(1)
  set.seed(1235689 + sim_id) # this should be reset to the same seed each time so that the differences in our results are due to changes in the parameters alone
  simName <- "baseCase_iterationChanges"
  
  source(file=here("analysis", "00a_data-parameter-setup.R"))
  source(file=here("analysis", "00b_policy-setup_run-sim_create-output.R"))
  source(file=here("analysis", "00c_simulation-analytics.R"))
  
  end_everything <- Sys.time()
  
  compTimes[[p]] <- (end_everything-start_everything)

}

