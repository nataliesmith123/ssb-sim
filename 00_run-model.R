
start_everything <- Sys.time()

###############################################################################
# Libraries etc.
###############################################################################
library(tidyverse)
library(here)
library(survey)
library(haven)

nIterations <- 1200 # how many times new baseline distribution data at the person-level (elasticities, etc.) will be drawn
yearsSim <- 10 # how many years the diabets natural history model will be run


###############################################################################
# BASE CASE MODEL
###############################################################################

# Seeds and simulation parameters
  sim_id <- as.integer(1)
  set.seed(1235689 + sim_id) # this should be reset to the same seed each time so that the differences in our results are due to changes in the parameters alone
  simName <- "baseCase"

source(file=here("analysis", "00a_data-parameter-setup.R"))
source(file=here("analysis", "00b_policy-setup_run-sim_create-output.R"))
source(file=here("analysis", "00c_simulation-analytics.R"))


###############################################################################
# SA of SSB RR 
###############################################################################
# Seeds and simulation parameters
  sim_id <- as.integer(1)
  set.seed(1235689 + sim_id) # this should be reset to the same seed each time so that the differences in our results are due to changes in the parameters alone
  simName <- "SA_ssbRR"
  
  source(file=here("analysis", "00a_data-parameter-setup_SA-SSBRR.R"))
  source(file=here("analysis", "00b_policy-setup_run-sim_create-output.R"))
  source(file=here("analysis", "00c_simulation-analytics.R"))

  
  
###############################################################################
# SA of POLICY EFFECTS
###############################################################################
# Seeds and simulation parameters
  sim_id <- as.integer(1)
  set.seed(1235689 + sim_id) # this should be reset to the same seed each time so that the differences in our results are due to changes in the parameters alone
  simName <- "SA_policyEffects"

  source(file=here("analysis", "00a_data-parameter-setup.R"))
  source(file=here("analysis", "00b_policy-setup_run-sim_create-output_SA-tax-wl.R"))
  source(file=here("analysis", "00c_simulation-analytics.R"))
  
  
# Compare base case and sensitivity results (just on pp of prev reduction)
  source(file=here("analysis", "00e_sa-comparison.R"))
  

end_everything <- Sys.time()

(end_everything - start_everything)
