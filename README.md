This repository contains the code used to simulate data presented in "Sugar-sweetened beverage warning labels and taxes: simulated impacts when considering implementation" by Smith, NR, Cruz, JL, Grummon, AH, Ng, SW, Hall, MG, Frerichs, L, Hassmiller Lich, K. 

Notes: 
* Some small pieces contained in the code posted here are not directly relevant to the simulation results presented (e.g., calculation of change in total energy intake).
* The model is not able to be run directly because we make use of NHANES dietary data cleaned and maintained by the [Global Food Research Program](https://www.globalfoodresearchprogram.org/) that is not publicly available. If you create your own NHANES input with the right variable names for demographics, SSB intake, and diabetes status, you can run the model. 
* The code makes use of functions to run the diabetes model and calculate survey-weighted average results. Those functions are applied to each row of a dataset that contains one policy-specific dataset per row.


For questions, contact Natalie Smith at natsmith@pitt.edu. 

Small modifications made during manuscript R&R updated on 10/28/25.
