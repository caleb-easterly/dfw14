
rm(list = ls())

library(dplyr)
set.seed(8568)
source("obj_function_est.R")
source("obj_function_inner.R")
source("init_parms.R")
source('utilities.R')
source("solution_unrestricted.R")

# read data
pan <- read.delim("../DFW14-Supplementary/Data and Code/child_data.tab",
                  header = TRUE, sep = "\t", na.strings = "-9")

pan1 <- pan %>%
  filter(size_kid == 1)

# initialize parameters that are estimated 
parms <- init_parms()

# get parameter structure for solution
pstru <- obj_function_est(pan1, parms)

# simulate model for everybody at fitted values.
# do five reps for each family
sim_data <- NULL
for (i in 1:105) {
  for (j in 1:5) {
    sim_data <- rbind(sim_data, obj_function_inner(pan1, i, pstru, sim_draw = j))
  }
}

write.csv(sim_data, file = "sim_data.csv",
          row.names = FALSE, quote = FALSE)


### repeat for calib data

# initialize parameters that are estimated 
parms <- init_parms_calib()

# get parameter structure for solution
pstru <- obj_function_est(pan1, parms)

# simulate model for everybody at fitted values.
# do five reps for each family
sim_data <- NULL
for (i in 1:105) {
  for (j in 1:5) {
    sim_data <- rbind(sim_data, obj_function_inner(pan1, i, pstru, sim_draw = j))
  }
}

write.csv(sim_data, file = "sim_data_calib.csv",
          row.names = FALSE, quote = FALSE)

