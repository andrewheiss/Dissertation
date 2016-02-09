library(rstanarm)
library(ggplot2)
library(Cairo)

# Use all possible cores
options(mc.cores = parallel::detectCores())

# Load data
full.data <- readRDS(file.path(PROJHOME, "Data","data_processed",
                               "full_data.rds"))

# Variables
# Internal stability: icrg.stability, yrsoffc, years.since.comp, opp1vote
# External stability: neighbor.stability.XXX
# International reputation: Variable from ICEWS / shaming data from Murdie
# Controls: e_polity2, physint, gdpcap.log, population.log, oda.log, countngo, globalization
