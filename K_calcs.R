#################### CODE SUMMARY ##########################################
# Description: Calculates carrying capacity (K) initial estimates for MSSPM
# using biomass data and population growth r values from the literature.
#
# Formula. K = r * B * (1-B) / CP.
# r = Rate of Population Increase (%)
# B = Population Size.
# CP = Change in Population Size.
#
# Input: 1) Biomass time series data posted to Github. 
# population growth (r) estimates in a csv file.
#
# Output: 1) CSV files with initial estimates for K for each species
#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# Date: April 5, 2021 
#
# Modified: 
# Modifications: 
############################################################################

############### Set up #############################################
#setwd("C:/Users/Howard.Townsend/Documents/EM Toolbox/MSSPM Projects/MSSPM-Keyruns")
paste("Today is", date())

library (readr)
library(tidyverse)

############### Read Data #############################################
# Biomass
urlfile1="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"
#urlfile1="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"
NOBA_sim_biomass<-as.data.frame(read_csv(url(urlfile1)))
attributes(NOBA_sim_biomass)
spec_names <- colnames(NOBA_sim_biomass[,2:ncol(NOBA_sim_biomass-1)])

# Pop growth, r,  estimates
pop_grow <- as.data.frame(read.csv2("Species_r.csv", sep=",", stringsAsFactors=F, header=TRUE))
# pop_grow$species = row.names(pop_grow)
pop_grow$r = as.numeric(pop_grow$r)




############### Calculate K #############################
### Rearrange data for easy calculating with dply
B_long <- NOBA_sim_biomass %>% gather("species", "biomass", spec_names)

### Find largest population size in biomass time series (B)
B_max = B_long %>% group_by(species) %>%  
  summarize(maxBiomass = max(biomass), maxBiomassYear = year[which.max(biomass)])

### Calculate population change from max (CP)
pop_change = B_long %>% group_by(species) %>% summarize(CP=biomass[which.max(biomass)+1]-max(biomass))
# pop_change = B_long %>% group_by(species) %>% summarize(CP=min(biomass)-max(biomass))


### Merge data frames and calculate K,  K = r * B * (1-B) / CP.
K_calc <- pop_grow %>% left_join(B_max, by = "species") %>% 
  inner_join(pop_change, by = "species")
K_calc$K_est = K_calc$r*K_calc$maxBiomass*(1-K_calc$maxBiomass)/K_calc$CP
K_calc$KtoB = K_calc$K_est/K_calc$maxBiomass

############### Output parameter estimates ###########################
write.csv(K_calc, "Parameter estimates.csv")