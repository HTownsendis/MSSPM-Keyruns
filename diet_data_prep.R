library (readr)
# setwd("C:/Users/Howard.Townsend/Documents/EM Toolbox/MSSPM Projects/NOBA_sim")


urlfile="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/diet_BTS_fall_allbox_effic1.csv"

NOBA_diet<-as.data.frame(read_csv(url(urlfile)))

library(tidyr)

library(dplyr)

#Calculate average biomass of prey consumed over years
MeanDiet <- NOBA_diet %>% group_by(species,prey) %>% summarize( MeanBiomass = mean(totdietComp))

#NOBA_interactions <- cor(NOBA_sim_biomass[16:144, -1])