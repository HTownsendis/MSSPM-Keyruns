library (readr)
# setwd("C:/Users/Howard.Townsend/Documents/EM Toolbox/MSSPM Projects/noba_simdata")


urlfile="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"

NOBA_sim_biomass<-as.data.frame(read_csv(url(urlfile)))

NOBA_interactions <- cor(NOBA_sim_biomass[16:144, -1])

write.csv(NOBA_interactions, "NOBA_sim_biomass_cor.csv",row.names=FALSE)
