library (readr)
setwd("C:/Users/Howard.Townsend/Documents/EM Toolbox/MSSPM Projects/MSSPM-Keyruns/noba_simdata")


urlfile="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"

NOBA_sim_biomass<-as.data.frame(read_csv(url(urlfile)))

NOBA_interactions <- cor(NOBA_sim_biomass[16:144, -1])

write.csv(NOBA_interactions,"Biomass_cor.csv",row.names=FALSE)

write.csv(NOBA_sim_biomass, "Biomass.csv",row.names=FALSE)

urlfile2="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/fishery_census.csv"

NOBA_sim_catch<-as.data.frame(read_csv(url(urlfile2)))

write.csv(NOBA_sim_catch, "Catch.csv",row.names=FALSE)
