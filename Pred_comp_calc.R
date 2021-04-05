#################### CODE SUMMARY ##########################################
# Description: Calculates concumption/predation coefficents and overlap 
# coefficients for multispecies surplus production models using biomass and
# Diet composition data as well as consumption to biomass estimates from literature.
#
#
# To estimate predation coefficients for MSSPM - calculates annual 
# consumption of prey biomass by predator biomass for a given year
# To do later: and calculate  the mean (and Std Dev) consumption of all years 
# to be used as initial predation coefficient.
#
# To estimate predation coefficients for MSSPM - uses Pianka (1973) Overlap index
# described further below.
#
# Input: 1) Biomass time series and diet composition data posted to Github. 
# Consumption to biomass estimates in a csv file.
#
# Output: 1) CSV files with summarized predation and overlap coefficients.
#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# Date: March 20, 2021
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

# Diet composition
urlfile2="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/diet_BTS_fall_allbox_effic1.csv"
NOBA_sim_diet<-as.data.frame(read_csv(url(urlfile2)))
# remove any species (predator) or prey that aren't included in th Biomass file
spec_names <- colnames(NOBA_sim_biomass[,2:ncol(NOBA_sim_biomass-1)])
NOBA_diet = NOBA_sim_diet %>% filter(species %in% spec_names)
attributes(NOBA_diet)

# Consumption to Biomass ratios
QtoB <- as.data.frame(read.csv2("SpeciesQB.csv", sep=",", stringsAsFactors=F, header=TRUE, row.names=1))
QtoB$QtoB = as.numeric(QtoB$QtoB)
class(QtoB$QtoB)

############### Calculate consumption coefficients #############################
#Background: Consumption coefficients, a(j,i), are the proportion of a prey (i) biomass
#consumed by predator (j). 
# 1) Calculate total annual prey biomass, Qtot, consumed 
# by predator j. Use Q/B ratios from the literature and biomass values from data.
# 2) Calculate  Biomass of each prey consumed annually by a predator, Qprey.
# Use Qtot and prey proportions from Diet composition data.
# 3) Calculate consumption coefficient, a(j,i), as proportion of prey Biomass
# consumed by a given predator. Use Qprey an prey biomass.
# Summary a(j,i) = ((Qj/Bj)*Bj*DCij)/Bi, where B and q use biomass units and
# DC is a proportion

### Consumption coefficients for a given year
# select data for given year and subset biomass and diet data for that year
period = 1

Year_B <- as.numeric(NOBA_sim_biomass %>% filter(year == period) %>% select(-year))
class(Year_B)


Year_diet = NOBA_diet %>% group_by(prey) %>% filter(year == period) %>% select(-year)
Year_dietTable <- Year_diet %>% spread(key=species, value=totdietComp) %>% 
  filter(prey %in% spec_names) %>%   mutate_all(~replace(., is.na(.), 0)) 
Year_dietMat = as.matrix(Year_dietTable[,2:ncol(Year_dietTable)])

# Calculate total annual prey biomass consumed by pred, Qtot - but first check tables aligned by species
setdiff(colnames(Year_B),colnames(Year_dietTable))
Qtot = QtoB[,2]*t(Year_B)

# Calculate  Biomass of each prey consumed annually by a predator, Qprey
Qprey=t(t(Year_dietMat)*Qtot[,1])

#Calculate consumption coefficient, a(j,i)
consumption=t(t(Year_dietMat)*1/(Qtot[,1]))


############### Calculate overlap #############################################
#Background: Pianka (1973) suggested the use of an overlap index derived from 
#the competition coefficients of the Lotka-Volterra equations. This index, Ojk , 
#which has been used for many descriptions of niche overlap, can be estimated, 
#for two species/groups (j) and (k), from:

#Ojk = sum (aji*aki)/sqrt(sum(aji^2*aki^2))

#where aji and aki are the proportions of the resource (i) used by species (j) 
#and (k), respectively. The index is symmetrical and assumes values between 0 
#and 1. A value of 0 suggests that the two species do not share resources, 1 
#indicates complete overlap, and intermediate values show partial overlap in 
#resource utilization
#from p.55 (https://s3-us-west-2.amazonaws.com/legacy.seaaroundus/researcher/dpauly/PDF/2000/OtherItems/ECOPATH_WITH_ECOSIM_A_USERS_GUIDE.pdf)
#Code adapted from: https://github.com/GotelliLab/EcoSimR/blob/master/R/metrics.R

m <- t(consumption)
pairwise <- cbind(t(combn(nrow(m),2)),0)	# set up pairwise species list
for (i in 1:nrow(pairwise)){
  pairwise[i,3] <- sum(m[pairwise[i,1],]*m[pairwise[i,2],])/
    sqrt(sum(m[pairwise[i,1],]^2)*sum(m[pairwise[i,2],]^2))
}
pairwise = as.data.frame(pairwise)
colnames(pairwise) = cbind("pred", "prey", "coeff")

overlap <- pairwise %>% spread(key=pred, value=coeff) %>% 
  mutate_all(~replace(., is.na(.), 0)) 

new_row = t(c(1,rep(0,10)))
colnames(new_row) =colnames(overlap)
overlap = rbind(new_row,overlap)
overlap$"11" =c(rep(0,11))
attributes(overlap)
overlap = overlap[,-1]
colnames(overlap) <- spec_names
############### Output predation and overlap indices ###########################
write.csv(consumption, "Predation coefficients.csv",row.names=spec_names)
write.csv(overlap, "Competition coefficients.csv",row.names=spec_names)




############### JUNK ###########################################################

# spec_ct = 1:length(spec_names)
# class(spec_names)
# 
# overlap = matrix(0,nrow=length(spec_names),ncol=length(spec_names))

# 
# for(i in spec_ct){
#   for (j in spec_ct){
#     for (k in spec_ct){
#       overlap[i,j]=(sum(consumption[i,j])*sum(consumption[i,k]))/sqrt(sum(consumption[i,j]*sum(consumption[i,k])))
#     }
#   }
# }
# 
# overlap[is.na(overlap)] = 0
# colnames(overlap) <- spec_names
