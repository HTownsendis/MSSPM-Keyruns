#################### CODE SUMMARY ##########################################
# Description: Reads in Biomass time series and diet composition data from
# Github and calculates predation and competition/overlap coefficients.
#
# To estimate predation coefficients for MSSPM - calculates annual 
# consumption of prey biomass by predator biomass and calculates the mean
# (and Std Dev) consumption to be used as initial predation coefficient.
#
# To estimate predation coefficients for MSSP - uses Pianka (1973) Overlap index
# described further below.
#
# Input: 1) Biomass time series and die composition data posted to Github
#
# Output: 1) CSV files with predation and overlap coefficients.
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
NOBA_sim_biomass<-as.data.frame(read_csv(url(urlfile1)))

# Diet compostion
urlfile2="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/diet_BTS_fall_allbox_effic1.csv"
NOBA_diet<-as.data.frame(read_csv(url(urlfile2)))


############### Calculate consumption ##########################################
#Background: Consumption (Q) is each prey biomass consumed by each predator's 
#biomasss. Qi,t = p(j,t)*B(j,t)/B(i,t)


############### Calculate overlap #############################################
#Background: Pianka (1973) suggested the use of an overlap index derived from 
#the competition coefficients of the Lotka-Volterraequations. This index, Ojk , 
#which has been used for many descriptions of niche overlap, can be estimated, 
#for two species/groups (j) and (k), from:

#Ojk = sum (Pji*pki)/sqrt(sum(Pji^2*pki^2))

#where Pji and Pki are the proportions of the resource (i) used by species (j) 
#and (k), respectively. The index is symmetrical and assumes values between 0 
#and 1. A value of 0 suggests that the two species do not share resources, 1 
#indicates complete overlap, and intermediate values show partial overlap in 
#resource utilization
#from p.55 (https://s3-us-west-2.amazonaws.com/legacy.seaaroundus/researcher/dpauly/PDF/2000/OtherItems/ECOPATH_WITH_ECOSIM_A_USERS_GUIDE.pdf)