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
# Modified: July 30 - add burn-in removal for modeled data, refined K calc
# Modifications: 
############################################################################

############### Set up #############################################
setwd("C:/Users/Howard.Townsend/Documents/EM Toolbox/MSSPM Projects/MSSPM-Keyruns/noba_simdata")
paste("Today is", date())

library (readr)
library(tidyverse)
library(ggplot2)
library(ggforce)

############### Read Data #############################################
# Biomass
urlfile1="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"
#urlfile1="https://raw.githubusercontent.com/NOAA-EDAB/ms-keyrun/master/simulated-data/msinput/msspm/BTS_fall_allbox_effic1.csv"
rawBiomass<-as.data.frame(read_csv(url(urlfile1)))
attributes(rawBiomass)
spec_names <- colnames(rawBiomass[,2:ncol(rawBiomass-1)])

#remove burn-in period for Atlantis simulated data
Biomass <- rawBiomass[rawBiomass$year > 20, ]

# Pop growth, r,  estimates
pop_grow <- as.data.frame(read.csv2("Species_r.csv", sep=",", stringsAsFactors=F, header=TRUE))
pop_grow$species = spec_names
pop_grow$r = as.numeric(pop_grow$r)

############### Check Data ############################################
### Rearrange data for easy calculating with dplyr and quick plotting
B_long <- Biomass %>% gather("species", "biomass", spec_names)

### Create function for formatting y-labels (Biomass)
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}


### Plot input Biomass
sp_list = unique(B_long$species)
pdf("NOBA_simdata_input_same y.pdf")
for (i in 1:length(sp_list)){
  print(ggplot(B_long,aes(year, biomass)) +
          theme_classic() +
          theme(plot.margin = unit(c(10, 30, 10, 10), "pt")) +
          geom_point(shape = 19, color = "black") +
          facet_grid_paginate(~species, ncol = 1, nrow = 1, page = i) +
          scale_y_continuous(labels=fancy_scientific) )
}
dev.off()




sp_ct = ncol(Biomass)-1
pdf("NOBA_simdata_input.pdf")
for (i in 1:sp_ct){
  print(ggplot(Biomass,aes(Biomass[,1],  Biomass[,i+1])) +
          theme_classic() +
          theme(plot.margin = unit(c(10, 30, 10, 10), "pt")) +
          geom_line(color = "black") + 
          ggtitle(colnames(Biomass[i+1])) + 
          labs(y="Biomass (mt)", x = "Year") +
          scale_y_continuous(labels=fancy_scientific) )
}
dev.off()


############### Calculate K #############################
### Find largest population size in biomass time series (B)
B_max = B_long %>% group_by(species) %>%  
  summarize(maxBiomass = max(biomass), maxBiomassYear = year[which.max(biomass)])

### Calculate population change from max (CP)
# pop_change = B_long %>% group_by(species) %>% summarize(CP=biomass[which.max(biomass)+1]-max(biomass))
pop_change = B_long %>% group_by(species) %>% summarize(CP=biomass[which.max(biomass)-1]-max(biomass))
# pop_change = B_long %>% group_by(species) %>% summarize(CP=min(biomass)-max(biomass))


### Merge data frames and calculate K,  K = r * B * (1-B) / CP
K_calc <- pop_grow %>% left_join(B_max, by = "species") %>% 
  left_join(pop_change, by = "species")
K_calc$K_est = K_calc$r*K_calc$maxBiomass*(1-K_calc$maxBiomass)/K_calc$CP
K_calc$KtoB = K_calc$K_est/K_calc$maxBiomass

if_else(K_calc$KtoB < 2, K_calc$K_est == 2*K_calc$maxBiomass, K_calc$K_est == K_calc$K_est )
if_else(K_calc$KtoB < 2, K_calc$KtoB == 2, K_calc$KtoB < 2 )
K_calc$B0=1.5*K_calc$maxBiomass

############### Output parameter estimates ###########################
write.csv(K_calc, "Parameter estimates.csv")


############## Check



# pdf("NOBA_simdata_inputs.pdf", 7, 5)
# for (i in 1:length(unique(B_long$species))) {
#   print(qplot(year, biomass, data=B_long) + 
#           facet_wrap( ~ species, ncol=1) + 
#           theme(legend.position = "none") +
#           ggtitle("NOBA sim dat rawBiomass"))
# }
# dev.off()


# plot(Biomass$year, Biomass$Norwegian_ssh,type="l")
# plot(Biomass$year, Biomass$North_atl_cod,type="l")
# plot(Biomass$year, Biomass$Blue_whiting,type="l")

# 
# p1 <- qplot(year, biomass, data=B_long)
# p2 <-
#   p1 + facet_wrap( ~ species, ncol = 1) + theme(legend.position = "none") +
#   ggtitle("NOBA sim dat Biomass")
# p2
# 
# rawB_long <- rawBiomass %>% gather("species", "biomass", spec_names)
# p3 <- qplot(year, biomass, data=B_long)
# p4 <-
#   p3 + facet_wrap( ~ species, ncol=1) + theme(legend.position = "none") +
#   ggtitle("NOBA sim dat rawBiomass")
# print(p4, page = 6)

