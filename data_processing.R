#####------------------------create dataframes for analysis from UVC data -------------------#######

#getwd()
#setwd("C:/git_repos_2020/bef-reefs")

#####------------------------load packages-------------------#######
library(vegan)
library(reshape2)
library(plyr)
library(iNEXT)


#####------------------------load UVC fish data-------------------#######
fish_dat <- read.csv("data/Chagos_rats_birds_UVC_data_2015_2018_2019.csv", header=TRUE) #note: doesn't include smallest indivdiuals (<6 cm) from 2015 because these weren't counted in 2018
str(fish_dat)
#metadata: 4 Transects per Island (spread across 3 Atolls) per Year
##Structure: structural complexity on 0-5 scale
##Coral_cover: percent cover of hard coral on transect
##Area: survey area of transect (m^2) - 60 m^2 for Pomacentridae, 150 m^2 for all other families
##Length: total length of individual, estimated to the nearest cm
##Abundance: indivduals/hectare


##Eagle surveyed in 2019 instead of 2018 due to logistical constraints. But change Year = 2019 to 2018 to make analysis easier 
#(b/c Year is really a factor corresponding to pre-climate extreme vs. post-climate extreme)
fish_dat$Year[fish_dat$Year==2019]<-2018
range(fish_dat$Year)

#####------------------------calculate biomass-------------------#######
# load datafile with values for a,b to convert length to biomass (also has traits)
chagos_ab<-read.csv('data/updated_Chagos_fish_2015_2018_2019_traits_ab.csv')
str(chagos_ab)


#make species format match
chagos_ab$Species<-as.factor(gsub(' ', '_', chagos_ab$Species))


#drop unneeded rows in trait data - only need a and b for now
chagos_ab_red <- subset(chagos_ab, select=c(Species, a,b))
head(chagos_ab_red)


##merge species data with survey data
fish_dat_2<-merge(fish_dat, chagos_ab_red, by=c("Species"), all.x=TRUE)
str(fish_dat_2)                     

nrow(fish_dat)
nrow(fish_dat_2)
###matches


#calculate biomass values to use
#biomass =a *length^b
fish_dat_2$Biomass_g<-fish_dat_2$a*fish_dat_2$Length^fish_dat_2$b

#convert to kg/ha - grams per area (m^2) *10
fish_dat_2$Biomass<-fish_dat_2$Biomass_g/fish_dat_2$Area * 10
str(fish_dat_2)


##re-order columns so it makes more sense
col_order <- c("Year", "Atoll", "Island",
               "Treatment", "Transect", "Structure", "Coral_cover", "Area", "Family", "Species", "Length", "Abundance",
               "a", "b", "Biomass_g", "Biomass")

fish_dat_2 <- fish_dat_2[, col_order]
str(fish_dat_2)

#####------------------------calculate richness-------------------#######
#first need to sum species by transect
#then need community matrix to calculate diversity indices

#add column to get counts (so just number of each species, not scaled by transect area)
fish_dat_2$count<-1

#now sum each species and transect by count
fish_dat_sum <-ddply(fish_dat_2, c("Year", "Atoll", "Island", "Treatment", "Transect", "Structure", "Coral_cover", "Species"), summarise,
                     Count = sum(count), 
                     Density = sum(Abundance),
                     Sum_Bio = sum(Biomass))
fish_dat_sum

#convert to wide
fish_dat_count_wide <- dcast(fish_dat_sum, Year+ Atoll + Island + Treatment + Structure+Coral_cover+ Transect  ~ Species, value.var="Count")
fish_dat_density_wide <- dcast(fish_dat_sum, Year + Atoll + Island + Treatment + Structure+Coral_cover+Transect  ~ Species, value.var="Density")
fish_dat_bio_wide <- dcast(fish_dat_sum, Year+ Atoll + Island + Treatment + Structure+Coral_cover+ Transect  ~ Species, value.var="Sum_Bio")

#remove NAs
fish_dat_count_wide[is.na(fish_dat_count_wide)]<-0
fish_dat_density_wide[is.na(fish_dat_density_wide)]<-0
fish_dat_bio_wide[is.na(fish_dat_bio_wide)]<-0
nrow(fish_dat_count_wide) #92 - makes sense = 4*12 + 4*11 = 92, so correct number of transects
nrow(fish_dat_density_wide)
nrow(fish_dat_bio_wide)

#sum density, biomass
str(fish_dat_density_wide)
sum_dens<-rowSums(fish_dat_density_wide[,8:ncol(fish_dat_density_wide)])
sum_bio<-rowSums(fish_dat_bio_wide[,8:ncol(fish_dat_bio_wide)])

#calculate richness
richness<-specnumber(fish_dat_density_wide[8:ncol(fish_dat_density_wide)]) #doesn't matter if you use density/biomass b/c presence-absence

#merge data
chagos_div<-cbind(fish_dat_density_wide[1:7],richness, sum_dens, sum_bio)
head(chagos_div)

#change Year to factor
chagos_div$Year<-as.factor(chagos_div$Year)


#calculate rarefied richness with inext

#first need to have treat_year_trans column
fish_dat_sum_treatyear<-fish_dat_sum
fish_dat_sum_treatyear$TreatYear<-paste(fish_dat_sum_treatyear$Year, fish_dat_sum_treatyear$Island,  fish_dat_sum_treatyear$Transect, sep='_')
fish_dat_sum_treatyear

#convert to wide
fish_dat_ty_density_wide <- dcast(fish_dat_sum_treatyear, Species ~ TreatYear, value.var="Count")
fish_dat_ty_density_wide

#remove na's
fish_dat_ty_density_wide[is.na(fish_dat_ty_density_wide==TRUE)]<-0
fish_dat_ty_density_wide


#make "Species" rownames
rownames(fish_dat_ty_density_wide) <- fish_dat_ty_density_wide[,1]
fish_dat_ty_density_wide[,1] <- NULL
head(fish_dat_ty_density_wide)

#use iNEXT to get rarefied richness
rare2<-iNEXT(fish_dat_ty_density_wide, datatype="abundance")
rare2$AsyEst
rare2$DataInfo

##just extract richness
chao<-ChaoRichness(fish_dat_ty_density_wide, datatype="abundance")	
chao$TreatYear<-rownames(chao)

#merge with other transect info
head(chagos_div)
chagos_div_ty<-chagos_div
chagos_div_ty$TreatYear<-paste(chagos_div_ty$Year, chagos_div_ty$Island,  chagos_div_ty$Transect, sep='_')

chagos_div_2<-merge(chagos_div_ty, chao, by="TreatYear")
chagos_div_2

chagos_div_2$Year<-as.factor(chagos_div_2$Year)
chagos_div_2$lbio<-log(chagos_div_2$sum_bio)
chagos_div_2$lrich<-log(chagos_div_2$richness)
chagos_div_2$lrare<-log(chagos_div_2$Estimator)

str(chagos_div_2)


#####----------------------combine UVC data with kmax data-------------------------------#####
#load Kmax data
kmax_list<-read.csv('data/DS2.csv') ##Supplemental file from from Morais and Bellwood 2019 with Kmax data in it
head(kmax_list)
unique(kmax_list$MaxSizeTL)
unique(kmax_list$sstmean)

##from sheppard et al. 2012: mean sst 28.5-30 degrees C in Chagos Archipelago in summer
##2015 survey dates: 22 March - 07 April 2015
##2018 survey dates: 05 May - 18 May 2018
##From NOAA bleachwatch, temps in Chagos for 1 year post 2015 survey = mean = 29.15
##From NOAA bleachwatch, temps in Chagos for 1 year post 2018 survey = mean = 28.70
##so options are 28 degrees or 30 degrees. Opted to go with 28 degrees (shouldn't make much of a difference, especially since it's applied to all reefs).

##limit list to right temp
kmax_list<-kmax_list[kmax_list$sstmean==28,]
kmax_list
nrow(kmax_list)

##also need species traits data (loaded earlier)
head(chagos_ab)

#merge kmax based on traits with trait info for species
str(chagos_ab)
str(kmax_list)
sp_kmax<-merge(chagos_ab, kmax_list, by=c("MaxSizeTL","Diet", "Position"), all.x=TRUE)
sp_kmax
##Sweet, now we have kmax for each species (and Kmax_lowqt and Kmaxupqt)

nrow(chagos_ab)
nrow(sp_kmax)
nrow(kmax_list)
duplicated(sp_kmax) #but wait, some rows duplicated - so delete duplicated rows
sp_kmax_2<-unique(sp_kmax)
nrow(sp_kmax_2)


##add column for k values in fish data
head(fish_dat)
head(fish_dat_2)
head(sp_kmax_2)
fish_dat_k<-merge(fish_dat_2,sp_kmax_2, by=c("Species", "Family", "a", "b"), all.x=TRUE )
head(fish_dat_k)
nrow(fish_dat_k)
nrow(fish_dat)
nrow(fish_dat_2)
#all good

##calculate k per transect (not actually necessary)
fish_k_trans<-ddply(fish_dat_k, c("Year", "Atoll", "Island", "Transect"), summarise,
                    mean_Kmax = mean(Kmax),
                    sum_Kmax = sum(Kmax),
                    mean_Klow = mean(Kmax_lowqt),
                    mean_Kup= mean(Kmax_upqt),
                    sum_Klow = sum(Kmax_lowqt),
                    sum_Kup= sum(Kmax_upqt)
)

fish_k_trans


##merge with other diversity, biomass, etc. data
head(chagos_div_2)
chagos_div_2_k<-merge(chagos_div_2, fish_k_trans, by=c("Year", "Atoll", "Island", "Transect") )
chagos_div_2_k
nrow(chagos_div_2)
nrow(chagos_div_2_k)

chagos_div_2_k$lkmax<-log(chagos_div_2_k$sum_Kmax)
str(chagos_div_2_k)

#save as .Rdata
save(sp_kmax_2, fish_dat, fish_dat_2, fish_dat_sum, fish_dat_k,chagos_div_2, chagos_div_2_k, file = 'data/Chagos_bio_kmax_rich_2019.Rdata')
