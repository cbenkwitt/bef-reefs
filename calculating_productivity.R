###calculating secondary production from fish survey data
###derived from Morais & Bellwood 2019
##note: because this code involves simulations, production results will vary slightly each time. Actual data used in all analyses and figures are provided in accompanying .Rdata fliles.


####----------------load packages---------------------####
library(Rlab) #for mortality simulations
library(plyr)
library(dplyr)

####-------Calculating biomass production---------####
#Derived from Morais & Bellwood 2019


####-----------------------load data---------------------------##########
#.Rdata files created from raw UVC data in code: data_processing.R
#Kmax from Morais and Bellwood, SST = 28 degrees
load(file='data/Chagos_bio_kmax_rich_2019.Rdata')


##need survey data - use data for individual fish PLUS already has traits included
#quick look at data as a reminder:
head(fish_dat_k)
str(fish_dat_k)


#####-----------------------estimated age and growth rate------------------------------------------######
####calculate starting estimated age of individual fish, given its length (from formula 3 in Depczynski et al. 2007, referenced in Morais and Bellwood 2019)
#age = (1/kmax)*ln(Lmax/((1-Lactual/Lmax)*Lmax))

#check which species estimated length > max length
fish_dat_k[fish_dat_k$Length>fish_dat_k$MaxSizeTL_Prod,] # only a few individuals, all lengths quite close to max length

#so need to make any lengths > max size = max size (because we're assuming lengths were slightly overestimated in the field), otherwise formula doesn't work
fish_dat_prod<-fish_dat_k
fish_dat_prod$Length[fish_dat_prod$Length>fish_dat_prod$MaxSizeTL_Prod]<-fish_dat_prod$MaxSizeTL_Prod[fish_dat_prod$Length>fish_dat_prod$MaxSizeTL_Prod]
fish_dat_prod[fish_dat_prod$Length>fish_dat_prod$MaxSizeTL_Prod,] #now this is empty - so above formula worked


#calculate age
fish_dat_prod$EstAge<-(1/fish_dat_prod$Kmax)*log((fish_dat_prod$MaxSizeTL_Prod)/((1-fish_dat_prod$Length/fish_dat_prod$MaxSizeTL_Prod)*fish_dat_prod$MaxSizeTL_Prod))
range(fish_dat_prod$EstAge)
#range from 1.003433 to infinity
#infinity just means already reached max length, so for these growth rate should = 0. plus doesn't matter that we don't know the age for these individuals, since length will just stay the same


#okay, now have ages - plug back into VGBF to calculate growth
#VBGF: Lt = Lmax*(1-exp(-K*t))
#t = Estimated Age + time interval
#following Morais and Bellwood 2019 - interval = 1 day (so 1/365)
##do this over a full year, so age + (1:365)/365

## Age to add for each day of the year
age <- (1:365) / 365


#set up table for new lengths: 1 column per day, 1 row per individual fish
vb_len_tab <- matrix (ncol = length (age), nrow = nrow (fish_dat_prod), dimnames = list (NULL, paste ('Day', 1:365, sep="_")))

#for each individual, calculate new length for that day using VBGF formula (written above)
#get same result when use:
#library(FSA)
#growthFunShow("vonBertalanffy", param="original")
#vbf <- vbFuns ('original')
#and then use below line in for loop:
#lgrtab [u, ] <- vbf (age, fish_dat_prod3$MaxSizeTL [u], fish_dat_prod3$Kmax[u], fish_dat_prod3$Length [u])


for (u in 1:nrow (fish_dat_prod)) {
  
  vb_len_tab [u, ] <- fish_dat_prod$MaxSizeTL_Prod[u]*(1-exp(-fish_dat_prod$Kmax[u]*(fish_dat_prod$EstAge[u]+age))) 
  
}

vb_len_tab
##now have matrix of lengths for every day of the year
range(vb_len_tab)



##convert length to weight using a,b coefs
vb_wt_tab <- apply (vb_len_tab, 2, function (x) fish_dat_prod$a * (x ^ fish_dat_prod$b))
head(vb_wt_tab)
##units =  grams (per individual)
range(vb_wt_tab)

##to convert to kg/ha need to divide by survey area (m ^2) to get g/m^2, and multpiply by 10
wt_kgh_tab<-apply(vb_wt_tab,2, function (x) x/fish_dat_prod$Area * 10 )

#so now have two large matrices with estimated length and weight of each individual over a year
##still need to convert this to RATE not actual age/length - to do so will just substract new weight - initial weight over the year = kg/ha/year


#create one last table with production (in grams, weight) per day
vb_wt_tab
biomass_not_rounded_g<-(fish_dat_prod$a*fish_dat_prod$Length^fish_dat_prod$b)
vb_wt_tab <- cbind(biomass_not_rounded_g, vb_wt_tab)
colnames(vb_wt_tab)[1]<- "Day_0"
vb_prod_wt_tab<-vb_wt_tab
head(vb_prod_wt_tab)

for (u in 2:ncol(vb_prod_wt_tab))
  for (v in 1:nrow(vb_prod_wt_tab)) {
    vb_prod_wt_tab [v,u ] <- vb_wt_tab[v,u]-vb_wt_tab[v,(u-1)]
  }
vb_prod_wt_tab

##so each is not cumulative, just daily production...
##delete first column, because this is just the baseline
##this is still in g/individual
vb_prod_wt_tab2<-vb_prod_wt_tab[,2:ncol(vb_prod_wt_tab)]
vb_prod_wt_tab2
###so now we have the daily production for each fish over the course of a year in grams


#####-----CONTINUE TO INCORPORATE SIZE-INDEPENDENT MORTALITY--------########

####--------------------calculate mortality----------------------------####
##so above is gross productivity, then  factor in mortality to get net productivity
#from Pauly (1980), referenced in Morais and Bellwood 2019:
#log(M) = -0.0066-0.279*log(Lmax)+0.6543*log(Kmax)+0.4634*log(temperature), where M = natural mortality 
#Z = M + F, but since fishing is prohibited, F = 0, and Z = M

head(fish_dat_prod)

fish_dat_prod$logZyear<- (-0.0066-(0.279*log(fish_dat_prod$MaxSizeTL_Prod))+(0.6543*log(fish_dat_prod$Kmax))+(0.4634*log(28)))
fish_dat_prod$Zyear<-exp(fish_dat_prod$logZyear)
range(fish_dat_prod$Zyear)
#range for M= ~0.2 to 2.076155
#similar to Brandl et al. 2019: range = 0.34 to 4.83

fish_dat_prod$Zday<-fish_dat_prod$Zyear/365
range(fish_dat_prod$Zday)



####--------------------calculate probability of survival----------------------------####
#probability of survival = exp(-zday)
#NOTE: probSurv does NOT equal exp (-fish_dat_prod4$Zday * age), this was a typo in the original paper
#so mortality doesn't scale with age right now, which isn't perfect but was used in Brandl et al. 2019

fish_dat_prod$SurvivalDay<-exp(-fish_dat_prod$Zday)
head(fish_dat_prod)


#now make vector of daily survival probabilities for the whole year
psurv_tab <- matrix (ncol = 365, nrow = nrow (fish_dat_prod), dimnames = list (NULL, paste ('Day', 1:365)))

for (u in 1:nrow (fish_dat_prod)) {
    psurv_tab [u, ] <- exp(-fish_dat_prod$Zday[u]) 
}
###so daily probability of survival does NOT change throughout the year


###If want cumulative survival, multiple each day*all the days before
##so probability of surviving each day is probability of surviving that day*probability of surviving all the days before....
psurv_cum_tab <- psurv_tab

for (u in 2:ncol(psurv_cum_tab))
  for (v in 1:nrow(psurv_cum_tab)) {
    psurv_cum_tab [v,u ] <- psurv_cum_tab[v,u]*psurv_cum_tab[v,(u-1)]
  }
psurv_cum_tab



psurv_yr<-apply(psurv_tab, 1, prod)

#quick check:
range(psurv_yr)
range(psurv_cum_tab[,365])
##yup, last day matches, so that's good

#so daily survival probabilities for each individual (which is based only on species survival probabilities)
#probability of survival DECREASES through time, because this is now CUMULATIVE survival probability



#####------simulate mortality per day-------####
surv_outcome_day <- matrix (ncol = 365, nrow = nrow (psurv_tab))


for (u in 1:nrow(psurv_tab))
  for (v in 1:ncol(psurv_tab)) {
    surv_outcome_day [u,v ] <- rbern(1,prob= psurv_tab[u,v])
  }
surv_outcome_day


nrow(subset(surv_outcome_day,rowSums(surv_outcome_day)!=365))
nrow(subset(surv_outcome_day,rowSums(surv_outcome_day)==365))
##now approximately half the fish die during the year, makes sense given the outcome from yearly survival (note: exact values will change b/c simulated)

##need to make it so once there is a zero in the row, every value after that is zero
zero_row_replace <- function(x){
  x[which(cumany(x==0))] <- 0
  return(x)
}

surv_outcome_day2<-adply(surv_outcome_day, 1, zero_row_replace)
head(surv_outcome_day2)
surv_outcome_day2<-select(surv_outcome_day2, -c(X1)) #remove first row, don't want that

nrow(subset(surv_outcome_day2,rowSums(surv_outcome_day2)!=365))
nrow(subset(surv_outcome_day2,rowSums(surv_outcome_day2)==365)) ##still matches above

##now multiply survival table*production table to get production estimates:
vb_prod_wt_tab2 # this is table of daily production in g/individual (not cumulative)
est_daily_prod_tab<-surv_outcome_day2*vb_prod_wt_tab2
head(est_daily_prod_tab)

#and then sum daily production over the course of a year
prod_year<-rowSums(est_daily_prod_tab)
prod_year


##make final conversions and save above simulation
fish_dat_prod_final<-fish_dat_prod
head(fish_dat_prod_final)
fish_dat_prod_final$prod_year_g<-prod_year
head(fish_dat_prod_final)

#convert to kg/ha
fish_dat_prod_final$prod_year_kg_h<-fish_dat_prod_final$prod_year_g/fish_dat_prod_final$Area*10


##sum productivity by transect
prod_trans<-ddply(fish_dat_prod_final, c("Year", "Treatment", "Atoll", "Island", "Transect"), summarise,
                  total_prod_year=sum(prod_year_kg_h)
)

prod_trans


####combine with other data
chagos_div_2_k_prod<-merge(chagos_div_2_k,prod_trans, by=c("Year", "Treatment", "Atoll", "Island", "Transect"), all.x=TRUE)
head(chagos_div_2_k_prod)
nrow(chagos_div_2_k_prod)
nrow(chagos_div_2_k)
##looks good

#log productivity for analysis:
chagos_div_2_k_prod$lprod<-log(chagos_div_2_k_prod$total_prod_year)


###save datafiles
#save as .Rdata
#save(fish_dat_prod_final, chagos_div_2_k_prod, file = 'data/Chagos_prod_rich_2019.Rdata')


#####-----------repeat above, but with different Kmax values--------######
#above is assuming no difference in Kmax between birdy and ratty islands
#now try to account for observed and potential differences in growth rates between birdy versus ratty islands:
#so add column to adjust kmax
#see Methods section for additional info

#estimated differences birdy versus ratty:
#mean = 24.7 % diff
#max = 44.5% diff
#min = 10.0% diff

##min
fish_dat_k$Kmax_min<-fish_dat_k$Kmax+fish_dat_k$Kmax*0.05 ###increase birdy islands by 5%
fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax_min<-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax*0.05 ##decrease ratty islands by 5% 

##mean
fish_dat_k$Kmax_mean<-fish_dat_k$Kmax+fish_dat_k$Kmax*.1235 ###increase birdy islands by 12.35%
fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax_mean<-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax*0.1235 ##decrease ratty islands by 12.35% 


##max
fish_dat_k$Kmax_max<-fish_dat_k$Kmax+fish_dat_k$Kmax*0.2225 ###increase birdy islands by 22.25%
fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax_max<-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax-fish_dat_k[fish_dat_k$Treatment=="Rats",]$Kmax*0.2225 ##decrease ratty islands by 22.25%  


####now, re-run all above code, but each time replace "Kmax" with "Kmax_min", "Kmax_mean", and "Kmax_max". Files for each of these runs saved as:
#save(fish_dat_prod_final, chagos_div_2_k_prod, file = 'data/Chagos_prod_rich_2019_NEWK_max45.Rdata')
#save(fish_dat_prod_final, chagos_div_2_k_prod, file = 'data/Chagos_prod_rich_2019_NEWK_max45.Rdata')
#save(fish_dat_prod_final, chagos_div_2_k_prod, file = 'data/Chagos_prod_rich_2019_NEWK_max45.Rdata')
