###Run models to test for biodiversity-ecosystem function (BEF) relationships, and effect of stressors (climate extreme and invasive rats) on BEF
###UPDATED AND CLEANED 24/02/2020 (re-ran all models using nlme to include correlation term, as per reviewer comments)

#####--------load packages--------####

library(ggplot2)
library(vegan)
library(reshape2)
library(plyr)
library(car) #for VIF
library(nlme) ##needed to include correlation term
library(ggeffects) #for marginal effects
library(lavaan) #for path anlaysis
library(semPlot) #for path anlaysis


########-------------load biomass data----------##########
load(file='data/Chagos_bio_kmax_rich_2019.Rdata')

#log coral cover
chagos_div_2_k$lcc<-log(chagos_div_2_k$Coral_cover+1) #log coral cover

##add treatment*year column to help plotting
chagos_div_2_k$TreatYear2<-paste(chagos_div_2_k$Year, chagos_div_2_k$Treatment, sep='_')
chagos_div_2_k$TreatYear2<-as.factor(chagos_div_2_k$TreatYear2)


########-----------load productivity data--------------#######
#4 options for magnitude of bird-rat differences:

#1
load(file='data/Chagos_prod_rich_2019.Rdata')

##log coral cover
chagos_div_2_k_prod$lcc<-log(chagos_div_2_k_prod$Coral_cover+1)

#rename dataframe
prod_Knone<-chagos_div_2_k_prod


#2
load(file='data/Chagos_prod_rich_2019_NEWK_min10.Rdata') #no rats = 5% higher than mean, rats = 5% lower than mean (= min 10% diff)
head(chagos_div_2_k_prod)

##log coral cover
chagos_div_2_k_prod$lcc<-log(chagos_div_2_k_prod$Coral_cover+1)

#rename dataframe
prod_Kmin<-chagos_div_2_k_prod


#3
load(file='data/Chagos_prod_rich_2019_NEWK_max45.Rdata') #no rats = 22.25% higher than mean, rats = 22.25% lower than mean (=max = 44.5% diff)
head(chagos_div_2_k_prod)

##log coral cover
chagos_div_2_k_prod$lcc<-log(chagos_div_2_k_prod$Coral_cover+1)

#rename dataframe
prod_Kmax<-chagos_div_2_k_prod


#4
load(file='data/Chagos_prod_rich_2019_NEWK_mean25.Rdata') #no rats = 12.35% higher than mean, rats = 12.35% lower than mean (=mean = 24.7 % diff)
head(chagos_div_2_k_prod)

##log coral cover
chagos_div_2_k_prod$lcc<-log(chagos_div_2_k_prod$Coral_cover+1)

#rename dataframe
prod_Kmean<-chagos_div_2_k_prod




#####------------biomass models: additive----------####
###additive models
log_rich_bio_add_temp<-lme(lbio~lrich+Year+Treatment+Structure+lcc, random =~1|Atoll/Island, 
                           correlation = corCompSymm(form = ~as.numeric(Year) | Atoll/Island), data=chagos_div_2_k)
summary(log_rich_bio_add_temp) #model output

##check assumptions: 
plot(log_rich_bio_add_temp) ##ok
qqnorm(log_rich_bio_add_temp) ##looks good
plot(log_rich_bio_add_temp, Treatment~resid(.))##looks good
plot(log_rich_bio_add_temp, Year~resid(.))##looks pretty good
plot(log_rich_bio_add_temp, Island~resid(.))##looks pretty good
plot(log_rich_bio_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rich_bio_add_temp) # highest = 1.7


#save estimates and CIs:
log_rich_bio_add_temp_95ci<-intervals(log_rich_bio_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_bio_add_temp_75ci<-intervals(log_rich_bio_add_temp,which="fixed",level=0.75) #get 75% CI
log_rich_bio_add_temp_df<-data.frame(cbind(log_rich_bio_add_temp_95ci$fixed, log_rich_bio_add_temp_75ci$fixed))
colnames(log_rich_bio_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_bio_add_temp_df
#write.csv(log_rich_bio_add_temp_df, "scripts/revisions_scripts/output_r2/log_rich_bio_add_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 1)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_bio_add_mod_scale_temp<-lme(lbio~scale(lrich)+Year+Treatment+scale(Structure)+scale(lcc), 
                                     random =~1|Atoll/Island, 
                                     correlation = corCompSymm(form = ~Year| Atoll/Island),
                                     data=chagos_div_2_k)
summary(log_rich_bio_add_mod_scale_temp) #model output

#save:
log_rich_bio_add_temp_scale_95ci<-intervals(log_rich_bio_add_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_bio_add_temp_scale_75ci<-intervals(log_rich_bio_add_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rich_bio_add_temp_scale_df<-data.frame(cbind(log_rich_bio_add_temp_scale_95ci$fixed, log_rich_bio_add_temp_scale_75ci$fixed))
colnames(log_rich_bio_add_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_bio_add_temp_scale_df
#write.csv(log_rich_bio_add_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rich_bio_add_temp_lme_ci_scale.csv")




#####------------productivity models: additive----------####
###additive models
log_rich_prod_add_temp<-lme(lprod~lrich+Year+Treatment+Structure+lcc, random =~1|Atoll/Island,
                            correlation = corCompSymm(form = ~Year | Atoll/Island), data=prod_Knone)
summary(log_rich_prod_add_temp) #model output


##check assumptions: 
plot(log_rich_prod_add_temp) ##ok
qqnorm(log_rich_prod_add_temp) ##looks good
plot(log_rich_prod_add_temp, Treatment~resid(.))##looks good
plot(log_rich_prod_add_temp, Year~resid(.))##looks pretty good
plot(log_rich_prod_add_temp, Island~resid(.))##looks pretty good
plot(log_rich_prod_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rich_prod_add_temp) # highest = 1.9


#save estimates and CIs:
log_rich_prod_add_temp_95ci<-intervals(log_rich_prod_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_add_temp_75ci<-intervals(log_rich_prod_add_temp,which="fixed",level=0.75) #get 75% CI
log_rich_prod_add_temp_df<-data.frame(cbind(log_rich_prod_add_temp_95ci$fixed, log_rich_prod_add_temp_75ci$fixed))
colnames(log_rich_prod_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_prod_add_temp_df
#write.csv(log_rich_prod_add_temp_df, "scripts/revisions_scripts/output_r2/log_rich_prod_add_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 1)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_prod_add_mod_scale_temp<-lme(lprod~scale(lrich)+Year+Treatment+scale(Structure)+scale(lcc), 
                                      random =~1|Atoll/Island, 
                                      correlation = corCompSymm(form = ~Year| Atoll/Island),
                                      data=prod_Knone)
summary(log_rich_prod_add_mod_scale_temp) #model output

#save:
log_rich_prod_add_temp_scale_95ci<-intervals(log_rich_prod_add_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_add_temp_scale_75ci<-intervals(log_rich_prod_add_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rich_prod_add_temp_scale_df<-data.frame(cbind(log_rich_prod_add_temp_scale_95ci$fixed, log_rich_prod_add_temp_scale_75ci$fixed))
colnames(log_rich_prod_add_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_prod_add_temp_scale_df
#write.csv(log_rich_prod_add_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rich_prod_add_temp_lme_ci_scale.csv")



#####----------biomass models: with interaction terms--------####
###interaction models
log_rich_bio_full_temp<-lme(lbio~lrich*Year+Treatment*lrich+Structure+lcc, random =~1|Atoll/Island, 
                            correlation = corCompSymm(form = ~Year| Atoll/Island), data=chagos_div_2_k)
summary(log_rich_bio_full_temp) #model output

##check assumptions: 
plot(log_rich_bio_full_temp) ##ok
qqnorm(log_rich_bio_full_temp) ##looks good
plot(log_rich_bio_full_temp, Treatment~resid(.))##looks good
plot(log_rich_bio_full_temp, Year~resid(.))##looks pretty good
plot(log_rich_bio_full_temp, Island~resid(.))##looks pretty good
plot(log_rich_bio_full_temp, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rich_bio_full_temp_95ci<-intervals(log_rich_bio_full_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_bio_full_temp_75ci<-intervals(log_rich_bio_full_temp,which="fixed",level=0.75) #get 75% CI
log_rich_bio_full_temp_df<-data.frame(cbind(log_rich_bio_full_temp_95ci$fixed, log_rich_bio_full_temp_75ci$fixed))
colnames(log_rich_bio_full_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_bio_full_temp_df
#write.csv(log_rich_bio_full_temp_df, "scripts/revisions_scripts/output_r2/log_rich_bio_full_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_bio_full_mod_scale_temp<-lme(lbio~scale(lrich)*Year+scale(lrich)*Treatment+scale(Structure)+scale(lcc), 
                                  random =~1|Atoll/Island, 
                                  correlation = corCompSymm(form = ~Year| Atoll/Island),
                                  data=chagos_div_2_k)
summary(log_rich_bio_full_mod_scale_temp) #model output

#save:
log_rich_bio_full_temp_scale_95ci<-intervals(log_rich_bio_full_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_bio_full_temp_scale_75ci<-intervals(log_rich_bio_full_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rich_bio_full_temp_scale_df<-data.frame(cbind(log_rich_bio_full_temp_scale_95ci$fixed, log_rich_bio_full_temp_scale_75ci$fixed))
colnames(log_rich_bio_full_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_bio_full_temp_scale_df
#write.csv(log_rich_bio_full_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rich_bio_full_temp_lme_ci_scale.csv")



######get marginal effects to plot BEF lines (Figure 2)
pred_lrich_lbio_temp_int<-ggpredict(log_rich_bio_full_temp, terms = c("lrich[all]", "Treatment", "Year"), ci.lvl=0.95, full.data=FALSE)
pred_lrich_lbio_temp_int
#write.csv(pred_lrich_lbio_temp_int, "scripts/revisions_scripts/output_r2/marginal_effects_lbio_lrich_temp.csv")



#####----------productivity models: with interaction terms--------####
###interaction models
log_rich_prod_full_temp<-lme(lprod~lrich*Year+Treatment*lrich+Structure+lcc, random =~1|Atoll/Island, 
                             correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Knone)
summary(log_rich_prod_full_temp) #model output

##check assumptions: 
plot(log_rich_prod_full_temp) ##looks okay
qqnorm(log_rich_prod_full_temp) ##looks good
plot(log_rich_prod_full_temp, Treatment~resid(.))##looks good
plot(log_rich_prod_full_temp, Year~resid(.))##looks pretty good
plot(log_rich_prod_full_temp, Island~resid(.))##looks pretty good
plot(log_rich_prod_full_temp, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rich_prod_full_temp_95ci<-intervals(log_rich_prod_full_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_75ci<-intervals(log_rich_prod_full_temp,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_df<-data.frame(cbind(log_rich_prod_full_temp_95ci$fixed, log_rich_prod_full_temp_75ci$fixed))
colnames(log_rich_prod_full_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_prod_full_temp_df
#write.csv(log_rich_prod_full_temp_df, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_prod_full_mod_scale_temp<-lme(lprod~scale(lrich)*Year+scale(lrich)*Treatment+scale(Structure)+scale(lcc), 
                                     random =~1|Atoll/Island, 
                                     correlation = corCompSymm(form = ~Year| Atoll/Island),
                                     data=prod_Knone)
summary(log_rich_prod_full_mod_scale_temp) #model output

#save:
log_rich_prod_full_temp_scale_95ci<-intervals(log_rich_prod_full_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_scale_75ci<-intervals(log_rich_prod_full_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_scale_df<-data.frame(cbind(log_rich_prod_full_temp_scale_95ci$fixed, log_rich_prod_full_temp_scale_75ci$fixed))
colnames(log_rich_prod_full_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_prod_full_temp_scale_df
#write.csv(log_rich_prod_full_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_scale.csv")


######get marginal effects to plot BEF lines (Figure 2)
pred_lrich_lprod_temp_int<-ggpredict(log_rich_prod_full_temp, terms = c("lrich[all]", "Treatment", "Year"), ci.lvl=0.95, full.data=FALSE)
pred_lrich_lprod_temp_int
#write.csv(pred_lrich_lprod_temp_int, "scripts/revisions_scripts/output_r2/marginal_effects_lprod_lrich_temp.csv")



#####----------richness models (additive)--------#####
log_rich_add_temp<-lme(lrich~Treatment+Year+Structure+lcc, random =~1|Atoll/Island, 
                        correlation = corCompSymm(form = ~as.numeric(Year) | Atoll/Island), data=chagos_div_2_k)
summary(log_rich_add_temp) #model output

##check assumptions: 
plot(log_rich_add_temp) ##looks good
qqnorm(log_rich_add_temp) ##looks good
plot(log_rich_add_temp, Treatment~resid(.))##looks good
plot(log_rich_add_temp, Year~resid(.))##looks pretty good
plot(log_rich_add_temp, Island~resid(.))##looks pretty good
plot(log_rich_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rich_add_temp) # highest = 1.9


#save estimates and CIs:
log_rich_add_temp_95ci<-intervals(log_rich_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_add_temp_75ci<-intervals(log_rich_add_temp,which="fixed",level=0.75) #get 75% CI
log_rich_add_temp_df<-data.frame(cbind(log_rich_add_temp_95ci$fixed, log_rich_add_temp_75ci$fixed))
colnames(log_rich_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_add_temp_df
#write.csv(log_rich_add_temp_df, "scripts/revisions_scripts/output_r2/log_rich_add_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 1)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_scale_temp<-lme(lrich~Year+Treatment+scale(Structure)+scale(lcc), 
                                      random =~1|Atoll/Island, 
                                      correlation = corCompSymm(form = ~Year| Atoll/Island),
                                      data=chagos_div_2_k)
summary(log_rich_scale_temp) #model output

#save:
log_rich_scale_temp_95ci<-intervals(log_rich_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rich_scale_temp_75ci<-intervals(log_rich_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rich_scale_temp_df<-data.frame(cbind(log_rich_scale_temp_95ci$fixed, log_rich_scale_temp_75ci$fixed))
colnames(log_rich_scale_temp_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_scale_temp_df
#write.csv(log_rich_scale_temp_df, "scripts/revisions_scripts/output_r2/log_rich_temp_lme_ci_scale.csv")



####----------------Re-run all models with estimated richness------------------####

####biomass- with interactions - rarefied####
log_rare_bio_full_temp<-lme(lbio~lrare*Year+Treatment*lrare+Structure+lcc, random =~1|Atoll/Island, 
                            correlation = corCompSymm(form = ~Year| Atoll/Island), data=chagos_div_2_k)
summary(log_rare_bio_full_temp) #model output

##check assumptions: 
plot(log_rare_bio_full_temp) ##looks good
qqnorm(log_rare_bio_full_temp) ##looks good
plot(log_rare_bio_full_temp, Treatment~resid(.))##looks good
plot(log_rare_bio_full_temp, Year~resid(.))##looks pretty good
plot(log_rare_bio_full_temp, Island~resid(.))##looks pretty good
plot(log_rare_bio_full_temp, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rare_bio_full_temp_95ci<-intervals(log_rare_bio_full_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_bio_full_temp_75ci<-intervals(log_rare_bio_full_temp,which="fixed",level=0.75) #get 75% CI
log_rare_bio_full_temp_df<-data.frame(cbind(log_rare_bio_full_temp_95ci$fixed, log_rare_bio_full_temp_75ci$fixed))
colnames(log_rare_bio_full_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_bio_full_temp_df
#write.csv(log_rare_bio_full_temp_df, "scripts/revisions_scripts/output_r2/log_rare_bio_full_temp_lme_ci.csv")


###Run model to get scaled estimates for supplement
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_bio_full_mod_scale_temp<-lme(lbio~scale(lrare)*Year+scale(lrare)*Treatment+scale(Structure)+scale(lcc), 
                                      random =~1|Atoll/Island, 
                                      correlation = corCompSymm(form = ~Year| Atoll/Island),
                                      data=chagos_div_2_k)
summary(log_rare_bio_full_mod_scale_temp) #model output

#save:
log_rare_bio_full_temp_scale_95ci<-intervals(log_rare_bio_full_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_bio_full_temp_scale_75ci<-intervals(log_rare_bio_full_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rare_bio_full_temp_scale_df<-data.frame(cbind(log_rare_bio_full_temp_scale_95ci$fixed, log_rare_bio_full_temp_scale_75ci$fixed))
colnames(log_rare_bio_full_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_bio_full_temp_scale_df
#write.csv(log_rare_bio_full_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rare_bio_full_temp_lme_ci_scale.csv")




#####productivity models - with interaction - rarefied####
###interaction models
log_rare_prod_full_temp<-lme(lprod~lrare*Year+Treatment*lrare+Structure+lcc, random =~1|Atoll/Island, 
                             correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Knone)
summary(log_rare_prod_full_temp) #model output

##check assumptions: 
plot(log_rare_prod_full_temp) ##looks okay
qqnorm(log_rare_prod_full_temp) ##looks good
plot(log_rare_prod_full_temp, Treatment~resid(.))##looks good
plot(log_rare_prod_full_temp, Year~resid(.))##looks pretty good
plot(log_rare_prod_full_temp, Island~resid(.))##looks pretty good
plot(log_rare_prod_full_temp, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rare_prod_full_temp_95ci<-intervals(log_rare_prod_full_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_75ci<-intervals(log_rare_prod_full_temp,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_df<-data.frame(cbind(log_rare_prod_full_temp_95ci$fixed, log_rare_prod_full_temp_75ci$fixed))
colnames(log_rare_prod_full_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_prod_full_temp_df
#write.csv(log_rare_prod_full_temp_df, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci.csv")


###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_prod_full_mod_scale_temp<-lme(lprod~scale(lrare)*Year+scale(lrare)*Treatment+scale(Structure)+scale(lcc), 
                                       random =~1|Atoll/Island, 
                                       correlation = corCompSymm(form = ~Year| Atoll/Island),
                                       data=prod_Knone)
summary(log_rare_prod_full_mod_scale_temp) #model output

#save:
log_rare_prod_full_temp_scale_95ci<-intervals(log_rare_prod_full_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_scale_75ci<-intervals(log_rare_prod_full_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_scale_df<-data.frame(cbind(log_rare_prod_full_temp_scale_95ci$fixed, log_rare_prod_full_temp_scale_75ci$fixed))
colnames(log_rare_prod_full_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_prod_full_temp_scale_df
#write.csv(log_rare_prod_full_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_scale.csv")


#####biomass models - additive - rarefied####
###additive models
log_rare_bio_add_temp<-lme(lbio~lrare+Year+Treatment+Structure+lcc, random =~1|Atoll/Island, 
                           correlation = corCompSymm(form = ~as.numeric(Year) | Atoll/Island), data=chagos_div_2_k)
summary(log_rare_bio_add_temp) #model output

##check assumptions: 
plot(log_rare_bio_add_temp) ##looks good
qqnorm(log_rare_bio_add_temp) ##looks good
plot(log_rare_bio_add_temp, Treatment~resid(.))##looks good
plot(log_rare_bio_add_temp, Year~resid(.))##looks pretty good
plot(log_rare_bio_add_temp, Island~resid(.))##looks pretty good
plot(log_rare_bio_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rare_bio_add_temp) # highest = 1.4


#save estimates and CIs:
log_rare_bio_add_temp_95ci<-intervals(log_rare_bio_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_bio_add_temp_75ci<-intervals(log_rare_bio_add_temp,which="fixed",level=0.75) #get 75% CI
log_rare_bio_add_temp_df<-data.frame(cbind(log_rare_bio_add_temp_95ci$fixed, log_rare_bio_add_temp_75ci$fixed))
colnames(log_rare_bio_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_bio_add_temp_df
#write.csv(log_rare_bio_add_temp_df, "scripts/revisions_scripts/output_r2/log_rare_bio_add_temp_lme_ci.csv")


###Run model to get scaled estimates for supplement
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_bio_add_mod_scale_temp<-lme(lbio~scale(lrare)+Year+Treatment+scale(Structure)+scale(lcc), 
                                     random =~1|Atoll/Island, 
                                     correlation = corCompSymm(form = ~Year| Atoll/Island),
                                     data=chagos_div_2_k)
summary(log_rare_bio_add_mod_scale_temp) #model output

#save:
log_rare_bio_add_temp_scale_95ci<-intervals(log_rare_bio_add_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_bio_add_temp_scale_75ci<-intervals(log_rare_bio_add_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rare_bio_add_temp_scale_df<-data.frame(cbind(log_rare_bio_add_temp_scale_95ci$fixed, log_rare_bio_add_temp_scale_75ci$fixed))
colnames(log_rare_bio_add_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_bio_add_temp_scale_df
#write.csv(log_rare_bio_add_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rare_bio_add_temp_lme_ci_scale.csv")




#####productivity models - additive - rarefied####
###additive models
log_rare_prod_add_temp<-lme(lprod~lrare+Year+Treatment+Structure+lcc, random =~1|Atoll/Island,
                            correlation = corCompSymm(form = ~as.numeric(Year) | Atoll/Island), data=prod_Knone)
summary(log_rare_prod_add_temp) #model output

##check assumptions: 
plot(log_rare_prod_add_temp) ##doesn't look great (actually it's similar too before)
qqnorm(log_rare_prod_add_temp) ##looks good
plot(log_rare_prod_add_temp, Treatment~resid(.))##looks good
plot(log_rare_prod_add_temp, Year~resid(.))##looks pretty good
plot(log_rare_prod_add_temp, Island~resid(.))##looks pretty good
plot(log_rare_prod_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rare_prod_add_temp) # highest = 1.45


#save estimates and CIs:
log_rare_prod_add_temp_95ci<-intervals(log_rare_prod_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_add_temp_75ci<-intervals(log_rare_prod_add_temp,which="fixed",level=0.75) #get 75% CI
log_rare_prod_add_temp_df<-data.frame(cbind(log_rare_prod_add_temp_95ci$fixed, log_rare_prod_add_temp_75ci$fixed))
colnames(log_rare_prod_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_prod_add_temp_df
#write.csv(log_rare_prod_add_temp_df, "scripts/revisions_scripts/output_r2/log_rare_prod_add_temp_lme_ci.csv")


###Run model to get scaled estimates for supplement
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_prod_add_mod_scale_temp<-lme(lprod~scale(lrare)+Year+Treatment+scale(Structure)+scale(lcc), 
                                      random =~1|Atoll/Island, 
                                      correlation = corCompSymm(form = ~Year| Atoll/Island),
                                      data=prod_Knone)
summary(log_rare_prod_add_mod_scale_temp) #model output

#save:
log_rare_prod_add_temp_scale_95ci<-intervals(log_rare_prod_add_mod_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_add_temp_scale_75ci<-intervals(log_rare_prod_add_mod_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rare_prod_add_temp_scale_df<-data.frame(cbind(log_rare_prod_add_temp_scale_95ci$fixed, log_rare_prod_add_temp_scale_75ci$fixed))
colnames(log_rare_prod_add_temp_scale_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_prod_add_temp_scale_df
#write.csv(log_rare_prod_add_temp_scale_df, "scripts/revisions_scripts/output_r2/log_rare_prod_add_temp_lme_ci_scale.csv")


#####richness models - additive - rarefied####
log_rare_add_temp<-lme(lrare~Treatment+Year+Structure+lcc, random =~1|Atoll/Island, 
                       correlation = corCompSymm(form = ~as.numeric(Year) | Atoll/Island), data=chagos_div_2_k)
summary(log_rare_add_temp) #model output

##check assumptions: 
plot(log_rare_add_temp) ##looks good
qqnorm(log_rare_add_temp) ##looks good
plot(log_rare_add_temp, Treatment~resid(.))##looks good
plot(log_rare_add_temp, Year~resid(.))##looks pretty good
plot(log_rare_add_temp, Island~resid(.))##looks pretty good
plot(log_rare_add_temp, Atoll~resid(.))##looks pretty good
vif(log_rare_add_temp) # highest = 1.38


#save estimates and CIs:
log_rare_add_temp_95ci<-intervals(log_rare_add_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_add_temp_75ci<-intervals(log_rare_add_temp,which="fixed",level=0.75) #get 75% CI
log_rare_add_temp_df<-data.frame(cbind(log_rare_add_temp_95ci$fixed, log_rare_add_temp_75ci$fixed))
colnames(log_rare_add_temp_df)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_add_temp_df
#write.csv(log_rare_add_temp_df, "scripts/revisions_scripts/output_r2/log_rare_add_temp_lme_ci.csv")


###Run model to get scaled estimates for supplment
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_scale_temp<-lme(lrare~Year+Treatment+scale(Structure)+scale(lcc), 
                         random =~1|Atoll/Island, 
                         correlation = corCompSymm(form = ~Year| Atoll/Island),
                         data=chagos_div_2_k)
summary(log_rare_scale_temp) #model output

#save:
log_rare_scale_temp_95ci<-intervals(log_rare_scale_temp, which="fixed", level = 0.95) # get 95% CI
log_rare_scale_temp_75ci<-intervals(log_rare_scale_temp,which="fixed",level=0.75) #get 75% CI
log_rare_scale_temp_df<-data.frame(cbind(log_rare_scale_temp_95ci$fixed, log_rare_scale_temp_75ci$fixed))
colnames(log_rare_scale_temp_df)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_scale_temp_df
#write.csv(log_rare_scale_temp_df, "scripts/revisions_scripts/output_r2/log_rare_temp_lme_ci_scale.csv")




####----------------re-run productivity models with different Kmax estimates------------------####
#check data
str(prod_Knone)
str(prod_Kmin)
str(prod_Kmean)
str(prod_Kmax)
max(prod_Knone$lprod)
max(prod_Kmin$lprod)
max(prod_Kmean$lprod)
max(prod_Kmax$lprod)


####re-run full models and save outputs for Supp Fig 2 - Observed Rich####
#MIN KMAX
log_rich_prod_full_temp_min<-lme(lprod~lrich*Year+Treatment*lrich+Structure+lcc, random =~1|Atoll/Island, 
                             correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmin)
summary(log_rich_prod_full_temp_min) #model output

##check assumptions: 
plot(log_rich_prod_full_temp_min) ##looks okay
qqnorm(log_rich_prod_full_temp_min) ##looks good
plot(log_rich_prod_full_temp_min, Treatment~resid(.))##looks good
plot(log_rich_prod_full_temp_min, Year~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_min, Island~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_min, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rich_prod_full_temp_95ci_min<-intervals(log_rich_prod_full_temp_min, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_75ci_min<-intervals(log_rich_prod_full_temp_min,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_df_min<-data.frame(cbind(log_rich_prod_full_temp_95ci_min$fixed, log_rich_prod_full_temp_75ci_min$fixed))
colnames(log_rich_prod_full_temp_df_min)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_prod_full_temp_df_min
#write.csv(log_rich_prod_full_temp_df_min, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_min.csv")



#mean KMAX
log_rich_prod_full_temp_mean<-lme(lprod~lrich*Year+Treatment*lrich+Structure+lcc, random =~1|Atoll/Island, 
                                 correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmean)
summary(log_rich_prod_full_temp_mean) #model output

##check assumptions: 
plot(log_rich_prod_full_temp_mean) ##looks okay
qqnorm(log_rich_prod_full_temp_mean) ##looks good
plot(log_rich_prod_full_temp_mean, Treatment~resid(.))##looks good
plot(log_rich_prod_full_temp_mean, Year~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_mean, Island~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_mean, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rich_prod_full_temp_95ci_mean<-intervals(log_rich_prod_full_temp_mean, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_75ci_mean<-intervals(log_rich_prod_full_temp_mean,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_df_mean<-data.frame(cbind(log_rich_prod_full_temp_95ci_mean$fixed, log_rich_prod_full_temp_75ci_mean$fixed))
colnames(log_rich_prod_full_temp_df_mean)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_prod_full_temp_df_mean
#write.csv(log_rich_prod_full_temp_df_mean, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_mean.csv")


#max KMAX
log_rich_prod_full_temp_max<-lme(lprod~lrich*Year+Treatment*lrich+Structure+lcc, random =~1|Atoll/Island, 
                                 correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmax)
summary(log_rich_prod_full_temp_max) #model output

##check assumptions: 
plot(log_rich_prod_full_temp_max) ##looks okay
qqnorm(log_rich_prod_full_temp_max) ##looks good
plot(log_rich_prod_full_temp_max, Treatment~resid(.))##looks good
plot(log_rich_prod_full_temp_max, Year~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_max, Island~resid(.))##looks pretty good
plot(log_rich_prod_full_temp_max, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rich_prod_full_temp_95ci_max<-intervals(log_rich_prod_full_temp_max, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_75ci_max<-intervals(log_rich_prod_full_temp_max,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_df_max<-data.frame(cbind(log_rich_prod_full_temp_95ci_max$fixed, log_rich_prod_full_temp_75ci_max$fixed))
colnames(log_rich_prod_full_temp_df_max)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rich_prod_full_temp_df_max
#write.csv(log_rich_prod_full_temp_df_max, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_max.csv")




####re-run full models and save outputs for Supp Fig 2 - Rarefied Rich####
#MIN KMAX
log_rare_prod_full_temp_min<-lme(lprod~lrare*Year+Treatment*lrare+Structure+lcc, random =~1|Atoll/Island, 
                                 correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmin)
summary(log_rare_prod_full_temp_min) #model output

##check assumptions: 
plot(log_rare_prod_full_temp_min) ##looks okay
qqnorm(log_rare_prod_full_temp_min) ##looks good
plot(log_rare_prod_full_temp_min, Treatment~resid(.))##looks good
plot(log_rare_prod_full_temp_min, Year~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_min, Island~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_min, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rare_prod_full_temp_95ci_min<-intervals(log_rare_prod_full_temp_min, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_75ci_min<-intervals(log_rare_prod_full_temp_min,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_df_min<-data.frame(cbind(log_rare_prod_full_temp_95ci_min$fixed, log_rare_prod_full_temp_75ci_min$fixed))
colnames(log_rare_prod_full_temp_df_min)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_prod_full_temp_df_min
#write.csv(log_rare_prod_full_temp_df_min, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_min.csv")



#mean KMAX
log_rare_prod_full_temp_mean<-lme(lprod~lrare*Year+Treatment*lrare+Structure+lcc, random =~1|Atoll/Island, 
                                  correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmean)
summary(log_rare_prod_full_temp_mean) #model output

##check assumptions: 
plot(log_rare_prod_full_temp_mean) ##looks okay
qqnorm(log_rare_prod_full_temp_mean) ##looks good
plot(log_rare_prod_full_temp_mean, Treatment~resid(.))##looks good
plot(log_rare_prod_full_temp_mean, Year~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_mean, Island~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_mean, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rare_prod_full_temp_95ci_mean<-intervals(log_rare_prod_full_temp_mean, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_75ci_mean<-intervals(log_rare_prod_full_temp_mean,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_df_mean<-data.frame(cbind(log_rare_prod_full_temp_95ci_mean$fixed, log_rare_prod_full_temp_75ci_mean$fixed))
colnames(log_rare_prod_full_temp_df_mean)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_prod_full_temp_df_mean
#write.csv(log_rare_prod_full_temp_df_mean, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_mean.csv")


#max KMAX
log_rare_prod_full_temp_max<-lme(lprod~lrare*Year+Treatment*lrare+Structure+lcc, random =~1|Atoll/Island, 
                                 correlation = corCompSymm(form =  ~as.numeric(Year)| Atoll/Island), data=prod_Kmax)
summary(log_rare_prod_full_temp_max) #model output

##check assumptions: 
plot(log_rare_prod_full_temp_max) ##looks okay
qqnorm(log_rare_prod_full_temp_max) ##looks good
plot(log_rare_prod_full_temp_max, Treatment~resid(.))##looks good
plot(log_rare_prod_full_temp_max, Year~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_max, Island~resid(.))##looks pretty good
plot(log_rare_prod_full_temp_max, Atoll~resid(.))##looks pretty good


#save estimates and CIs:
log_rare_prod_full_temp_95ci_max<-intervals(log_rare_prod_full_temp_max, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_75ci_max<-intervals(log_rare_prod_full_temp_max,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_df_max<-data.frame(cbind(log_rare_prod_full_temp_95ci_max$fixed, log_rare_prod_full_temp_75ci_max$fixed))
colnames(log_rare_prod_full_temp_df_max)<-c('ll_95', 'est', 'ul_95', 'll_75', 'est2', 'ul_75')
log_rare_prod_full_temp_df_max
#write.csv(log_rare_prod_full_temp_df_max, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_max.csv")


####different Kmax, scaled estimates for plots####

###MIN KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_prod_full_mod_scale_temp_min<-lme(lprod~scale(lrich)*Year+scale(lrich)*Treatment+scale(Structure)+scale(lcc), 
                                       random =~1|Atoll/Island, 
                                       correlation = corCompSymm(form = ~Year| Atoll/Island),
                                       data=prod_Kmin)
summary(log_rich_prod_full_mod_scale_temp_min) #model output

#save:
log_rich_prod_full_temp_scale_95ci_min<-intervals(log_rich_prod_full_mod_scale_temp_min, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_scale_75ci_min<-intervals(log_rich_prod_full_mod_scale_temp_min,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_scale_df_min<-data.frame(cbind(log_rich_prod_full_temp_scale_95ci_min$fixed, log_rich_prod_full_temp_scale_75ci_min$fixed))
colnames(log_rich_prod_full_temp_scale_df_min)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_prod_full_temp_scale_df_min
#write.csv(log_rich_prod_full_temp_scale_df_min, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_scale_min.csv")

###mean KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_prod_full_mod_scale_temp_mean<-lme(lprod~scale(lrich)*Year+scale(lrich)*Treatment+scale(Structure)+scale(lcc), 
                                           random =~1|Atoll/Island, 
                                           correlation = corCompSymm(form = ~Year| Atoll/Island),
                                           data=prod_Kmean)
summary(log_rich_prod_full_mod_scale_temp_mean) #model output

#save:
log_rich_prod_full_temp_scale_95ci_mean<-intervals(log_rich_prod_full_mod_scale_temp_mean, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_scale_75ci_mean<-intervals(log_rich_prod_full_mod_scale_temp_mean,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_scale_df_mean<-data.frame(cbind(log_rich_prod_full_temp_scale_95ci_mean$fixed, log_rich_prod_full_temp_scale_75ci_mean$fixed))
colnames(log_rich_prod_full_temp_scale_df_mean)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_prod_full_temp_scale_df_mean
#write.csv(log_rich_prod_full_temp_scale_df_mean, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_scale_mean.csv")


###max KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rich_prod_full_mod_scale_temp_max<-lme(lprod~scale(lrich)*Year+scale(lrich)*Treatment+scale(Structure)+scale(lcc), 
                                           random =~1|Atoll/Island, 
                                           correlation = corCompSymm(form = ~Year| Atoll/Island),
                                           data=prod_Kmax)
summary(log_rich_prod_full_mod_scale_temp_max) #model output

#save:
log_rich_prod_full_temp_scale_95ci_max<-intervals(log_rich_prod_full_mod_scale_temp_max, which="fixed", level = 0.95) # get 95% CI
log_rich_prod_full_temp_scale_75ci_max<-intervals(log_rich_prod_full_mod_scale_temp_max,which="fixed",level=0.75) #get 75% CI
log_rich_prod_full_temp_scale_df_max<-data.frame(cbind(log_rich_prod_full_temp_scale_95ci_max$fixed, log_rich_prod_full_temp_scale_75ci_max$fixed))
colnames(log_rich_prod_full_temp_scale_df_max)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rich_prod_full_temp_scale_df_max
#write.csv(log_rich_prod_full_temp_scale_df_max, "scripts/revisions_scripts/output_r2/log_rich_prod_full_temp_lme_ci_scale_max.csv")



####different Kmax, scaled estimates for plots - rarefied####

###MIN KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_prod_full_mod_scale_temp_min<-lme(lprod~scale(lrare)*Year+scale(lrare)*Treatment+scale(Structure)+scale(lcc), 
                                           random =~1|Atoll/Island, 
                                           correlation = corCompSymm(form = ~Year| Atoll/Island),
                                           data=prod_Kmin)
summary(log_rare_prod_full_mod_scale_temp_min) #model output

#save:
log_rare_prod_full_temp_scale_95ci_min<-intervals(log_rare_prod_full_mod_scale_temp_min, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_scale_75ci_min<-intervals(log_rare_prod_full_mod_scale_temp_min,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_scale_df_min<-data.frame(cbind(log_rare_prod_full_temp_scale_95ci_min$fixed, log_rare_prod_full_temp_scale_75ci_min$fixed))
colnames(log_rare_prod_full_temp_scale_df_min)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_prod_full_temp_scale_df_min
#write.csv(log_rare_prod_full_temp_scale_df_min, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_scale_min.csv")

###mean KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_prod_full_mod_scale_temp_mean<-lme(lprod~scale(lrare)*Year+scale(lrare)*Treatment+scale(Structure)+scale(lcc), 
                                            random =~1|Atoll/Island, 
                                            correlation = corCompSymm(form = ~Year| Atoll/Island),
                                            data=prod_Kmean)
summary(log_rare_prod_full_mod_scale_temp_mean) #model output

#save:
log_rare_prod_full_temp_scale_95ci_mean<-intervals(log_rare_prod_full_mod_scale_temp_mean, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_scale_75ci_mean<-intervals(log_rare_prod_full_mod_scale_temp_mean,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_scale_df_mean<-data.frame(cbind(log_rare_prod_full_temp_scale_95ci_mean$fixed, log_rare_prod_full_temp_scale_75ci_mean$fixed))
colnames(log_rare_prod_full_temp_scale_df_mean)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_prod_full_temp_scale_df_mean
#write.csv(log_rare_prod_full_temp_scale_df_mean, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_scale_mean.csv")


###max KMAX
###Run model to get scaled estimates for plot (Figure 2)
##(This way can compare magnitude of effects across explanatory variables with different ranges)
log_rare_prod_full_mod_scale_temp_max<-lme(lprod~scale(lrare)*Year+scale(lrare)*Treatment+scale(Structure)+scale(lcc), 
                                           random =~1|Atoll/Island, 
                                           correlation = corCompSymm(form = ~Year| Atoll/Island),
                                           data=prod_Kmax)
summary(log_rare_prod_full_mod_scale_temp_max) #model output

#save:
log_rare_prod_full_temp_scale_95ci_max<-intervals(log_rare_prod_full_mod_scale_temp_max, which="fixed", level = 0.95) # get 95% CI
log_rare_prod_full_temp_scale_75ci_max<-intervals(log_rare_prod_full_mod_scale_temp_max,which="fixed",level=0.75) #get 75% CI
log_rare_prod_full_temp_scale_df_max<-data.frame(cbind(log_rare_prod_full_temp_scale_95ci_max$fixed, log_rare_prod_full_temp_scale_75ci_max$fixed))
colnames(log_rare_prod_full_temp_scale_df_max)<-c('ll_95_scale', 'est_scale', 'ul_95_scale', 'll_75_scale', 'est2_scale', 'ul_75_scale')
log_rare_prod_full_temp_scale_df_max
#write.csv(log_rare_prod_full_temp_scale_df_max, "scripts/revisions_scripts/output_r2/log_rare_prod_full_temp_lme_ci_scale_max.csv")



####------------STRUCTURAL EQUATION MODELS - biomass - (actual richness)--------------####

##define the path:
log_rich_bio_path_1 <-'
lbio ~ lrich + Year +Treatment + Structure + lcc 
lrich ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'

#model
log_bio_fit_1 <- cfa(log_rich_bio_path_1, data = chagos_div_2_k)
summary(log_bio_fit_1, fit.measures = TRUE, standardized=T,rsquare=T) ##diagnostics look good
varTable(log_bio_fit_1)


#extract standardized path coefficients for tables
lbio_path_est<-parameterEstimates(log_bio_fit_1, standardized=T) #get point estimates 
#write.csv(lbio_path_est, "scripts/revisions_scripts/output/lbio_path_est.csv") #save



####------------STRUCTURAL EQUATION MODELS - biomass -(rarefied richness)--------------####

##define the path:
log_rare_bio_path_1 <-'
lbio ~ lrare + Year +Treatment + Structure + lcc 
lrare ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'

log_bio_rare_fit_1 <- cfa(log_rare_bio_path_1, data = chagos_div_2_k)
summary(log_bio_rare_fit_1, fit.measures = TRUE, standardized=T,rsquare=T) ##diagnostics look good
varTable(log_bio_rare_fit_1)


#extract standardized path coefficients for tables
lbio_lrare_path_est<-parameterEstimates(log_bio_rare_fit_1, standardized=T) #get point estimates 
#write.csv(lbio_lrare_path_est, "scripts/revisions_scripts/output/lbio_lrare_path_est.csv") #save

####------------STRUCTURAL EQUATION MODELS - productivity - (actual richness)--------------####

##define the path:
log_rich_prod_path_1 <-'
lprod ~ lrich + Year +Treatment + Structure + lcc 
lrich ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'

##a. no Kmax difference
#model
log_rich_fit_none <- cfa(log_rich_prod_path_1, data = prod_Knone)
summary(log_rich_fit_none, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_none)

#extract standardized path coefficients for tables/plots
lprod_path_none<-parameterEstimates(log_rich_fit_none, standardized=T) #get point estimates 
#write.csv(lprod_path_none, "scripts/revisions_scripts/output/lprod_path_none.csv") #save

##b. min Kmax difference
#model
log_rich_fit_min <- cfa(log_rich_prod_path_1, data = prod_Kmin)
summary(log_rich_fit_min, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_min)

#extract standardized path coefficients for tables/plots
lprod_path_min<-parameterEstimates(log_rich_fit_min, standardized=T) #get point estimates 
#write.csv(lprod_path_min, "scripts/revisions_scripts/output/lprod_path_min.csv") #save


##c. mean Kmax difference
#model
log_rich_fit_mean <- cfa(log_rich_prod_path_1, data = prod_Kmean)
summary(log_rich_fit_mean, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_mean)

#extract standardized path coefficients for tables/plots
lprod_path_mean<-parameterEstimates(log_rich_fit_mean, standardized=T) #get point estimates 
#write.csv(lprod_path_mean, "scripts/revisions_scripts/output/lprod_path_mean.csv") #save


##d. max Kmax difference
#model
log_rich_fit_max <- cfa(log_rich_prod_path_1, data = prod_Kmax)
summary(log_rich_fit_max, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_max)

#extract standardized path coefficients for tables/plots
lprod_path_max<-parameterEstimates(log_rich_fit_max, standardized=T) #get point estimates 
#write.csv(lprod_path_max, "scripts/revisions_scripts/output/lprod_path_max.csv") #save




####------------STRUCTURAL EQUATION MODELS - productivity - (rarefied richness)--------------####

##define the path:
log_rare_prod_path_1 <-'
lprod ~ lrare + Year +Treatment + Structure + lcc 
lrare ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'

##a. no difference
lprod_lrare_fit_none <- cfa(log_rare_prod_path_1, data = prod_Knone)
summary(log_rich_fit_none, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_none)

lprod_lrare_path_none<-parameterEstimates(lprod_lrare_fit_none, standardized=T) #get point estimates 
#write.csv(lprod_lrare_path_none, "scripts/revisions_scripts/output/lprod_lrare_path_none.csv") #save


##b. min diff
lprod_lrare_fit_min <- cfa(log_rare_prod_path_1, data = prod_Kmin)
summary(log_rich_fit_min, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_min)

lprod_lrare_path_min<-parameterEstimates(lprod_lrare_fit_min, standardized=T) #get point estimates 
#write.csv(lprod_lrare_path_min, "scripts/revisions_scripts/output/lprod_lrare_path_min.csv") #save



##c. mean diff
lprod_lrare_fit_mean <- cfa(log_rare_prod_path_1, data = prod_Kmean)
summary(log_rich_fit_mean, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_mean)

lprod_lrare_path_mean<-parameterEstimates(lprod_lrare_fit_mean, standardized=T) #get point estimates 
#write.csv(lprod_lrare_path_mean, "scripts/revisions_scripts/output/lprod_lrare_path_mean.csv") #save


##d. max diff
lprod_lrare_fit_max <- cfa(log_rare_prod_path_1, data = prod_Kmax)
summary(log_rich_fit_max, fit.measures = TRUE, standardized=T,rsquare=T) ##model fits look good
varTable(log_rich_fit_max)

lprod_lrare_path_max<-parameterEstimates(lprod_lrare_fit_max, standardized=T) #get point estimates 
#write.csv(lprod_lrare_path_max, "scripts/revisions_scripts/output/lprod_lrare_path_max.csv") #save

