############-------------------FIGURES FOR SUPPLEMENT------------------------########################


####-------------------load packages--------------------####
library(ggplot2)
library(RColorBrewer)
library(gridExtra)


#####-----------------SUPPLEMENTAL FIGURE 1: COEFFICIENT PLOT (SCALED) COMPARING OBSERVED VS. ESTIMATED RICHNESS (RESPONSE = BIOMASS)---------------####
####Interaction model - scaled estimates for non-interaction terms similar to additive model, so fine to just use this...
####load back in data####
##biomass model - regular richness
bio_coef_int_scale<-read.csv("model_outputs/log_rich_bio_full_temp_lme_ci_scale.csv")
bio_coef_int_scale

bio_coef_int_rare_scale<-read.csv("model_outputs/log_rare_bio_full_temp_lme_ci_scale.csv")
bio_coef_int_rare_scale

##combine files
##first int column for model
bio_coef_int_scale$model<-as.factor("observed")
bio_coef_int_rare_scale$model<-as.factor("estimated")


##combine rows
supp_fig1_int_dat<-rbind(bio_coef_int_scale, bio_coef_int_rare_scale)
supp_fig1_int_dat

##rename levels of X to make plotting easier
supp_fig1_int_dat$X<-revalue(supp_fig1_int_dat$X, c("(Intercept)"="Int", "scale(lcc)"="coral cover","scale(Structure)"="structural complexity", "scale(lrich)"="richness", "scale(lrare)"="richness", "TreatmentRats"="invasive rats", "Year2018"="climate extreme", "scale(lrich):Year2018" = "richness x climate extreme",  "scale(lrich):TreatmentRats" = "richness x invasive rats", "scale(lrare):Year2018" = "richness x climate extreme",  "scale(lrare):TreatmentRats" = "richness x invasive rats" )) #from library(plyr)
levels(supp_fig1_int_dat$X)


#re-order
supp_fig1_int_dat$X<-factor(supp_fig1_int_dat$X, levels=c("coral cover","structural complexity", "climate extreme", "invasive rats", "richness", "richness x climate extreme", "richness x invasive rats",  "Int"))
supp_fig1_int_dat$model<-factor(supp_fig1_int_dat$model, levels=c("estimated", "observed"))

supp_fig1_int_dat


####create supplemental figure 1B####
supp_figure1<-
  ggplot(supp_fig1_int_dat[supp_fig1_int_dat$X!="Int"&supp_fig1_int_dat$X!="structural complexity"&supp_fig1_int_dat$X!="coral cover",], aes(x=X, y=est_scale, color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x = X, 
                     ymax = ul_95_scale),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale),
                 lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c( "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288"))+
  scale_fill_manual(values=c( "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE", "#332288"))+
  scale_y_continuous(limits=c(-1.0, 1.0), breaks=seq(-1.0,1.0,.5))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
supp_figure1


#ggsave(file="figures/supp_figure_1_biomass_2020_03_03.eps", width = 10, height = 7, units = "in", supp_figure1)



#####-----------------SUPPLEMENTAL FIGURE 2: COEFFICIENT PLOT (SCALED) COMPARING OBSERVED VS. ESTIMATED RICHNESS AND DIFF K ESTIMATES (RESPONSE = PRODUCTIVITY)---------------####
####Panel a: Interaction model (observed richness)

#no Kmax diff
prod_coef_int_scale_Knone<-read.csv("model_outputs/log_rich_prod_full_temp_lme_ci_scale.csv")
prod_coef_int_scale_Knone

prod_coef_int_scale_rare_Knone<-read.csv("model_outputs/log_rare_prod_full_temp_lme_ci_scale.csv")
prod_coef_int_scale_rare_Knone

#min Kmax diff
prod_coef_int_scale_Kmin<-read.csv("model_outputs/log_rich_prod_full_temp_lme_ci_scale_min.csv")
prod_coef_int_scale_Kmin

prod_coef_int_scale_rare_Kmin<-read.csv("model_outputs/log_rare_prod_full_temp_lme_ci_scale_min.csv")
prod_coef_int_scale_rare_Kmin

#mean Kmax diff
prod_coef_int_scale_Kmean<-read.csv("model_outputs/log_rich_prod_full_temp_lme_ci_scale_mean.csv")
prod_coef_int_scale_Kmean

prod_coef_int_scale_rare_Kmean<-read.csv("model_outputs/log_rare_prod_full_temp_lme_ci_scale_mean.csv")
prod_coef_int_scale_rare_Kmean

#max Kmax diff
prod_coef_int_scale_Kmax<-read.csv("model_outputs/log_rich_prod_full_temp_lme_ci_scale_max.csv")
prod_coef_int_scale_Kmax

prod_coef_int_scale_rare_Kmax<-read.csv("model_outputs/log_rare_prod_full_temp_lme_ci_scale_max.csv")
prod_coef_int_scale_rare_Kmax


##combine files
##first insert column for model types (observed vs. estimated)
prod_coef_int_scale_Knone$model<-as.factor("observed")
prod_coef_int_scale_rare_Knone$model<-as.factor("estimated")

prod_coef_int_scale_Kmin$model<-as.factor("observed")
prod_coef_int_scale_rare_Kmin$model<-as.factor("estimated")

prod_coef_int_scale_Kmean$model<-as.factor("observed")
prod_coef_int_scale_rare_Kmean$model<-as.factor("estimated")

prod_coef_int_scale_Kmax$model<-as.factor("observed")
prod_coef_int_scale_rare_Kmax$model<-as.factor("estimated")

##and insert column for Kmax types
prod_coef_int_scale_Knone$Kdiff<-as.factor("none")
prod_coef_int_scale_rare_Knone$Kdiff<-as.factor("none")

prod_coef_int_scale_Kmin$Kdiff<-as.factor("min")
prod_coef_int_scale_rare_Kmin$Kdiff<-as.factor("min")

prod_coef_int_scale_Kmean$Kdiff<-as.factor("mean")
prod_coef_int_scale_rare_Kmean$Kdiff<-as.factor("mean")

prod_coef_int_scale_Kmax$Kdiff<-as.factor("max")
prod_coef_int_scale_rare_Kmax$Kdiff<-as.factor("max")



##combine rows
supp_fig2_int_dat<-rbind(prod_coef_int_scale_Knone, prod_coef_int_scale_rare_Knone, 
                         prod_coef_int_scale_Kmin, prod_coef_int_scale_rare_Kmin,
                         prod_coef_int_scale_Kmean, prod_coef_int_scale_rare_Kmean,
                         prod_coef_int_scale_Kmax, prod_coef_int_scale_rare_Kmax)
supp_fig2_int_dat




##rename levels of X to make plotting easier
supp_fig2_int_dat$X<-revalue(supp_fig2_int_dat$X, c("(Intercept)"="Int", "scale(lcc)"="coral cover","scale(Structure)"="structural complexity", "scale(lrich)"="richness", "scale(lrare)"="richness", "TreatmentRats"="invasive rats", "Year2018"="climate extreme", "scale(lrich):Year2018" = "richness x climate extreme",  "scale(lrich):TreatmentRats" = "richness x invasive rats", "scale(lrare):Year2018" = "richness x climate extreme",  "scale(lrare):TreatmentRats" = "richness x invasive rats" )) #from library(plyr)
levels(supp_fig2_int_dat$X)


#re-order
supp_fig2_int_dat$X<-factor(supp_fig2_int_dat$X, levels=c("coral cover","structural complexity", "climate extreme", "invasive rats", "richness", "richness x climate extreme", "richness x invasive rats",  "Int"))
supp_fig2_int_dat$model<-factor(supp_fig2_int_dat$model, levels=c("estimated", "observed"))
supp_fig2_int_dat$Kdiff<-factor(supp_fig2_int_dat$Kdiff, levels=c("max", "mean", "min", "none"))

supp_fig2_int_dat


####create supplemental figure 2A####
supp_figure2a<-
  ggplot(supp_fig2_int_dat[supp_fig2_int_dat$model=="observed"& supp_fig2_int_dat$X!="Int"&supp_fig2_int_dat$X!="structural complexity"&supp_fig2_int_dat$X!="coral cover",], aes(x=X, y=est_scale)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x = X, 
                     ymax = ul_95_scale, col = Kdiff),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale, col = Kdiff),
                 lwd=3,position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=Kdiff), size=6, shape=21, color="black",position = position_dodge(width = .5))  +
  scale_colour_manual(values=c("#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288"))+
  scale_fill_manual(values=c("#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288"))+
  scale_y_continuous(limits=c(-1.0, 1.0), breaks=seq(-1.0,1.0,.5))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
supp_figure2a


####create supplemental figure 2B (estimated richness)####
supp_figure2b<-
  ggplot(supp_fig2_int_dat[supp_fig2_int_dat$model=="estimated"& supp_fig2_int_dat$X!="Int"&supp_fig2_int_dat$X!="structural complexity"&supp_fig2_int_dat$X!="coral cover",], aes(x=X, y=est_scale)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x = X, 
                     ymax = ul_95_scale, col = Kdiff),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale, col = Kdiff),
                 lwd=3,position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=Kdiff), size=6, shape=21, color="black",position = position_dodge(width = .5))  +
  scale_colour_manual(values=c("#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288"))+
  scale_fill_manual(values=c("#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288", "#117733", "#44AA99", "#88CCEE", "#332288"))+
   scale_y_continuous(limits=c(-1.0, 1.0), breaks=seq(-1.0,1.0,.5))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
supp_figure2b


supp_figure2ab<-arrangeGrob(supp_figure2a, supp_figure2b, ncol=2)
#ggsave(file="figures/supp_figure2ab_prod_est_2020_03_02.eps", width = 20, height = 10, units = "in", supp_figure2ab)





#####-----------------SUPPLEMENTAL FIGURE 3: COEFFICIENT PLOT (SCALED) COMPARING OBSERVED VS. ESTIMATED RICHNESS (RESPONSE = RICHNESS)---------------####

####additive model####

##regular richness
rich_coef_add_scale<-read.csv("model_outputs/log_rich_temp_lme_ci_scale.csv")
rich_coef_add_scale

##estimate richness
rare_coef_add_scale<-read.csv("model_outputs/log_rare_temp_lme_ci_scale.csv")
rare_coef_add_scale

##combine files
##first add column for model
rich_coef_add_scale$model<-as.factor("observed")
rare_coef_add_scale$model<-as.factor("estimated")


##combine rows
supp_fig3_dat<-rbind(rich_coef_add_scale, rare_coef_add_scale)
supp_fig3_dat


##rename levels of X to make plotting easier
supp_fig3_dat$X<-revalue(supp_fig3_dat$X, c("(Intercept)"="Int", "scale(lcc)"="coral cover","scale(Structure)"="structural complexity", "TreatmentRats"="invasive rats", "Year2018"="climate extreme" )) #from library(plyr)
levels(supp_fig3_dat$X)


#re-order
supp_fig3_dat$X<-factor(supp_fig3_dat$X, levels=c("coral cover","structural complexity", "climate extreme", "invasive rats",   "Int"))
supp_fig3_dat$model<-factor(supp_fig3_dat$model, levels=c("estimated", "observed"))

supp_fig3_dat



####create supplemental figure 3 - reduced####

supp_figure3<-
  ggplot(supp_fig3_dat[supp_fig3_dat$X=="invasive rats"|supp_fig3_dat$X=="climate extreme",], aes(x=X, y=est_scale, color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x = X, 
                     ymax = ul_95_scale),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale),
                 lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c( "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE"))+
  scale_fill_manual(values=c( "#88CCEE", "#332288",  "#88CCEE", "#332288",  "#88CCEE"))+
  
 scale_y_continuous(limits=c(-0.4, 0.4), breaks=seq(-.4,.4,.2))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
supp_figure3

#ggsave(file="figures/coef_obs_est_richness_supp_fig_3_2020_03_03.eps", width = 10, height = 7, units = "in", supp_figure3)
