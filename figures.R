############-------------------FINAL FIGURES FOR PAPER------------------------########################

####-------------------load packages--------------------####
library(RColorBrewer)
library(gridExtra)
library(ggplot2)

#####-------load data------####
#note: same .Rdata files as in analysis file (so if just ran analysis, don't need to re-load all these):

##biomass
load(file='data/Chagos_bio_kmax_rich_2019.Rdata')

#log coral cover
chagos_div_2_k$lcc<-log(chagos_div_2_k$Coral_cover+1) #log coral cover

##add treatment*year column to help plotting
chagos_div_2_k$TreatYear2<-paste(chagos_div_2_k$Year, chagos_div_2_k$Treatment, sep='_')
chagos_div_2_k$TreatYear2<-as.factor(chagos_div_2_k$TreatYear2)


#4 productivity files:
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



#####-----------------FIGURE 1: COEFFICIENT PLOT (SCALED) FROM ADDITIVE MODELS---------------####

####load back in data####
##biomass model
bio_coef_add_scale<-read.csv("model_outputs/log_rich_bio_add_temp_lme_ci_scale.csv")
bio_coef_add_scale

##productivity model (no K diff - for main text)
prod_coef_add_noKdiff_scale<-read.csv("model_outputs/log_rich_prod_add_temp_lme_ci_scale.csv")
prod_coef_add_noKdiff_scale

##richness model
rich_coef_scale<-read.csv("model_outputs/log_rich_temp_lme_ci_scale.csv")
rich_coef_scale

##combine files
##first add column for model
bio_coef_add_scale$model<-as.factor("biomass")
prod_coef_add_noKdiff_scale$model<-as.factor("productivity")
rich_coef_scale$model<-as.factor("richness")

##combine rows
fig1_dat<-rbind(bio_coef_add_scale, prod_coef_add_noKdiff_scale, rich_coef_scale)
fig1_dat

##rename levels of X to make plotting easier
fig1_dat$X<-revalue(fig1_dat$X, c("(Intercept)"="Int", "scale(lcc)"="coral cover","scale(Structure)"="structural complexity", "scale(lrich)"="richness", "TreatmentRats"="invasive rats", "Year2018"="climate extreme" )) #from library(plyr)
levels(fig1_dat$X)


#re-order
fig1_dat$X<-factor(fig1_dat$X, levels=c("coral cover","structural complexity", "climate extreme", "invasive rats", "richness",   "Int"))
fig1_dat$model<-factor(fig1_dat$model, levels=c("richness", "productivity", "biomass"))

fig1_dat


####create figure 1####
figure1<-
  ggplot(fig1_dat[fig1_dat$X!="Int",], aes(x=X, y=est_scale, color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale   , 
                   x = X, 
                   ymax = ul_95_scale),
               lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                   x = X, 
                   ymax = ul_75_scale),
               lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c( "#E78AC3","#8DA0CB",  "#66C2A5"))+
  scale_fill_manual(values=c("#E7298A", "#7570B3",  "#1B9E77"))+
 #  scale_fill_brewer(palette="Dark2")+ 
#  scale_colour_brewer(palette="Set2")+
 # scale_colour_manual(values=c("#332288", "#44AA99", "#AA4499"))+
#  scale_fill_manual(values=c("#332288", "#44AA99", "#AA4499"))+
  scale_y_continuous(limits=c(-0.8, 0.8), breaks=seq(-.8,.8,.4))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
figure1

#ggsave(file="figures/figure1_coef_plot_add_2020_03_02.eps", width = 10, height = 10, units = "in", figure1)


#####-----------------FIGURE 2A,B: BEF RELATIONSHIPS (MARGINAL EFFECTS FOR EACH CLIMATE-RAT COMBINATION FROM INTERACTION MODELS) AND RAW DATA---------------####

#####--------figure 2A: biomass-------####
#import predictions for plotting
pre_lrich_lbio_int_dat<-read.csv("model_outputs/marginal_effects_lbio_lrich_temp.csv")
pre_lrich_lbio_int_dat

#add column for combined treatment and year
pre_lrich_lbio_int_dat$TreatYear<-paste(pre_lrich_lbio_int_dat$facet, pre_lrich_lbio_int_dat$group, sep='_')
pre_lrich_lbio_int_dat$TreatYear<-as.factor(pre_lrich_lbio_int_dat$TreatYear)
pre_lrich_lbio_int_dat


##plot predictions (CIs) plus raw data, restricted data range
##get richness range for each treatment-year combination, to restrict lines to data range: 
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2015_No_rats"]) # 2.639057 3.401197
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2018_No_rats"]) # 1.386294 3.258097
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2015_Rats"]) # 2.079442 3.218876
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2018_Rats"]) # 1.609438 3.044522

##raw data, trimmed, with CIs
figure2a<-
  ggplot(chagos_div_2_k) +
  ##raw data
  geom_point(size=6, aes(x=lrich, y=lbio, colour=TreatYear2, fill = TreatYear2), alpha=0.6) + 
  scale_colour_manual(values=c( "#56B4E9","#E69F00", "#0072B2", "#D55E00"))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00","#0072B2",   "#D55E00"))+
  ##one line per group
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lbio_int_dat$x>=2.63 &pre_lrich_lbio_int_dat$x<=3.41,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#56B4E9" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_Rats"&pre_lrich_lbio_int_dat$x>=2.07 &pre_lrich_lbio_int_dat$x<=3.22,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#E69F00" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lbio_int_dat$x>=1.37 &pre_lrich_lbio_int_dat$x<=3.26,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#0072B2" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_Rats"&pre_lrich_lbio_int_dat$x>=1.60 &pre_lrich_lbio_int_dat$x<=3.03,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#D55E00" ) + 
  ##confidence intervals per group
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lbio_int_dat$x>=2.63 &pre_lrich_lbio_int_dat$x<=3.41,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#56B4E9", alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_Rats"&pre_lrich_lbio_int_dat$x>=2.07 &pre_lrich_lbio_int_dat$x<=3.22,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#E69F00" , alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lbio_int_dat$x>=1.37 &pre_lrich_lbio_int_dat$x<=3.26,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#0072B2" , alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_Rats"&pre_lrich_lbio_int_dat$x>=1.60 &pre_lrich_lbio_int_dat$x<=3.03,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#D55E00", alpha = 0.1, colour=NA ) + 
  ##other formatting
  scale_y_continuous(limits = c(1.5,7.5), breaks=seq(2,7,1))+
  #  scale_x_continuous(limits = c(1.2, 3.4), breaks=seq(1.5,3.0, 0.5))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(),
        text = element_text(size=8))
figure2a


#####--------figure 2B: productivity-------####
#import predictions for plotting
pre_lrich_lprod_int_dat<-read.csv("model_outputs/marginal_effects_lprod_lrich_temp.csv")
pre_lrich_lprod_int_dat

#add column for combined treatment and year
pre_lrich_lprod_int_dat$TreatYear<-paste(pre_lrich_lprod_int_dat$facet, pre_lrich_lprod_int_dat$group, sep='_')
pre_lrich_lprod_int_dat$TreatYear<-as.factor(pre_lrich_lprod_int_dat$TreatYear)
pre_lrich_lprod_int_dat


##plot predictions (CIs) plus raw data, restricted data range
##get richness range for each treatment-year combination, to restrict lines to data range: 
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2015_No_rats"]) # 2.639057 3.401197
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2018_No_rats"]) # 1.386294 3.258097
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2015_Rats"]) # 2.079442 3.218876
range(chagos_div_2_k$lrich[chagos_div_2_k$TreatYear2=="2018_Rats"]) # 1.609438 3.044522


#add column for combined treatment and year in raw data
prod_Knone$TreatYear2<-paste(prod_Knone$Year, prod_Knone$Treatment, sep='_')
prod_Knone$TreatYear2<-as.factor(prod_Knone$TreatYear2)
prod_Knone


##raw data, trimmed, with CIs
figure2b<-
  ggplot(prod_Knone) +
  ##raw data
  geom_point(size=6, aes(x=lrich, y=lprod, colour=TreatYear2, fill = TreatYear2), alpha=0.6) + 
  scale_colour_manual(values=c( "#56B4E9","#E69F00", "#0072B2", "#D55E00"))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00","#0072B2",   "#D55E00"))+
  ##one line per group
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lprod_int_dat$x>=2.63 &pre_lrich_lprod_int_dat$x<=3.41,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#56B4E9" ) + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_Rats"&pre_lrich_lprod_int_dat$x>=2.07 &pre_lrich_lprod_int_dat$x<=3.22,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#E69F00" ) + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lprod_int_dat$x>=1.37 &pre_lrich_lprod_int_dat$x<=3.26,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#0072B2" ) + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_Rats"&pre_lrich_lprod_int_dat$x>=1.60 &pre_lrich_lprod_int_dat$x<=3.03,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#D55E00" ) + 
  ##confidence intervals per group
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lprod_int_dat$x>=2.63 &pre_lrich_lprod_int_dat$x<=3.41,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#56B4E9", alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_Rats"&pre_lrich_lprod_int_dat$x>=2.07 &pre_lrich_lprod_int_dat$x<=3.22,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#E69F00" , alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lprod_int_dat$x>=1.37 &pre_lrich_lprod_int_dat$x<=3.26,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#0072B2" , alpha = 0.1, colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_Rats"&pre_lrich_lprod_int_dat$x>=1.60 &pre_lrich_lprod_int_dat$x<=3.03,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#D55E00", alpha = 0.1, colour=NA ) + 
  ##other formatting
  scale_y_continuous(limits = c(1.5,6.5), breaks=seq(2,6,1))+
  #  scale_x_continuous(limits = c(1.2, 3.4), breaks=seq(1.5,3.0, 0.5))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(),
        text = element_text(size=8))
figure2b


#export figure:
figure2ab<-arrangeGrob(figure2a, figure2b)
#ggsave(file="figures/figure2ab_bef_plot_int_2020_03_02.pdf", width = 10, height = 20, units = "in", figure2ab)

#####re-run without transparency:####
figure2a<-
  ggplot(chagos_div_2_k) +
  ##raw data
  geom_point(size=4, aes(x=lrich, y=lbio, colour=TreatYear2, fill = TreatYear2)) + 
  scale_colour_manual(values=c( "#56B4E9","#E69F00", "#0072B2", "#D55E00"))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00","#0072B2",   "#D55E00"))+
  ##one line per group
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lbio_int_dat$x>=2.63 &pre_lrich_lbio_int_dat$x<=3.41,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#56B4E9" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_Rats"&pre_lrich_lbio_int_dat$x>=2.07 &pre_lrich_lbio_int_dat$x<=3.22,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#E69F00" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lbio_int_dat$x>=1.37 &pre_lrich_lbio_int_dat$x<=3.26,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#0072B2" ) + 
  geom_line(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_Rats"&pre_lrich_lbio_int_dat$x>=1.60 &pre_lrich_lbio_int_dat$x<=3.03,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#D55E00" ) + 
  ##confidence intervals per group
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lbio_int_dat$x>=2.63 &pre_lrich_lbio_int_dat$x<=3.41,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#56B4E9", colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2015_Rats"&pre_lrich_lbio_int_dat$x>=2.07 &pre_lrich_lbio_int_dat$x<=3.22,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#E69F00" ,  colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lbio_int_dat$x>=1.37 &pre_lrich_lbio_int_dat$x<=3.26,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#0072B2" ,  colour=NA) + 
  geom_ribbon(pre_lrich_lbio_int_dat[pre_lrich_lbio_int_dat$TreatYear=="2018_Rats"&pre_lrich_lbio_int_dat$x>=1.60 &pre_lrich_lbio_int_dat$x<=3.03,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#D55E00",  colour=NA) + 
  ##other formatting
  scale_y_continuous(limits = c(1.5,7.5), breaks=seq(2,7,1))+ ##or 1.5 to 7.5
  #  scale_x_continuous(limits = c(1.2, 3.4), breaks=seq(1.5,3.0, 0.5))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(),
        text = element_text(size=8))
figure2a

figure2b<-
  ggplot(prod_Knone) +
  ##raw data
  geom_point(size=4, aes(x=lrich, y=lprod, colour=TreatYear2, fill = TreatYear2)) + 
  scale_colour_manual(values=c( "#56B4E9","#E69F00", "#0072B2", "#D55E00"))+
  scale_fill_manual(values=c("#56B4E9", "#E69F00","#0072B2",   "#D55E00"))+
  ##one line per group
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lprod_int_dat$x>=2.63 &pre_lrich_lprod_int_dat$x<=3.41,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#56B4E9") + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_Rats"&pre_lrich_lprod_int_dat$x>=2.07 &pre_lrich_lprod_int_dat$x<=3.22,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=1, colour = "#E69F00" ) + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lprod_int_dat$x>=1.37 &pre_lrich_lprod_int_dat$x<=3.26,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#0072B2" ) + 
  geom_line(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_Rats"&pre_lrich_lprod_int_dat$x>=1.60 &pre_lrich_lprod_int_dat$x<=3.03,], 
            mapping = aes(x=x, y=predicted), lwd = 2, lty=2, colour = "#D55E00" ) + 
  ##confidence intervals per group
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_No_rats"&pre_lrich_lprod_int_dat$x>=2.63 &pre_lrich_lprod_int_dat$x<=3.41,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#56B4E9",  colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2015_Rats"&pre_lrich_lprod_int_dat$x>=2.07 &pre_lrich_lprod_int_dat$x<=3.22,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#E69F00" ,  colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_No_rats"&pre_lrich_lprod_int_dat$x>=1.37 &pre_lrich_lprod_int_dat$x<=3.26,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#0072B2" ,  colour=NA) + 
  geom_ribbon(pre_lrich_lprod_int_dat[pre_lrich_lprod_int_dat$TreatYear=="2018_Rats"&pre_lrich_lprod_int_dat$x>=1.60 &pre_lrich_lprod_int_dat$x<=3.03,], 
              mapping = aes(x=x, ymin=conf.low, ymax=conf.high,),  fill = "#D55E00",  colour=NA) + 
  ##other formatting
  scale_y_continuous(limits = c(1.5,6.5), breaks=seq(2,7,1))+ # or 1.5 to 7.5
  #  scale_x_continuous(limits = c(1.2, 3.4), breaks=seq(1.5,3.0, 0.5))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(),
        text = element_text(size=8))
figure2b

figure2ab<-arrangeGrob(figure2a, figure2b)
#ggsave(file="figures/figure2ab_bef_plot_int_no_transp_2020_03_02.eps", width = 10, height = 20, units = "in", figure2ab)
#

#####-----------------FIGURE 2C,D: COEFFICIENT PLOT (SCALED) FROM INTERACTION MODELS---------------####

####load back in data####
##biomass model
biomass_scaled_int_dat<-read.csv("model_outputs/log_rich_bio_full_temp_lme_ci_scale.csv")
biomass_scaled_int_dat

##productivity model (no K diff - for main text)
prod_scaled_int_dat<-read.csv("model_outputs/log_rich_prod_full_temp_lme_ci_scale.csv")
prod_scaled_int_dat


##combine rows
##first add column for model
biomass_scaled_int_dat$model<-as.factor("biomass")
prod_scaled_int_dat$model<-as.factor("productivity")

fig2_coef_dat<-rbind(biomass_scaled_int_dat, prod_scaled_int_dat)
fig2_coef_dat

##rename levels of X to make plotting easier
fig2_coef_dat$X<-revalue(fig2_coef_dat$X, c("scale(lrich):TreatmentRats" = "richness x invasive rats", "scale(lrich):Year2018" = "richness x climate extreme")) #from library(plyr)
levels(fig2_coef_dat$X)


#re-order
fig2_coef_dat$X<-factor(fig2_coef_dat$X, levels=c("richness x climate extreme", "richness x invasive rats",  "(Intercept)", "scale(lcc)", "scale(lrich)", "scale(Structure)",   "TreatmentRats", "Year2018"  ))
fig2_coef_dat$model<-factor(fig2_coef_dat$model, levels=c("productivity", "biomass"))

fig2_coef_dat

####Plot for figure 2Cd
figure2cd<-
  ggplot(fig2_coef_dat[fig2_coef_dat$X=="richness x invasive rats"|fig2_coef_dat$X=="richness x climate extreme",],aes(x=X, y=est_scale , color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x =X, 
                     ymax = ul_95_scale),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale),
                 lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c("#8DA0CB",  "#66C2A5"))+
  scale_fill_manual(values=c( "#7570B3",  "#1B9E77"))+
  scale_y_continuous(limits=c(-0.8, 0.8), breaks=seq(-.8,.8,.4))+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
figure2cd

###plot separately: 
#figure 2c
figure2c<-
  ggplot(fig2_coef_dat[fig2_coef_dat$model=="biomass" & fig2_coef_dat$X=="richness x invasive rats"|fig2_coef_dat$model=="biomass" & fig2_coef_dat$X=="richness x climate extreme",],aes(x=X, y=est_scale, color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x =X, 
                     ymax = ul_95_scale),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale),
                 lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c( "#66C2A5"))+
  scale_fill_manual(values=c("#1B9E77"))+
  scale_y_continuous(limits=c(-0.25, 0.52), breaks=seq(-.5,.5,.25))+
  coord_flip()+
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
figure2c


figure2d<-
  ggplot(fig2_coef_dat[fig2_coef_dat$model=="productivity" & fig2_coef_dat$X=="richness x invasive rats"|fig2_coef_dat$model=="productivity" & fig2_coef_dat$X=="richness x climate extreme",],aes(x=X, y=est_scale, color = model)) + 
  geom_hline(yintercept=0, lwd=1, lty=2)+
  geom_linerange(aes(ymin = ll_95_scale, 
                     x =X, 
                     ymax = ul_95_scale),
                 lwd=1.5, position = position_dodge(width = .5))+
  geom_linerange(aes(ymin = ll_75_scale, 
                     x = X, 
                     ymax = ul_75_scale),
                 lwd=3, position = position_dodge(width = .5))+
  # scale_linetype_manual(values=c(2,1,2,1,3))+
  geom_point(stat='identity', aes(fill=model), size=6, shape=21, color="black", position = position_dodge(width = .5))  +
  scale_colour_manual(values=c( "#8DA0CB"))+
  scale_fill_manual(values=c("#7570B3"))+
  scale_y_continuous(limits=c(-0.25, 0.52), breaks=seq(-.5,.5,.25))+
  coord_flip()+
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) # remove gridlines
figure2d

fig2cd<-arrangeGrob(figure2c, figure2d)
#ggsave(file="figures/figure2cd_coef_plot_int_2020_03_02.eps", width = 8, height = 8, units = "in", fig2cd)



#####-----------------FIGURE 3: STRUCTURAL EQUATION MODELS---------------####
##re-run models for plots:

#biomass
##define the path:
log_rich_bio_path_1 <-'
lbio ~ lrich + Year +Treatment + Structure + lcc 
lrich ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'

log_bio_fit_1 <- cfa(log_rich_bio_path_1, data = chagos_div_2_k)
summary(log_bio_fit_1, fit.measures = TRUE, standardized=T,rsquare=T)

#define the label that will go into the nodes
lbls<-c("biomass","richness","structure","coral", "bleaching","rats")
#define the layout
ly<-matrix(c(.9,0,.5,0,0,.25,0,-.25,-.5,.1,-.5,-.1), ncol=2,byrow=TRUE)

#new plot
semPaths(log_bio_fit_1,what="std", layout=ly,residuals=FALSE,nCharNodes=0,sizeMan=8,nodeLabels=lbls,edge.label.cex=.9,curve = TRUE, legend=FALSE, shapeMan="circle", curvePivot = TRUE, posCol="#4DAF4A", negCol="#E41A1C", fade=TRUE, maximum=.81)
##set maximum = 0.81 so can compare with productivity plot (where max path coefficient = .8)

#save plot: 
#semPaths(log_bio_fit_1,what="std", layout=ly,residuals=FALSE,nCharNodes=0,sizeMan=8,nodeLabels=lbls,edge.label.cex=.9,curve = TRUE, legend=FALSE, shapeMan="circle", curvePivot = TRUE, posCol="#4DAF4A", negCol="#E41A1C", fade=TRUE, maximum=.81,
    #     filename="figures/figure3a_biomass_path_2020_01_13", filetype="pdf", width = 12, height=9)
##set maximum = 0.81 so can compare with productivity plot (where max path coefficient = .8)


#productivity (no K diff)
##define the path:
log_rich_prod_path_1 <-'
lprod ~ lrich + Year +Treatment + Structure + lcc 
lrich ~ Treatment + Year  + Structure + lcc 
Structure ~ Year +lcc
lcc ~ Year + Treatment
'
##define layout for plot: 
lbls<-c("productivity","richness","structure","coral", "bleaching","rats") #define the label that will go into the nodes
ly<-matrix(c(.9,0,.5,0,0,.25,0,-.25,-.5,.1,-.5,-.1), ncol=2,byrow=TRUE )#define the layout


##a. no Kmax difference
log_rich_fit_none <- cfa(log_rich_prod_path_1, data = prod_Knone)
semPaths(log_rich_fit_none,what="std", layout=ly,residuals=FALSE,nCharNodes=0,sizeMan=8,nodeLabels=lbls,edge.label.cex=.9,curve = TRUE, legend=FALSE, shapeMan="circle", curvePivot = TRUE, posCol="#4DAF4A", negCol="#E41A1C", fade=TRUE, maximum=.81
#         ,filename="figures/figure3b_prod_path_none_2020_01_13", filetype="pdf", width = 12, height=9
          )
##set maximum = 0.81 so can compare with biomass plot (where max path coeff is lower)


summary(log_rich_fit_none, fit.measures = TRUE, standardized=T,rsquare=T)
