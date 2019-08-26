######DRIFT1A FULL ANALYSES#######
setwd("C:/Users/s02na7/Dropbox/0_UoA/Drift1Full")
setwd("~/Dropbox/0_UoA/Drift1Full")
source('C:/Users/s02na7/Dropbox/functions.R')
source('~/Dropbox/functions.R')
library(tidyverse)
library(lmerTest)
library(ez)
library(multcomp)



######COMPILE DATASET######
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1Full/RTAmpsRaw.RData")
rtamps<-RTAmps
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1Full/RTAmps.RData")
norm_rtamps<-RTAmps%>%mutate(norm_amps = amps)%>%dplyr::select(-c(amps))
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1Full/SWFitsFull.RData")
fits<-a


alldata<-list(rtamps,norm_rtamps, fits)%>%reduce(full_join)%>%
  dplyr::select(Subj, RDK, Attention, amps, norm_amps, RT, gamma, alpha, theta)
head(alldata)
save(alldata, file = 'allDataDriftFull.RData')



######ANALYZE#########
load('allDataDriftFull.RData')


r<-ezANOVA(
  data = alldata
  , dv = gamma
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=F
)
r

r<-ezANOVA(
  data = alldata
  , dv = theta
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=F
)
r

r<-ezANOVA(
  data = alldata
  , dv = alpha
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=F
)
r

r<-ezANOVA(
  data = alldata
  , dv = amps
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=F
)
r

r<-ezANOVA(
  data = alldata
  , dv = RT
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=F
)
r

alphaSum = summary_se_within(alldata, 'alpha', 'Subj', withinvars = c('RDK', 'Attention')) #thres
gammaSum = summary_se_within(alldata, 'gamma', 'Subj', withinvars = c('RDK', 'Attention')) # drift
thetaSum = summary_se_within(alldata, 'theta', 'Subj', withinvars = c('RDK', 'Attention')) #nondec
ampSum = summary_se_within(alldata, 'norm_amps', 'Subj', withinvars = c('RDK', 'Attention')) #ssvep
RTSum = summary_se_within(alldata, 'RT', 'Subj', withinvars = c('RDK', 'Attention')) #meanrt
pos=position_dodge(width=0.15)
plot=ggplot(alphaSum, aes(x=Attention, y=alpha, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Threshold')+
  geom_errorbar(aes(ymin=alpha-CI, ymax=alpha+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

plot=ggplot(gammaSum, aes(x=Attention, y=gamma, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Drift rate')+
  geom_errorbar(aes(ymin=gamma-CI, ymax=gamma+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

plot=ggplot(thetaSum, aes(x=Attention, y=theta, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Shift')+
  geom_errorbar(aes(ymin=theta-CI, ymax=theta+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

plot=ggplot(ampSum, aes(x=Attention, y=amps, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Normalised amplitudes')+
  geom_errorbar(aes(ymin=amps-CI, ymax=amps+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

plot=ggplot(RTSum, aes(x=Attention, y=RT, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Reaction times (ms)')+
  geom_errorbar(aes(ymin=RT-CI, ymax=RT+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

 #fit<-lm(norm_amps~gamma, alldata)
 #summary(fit)
 #ggPredict(fit, colorn = 5, colorAsFactor = F)+theme_classic()+
#   scale_x_continuous(name='Drift rate')+
#   scale_y_continuous(name='Normalised amplitudes')
 

DataAll<-alldata


RTDiffs<-DataAll%>%
  dplyr::select(RDK, Subj, RT, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, RT)%>%
  mutate(RTenhance=attended-neutral, 
         RTsuppress=neutral-unattended,
         RTrange=attended-unattended)%>%
  dplyr::select(RDK, Subj, RTenhance, RTsuppress, RTrange)

AmpDiffs<-DataAll%>%
  dplyr::select(RDK, Subj, amps, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, amps)%>%
  mutate(Ampenhance=attended-neutral, 
         Ampsuppress=neutral-unattended,
         Amprange=attended-unattended)%>%
  dplyr::select(RDK, Subj,Ampenhance, Ampsuppress, Amprange)

GammaDiffs<-DataAll%>%
  dplyr::select(RDK, Subj, gamma, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, gamma)%>%
  mutate(Driftenhance=attended-neutral, 
         Driftsuppress=neutral-unattended,
         Driftrange=attended-unattended)%>%
  dplyr::select(RDK, Subj,Driftenhance, Driftsuppress, Driftrange)

ThetaDiffs<-DataAll%>%
  dplyr::select(RDK, Subj, theta, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, theta)%>%
  mutate(Shiftenhance=attended-neutral, 
         Shiftsuppress=neutral-unattended,
         Shiftrange=attended-unattended)%>%
  dplyr::select(RDK, Subj,Shiftenhance, Shiftsuppress, Shiftrange)


Diffs<-list(RTDiffs, AmpDiffs, GammaDiffs, ThetaDiffs)%>%reduce(full_join)
head(Diffs)

ggplot(Diffs, aes(x=Ampenhance, y=RTenhance, col=RDK))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+theme_classic()+
  ggtitle('attended - neutral')

ggplot(Diffs, aes(x=Ampsuppress, y=RTsuppress, col=RDK))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+theme_classic()+
  ggtitle('neutral - unattended')


subselection = levels(Diffs$Subj)
#subselection = subselection [-c( 2, 18, 26, 30, 31)]
  Diffs%>%
  filter(Subj %in% subselection)%>%
  ggplot(aes(x=Amprange, y=RTrange, color = RDK))+
  geom_point(size = 3)+
  geom_smooth(method='lm',formula=y~x, se=FALSE, inherit.aes = FALSE, aes(x=Amprange, y=RTrange))+
  theme_classic()+
  theme(aspect.ratio = 0.5)+
  ylim(c(-150, 50))+
  xlim(c(-0.25, 1.25))+
  ggtitle('attended - unattended')+
  ylab(label='RT attended - unattended (ms)')+
  xlab(label='SSVEP attended - unattended (a.u.)')


cor.test(Diffs$Amprange,Diffs$RTrange)

mod1<-lmer(Amprange ~ RDK + (1|Subj), data = Diffs)
summary(mod1)
mod2<-lmer(Amprange ~ RDK + RTrange + (1|Subj), data = Diffs)
summary(mod2)
anova(mod1, mod2)

mod <- lm(Amprange ~ RDK + RTrange, data = Diffs)
summary(mod)

library(MuMIn)
r.squaredGLMM(mod2)

library(lmerTest)
d<-Diffs
fit <- lmer(Amprange ~ RTrange + (1|Subj), data=d)
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values
plot(d$residuals)

redSum = summary_se_within(d, 'residuals', 'Subj', withinvars = c('RDK', 'Attention')) #res

plot=ggplot(redSum, aes(x=Attention, y=residuals, group=RDK, col = RDK)) + 
  geom_point(size=4, shape=19, position=pos)+
  geom_line(size = 1.5, position=pos)+
  scale_y_continuous(name = 'Residuals')+
  geom_errorbar(aes(ymin=residuals-CI, ymax=residuals+CI), 
                width=.01, position=pos) + 
  scale_color_manual(values=c('red', 'blue'), labels = c('Red', 'Blue'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

