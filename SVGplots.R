source('~/Dropbox/functions.R')
load("~/Dropbox/0_UoA/Drift1/Drift1/DataAll.RData")
library(tidyverse)
library(ez)

r<-ezANOVA(
  data = allData
  , dv = gamma
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
r
r<-ezANOVA(
  data = allData
  , dv = theta
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
r

r<-ezANOVA(
  data = allData
  , dv = alpha
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
r

r<-ezANOVA(
  data = allData
  , dv = amps
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
r

r<-ezANOVA(
  data = allData
  , dv = RT
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
r

alphaSum = summary_se_within(allData, 'alpha', 'Subj', withinvars = c('RDK', 'Attention')) #thres
gammaSum = summary_se_within(allData, 'gamma', 'Subj', withinvars = c('RDK', 'Attention')) # drift
thetaSum = summary_se_within(allData, 'theta', 'Subj', withinvars = c('RDK', 'Attention')) #nondec
ampSum = summary_se_within(allData, 'amps', 'Subj', withinvars = c('RDK', 'Attention')) #nondec
RTSum = summary_se_within(allData, 'RT', 'Subj', withinvars = c('RDK', 'Attention')) #nondec
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


plot=ggplot(d, aes(x=RT,group=Attention, colour=Attention)) + 
  scale_x_continuous(name='Reaction time (ms)', limits=c(200,1500))+
  geom_density(size=0.8, alpha=0.8)+theme_classic()+facet_wrap(~RDK)+ 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        legend.text=element_text(size=14), 
        legend.title = element_text(size=14))
plot

MLdata <- transform(allData,
                    amps_cs=scale(amps),
                    alpha_cs=scale(alpha), 
                    gamma_cs = scale(gamma), 
                    theta_cs = scale(theta))



fit<-lm(amps~gamma+theta, partMLdata)
summary(fit)
ggPredict(fit, colorn = 5, colorAsFactor = F)+theme_classic()+
  scale_x_continuous(name='Drift rate')+
  scale_color_continuous(name='Shift')+
  scale_y_continuous(name='Normalised amplitudes')
  

plot(MLdata$amps~MLdata$theta_cs)
abline(fitDrift)


fit<-lm(amps~gamma_cs, partMLdata)
summary(fit)


partMLdata<-MLdata%>%filter(Subj %in% subj)
subj=c(2,3,4,6,7,9,10,13,17, 18, 19)
