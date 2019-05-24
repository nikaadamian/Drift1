######DRIFT1A FULL ANALYSES#######
source('C:/Users/s02na7/Dropbox/functions.R')
source('~/Dropbox/functions.R')
library(tidyverse)
library(lme4)
library(ez)
library(multcomp)


load("C:/Users/s02na7/Dropbox/0_UoA/Drift1/Drift1/Amp1a.RData")
amps<-a
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1/Drift1/RT1a.RData")
rt<-a
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1/Drift1/SWFits1a.RData")
fits<-a

alldata<-list(amps, rt, fits)%>%reduce(full_join)%>%
  dplyr::select(Subj, RDK, Attention, amps, RT, gamma, alpha, theta)
head(alldata)
save(alldata, file = 'allDataDrift1a.RData')

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
ampSum = summary_se_within(alldata, 'amps', 'Subj', withinvars = c('RDK', 'Attention')) #ssvep
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

fit<-lm(amps~gamma+theta, partMLdata)
summary(fit)
ggPredict(fit, colorn = 5, colorAsFactor = F)+theme_classic()+
  scale_x_continuous(name='Drift rate')+
  scale_color_continuous(name='Shift')+
  scale_y_continuous(name='Normalised amplitudes')


plot(alldata$amps~alldata$theta)
plot(alldata$amps~alldata$gamma)


###DUMP ALL DATA TOGETHER###
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1/Drift1/allDataDrift1a.RData")
dataDrift1a<-alldata%>%arrange(Attention, RDK, Subj)%>%
      mutate(SubjAll=Subj)%>%
      dplyr::select(-Subj)
rm(alldata)

nSubj<-nlevels(dataDrift1a$Subj)
load("C:/Users/s02na7/Dropbox/0_UoA/Drift1/Drift1/DataAll.RData")

dataDrift<-allData%>%arrange(Attention, RDK, Subj)%>%
          mutate(SubjAll=factor(as.numeric(Subj)+nSubj))%>%
          dplyr::select(-Subj)

rm(allData)
    
DataAll<-full_join(dataDrift, dataDrift1a)%>%
        arrange(Attention, RDK, SubjAll)

rm(dataDrift, dataDrift1a)

alphaSum = summary_se_within(DataAll, 'alpha', 'SubjAll', withinvars = c('RDK', 'Attention')) #thres
gammaSum = summary_se_within(DataAll, 'gamma', 'SubjAll', withinvars = c('RDK', 'Attention')) # drift
thetaSum = summary_se_within(DataAll, 'theta', 'SubjAll', withinvars = c('RDK', 'Attention')) #nondec
ampSum = summary_se_within(DataAll, 'amps', 'SubjAll', withinvars = c('RDK', 'Attention')) #ssvep
RTSum = summary_se_within(DataAll, 'RT', 'SubjAll', withinvars = c('RDK', 'Attention')) #meanrt
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


plot(DataAll$amps~DataAll$theta)
plot(DataAll$amps~DataAll$gamma)
cor.test(DataAll$amps, DataAll$gamma)


