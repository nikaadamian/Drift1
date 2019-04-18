source('C:/Users/nikaadamian/Dropbox/functions.R')
source('~/Dropbox/functions.R')
library(tidyverse)
library(lme4)
library(ez)
library(multcomp)

data<-read.table('amps.txt', header=F)
data$Subj=c(1:19)
data_long <- gather(data, key=condition, value=amps, -Subj)
head(data_long)

data_long<-mutate(data_long, RDK = ifelse ((condition == 'V1' | condition == 'V2' | condition == 'V3'), 'red', 'blue'))

data_long<-mutate(data_long, Attention = ifelse (condition == 'V1' | condition == 'V4', 'attended', 
                                                 ifelse (condition == 'V2' | condition == 'V5', 'neutral', 
                                                 'unattended' )))
data<-data_long
data$RDK<-as.factor(data$RDK)
data$Attention<-as.factor(data$Attention)
relevel(data$RDK, 'red')
data<-filter(data,Subj!=14)

plot=ggplot(data, aes(x=Attention, y=amps, group=RDK, col = RDK)) + 
  geom_point(size=2, shape=21,fill="white")+
  geom_line()+
  scale_y_continuous(name = 'NormAmp')+
  scale_colour_discrete(labels = c("Red", "Blue"))+
  theme_classic()+  theme(axis.text=element_text(size=6))+
  facet_wrap(~Subj)
plot


a = summary_se_within(data, 'amps', 'Subj', withinvars = c('RDK', 'Attention'))
print(a)
position=position_dodge(width=0.2)
plot=ggplot(a, aes(x=Attention, y=amps, group = RDK, col = RDK)) + 
  geom_line(size = 1.5, position = position)+
  geom_errorbar(aes(ymin=amps-CI, ymax=amps+CI), 
                width=.01, position = position) +
  geom_point(size=3, shape=21,fill="white", position=position)+expand_limits(y = c(0.85, 1.15))+
  scale_y_continuous(name = 'normalized amplitude')+
  scale_color_manual(values=c('blue', 'red'))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

a<-ezANOVA(
  data = data
  , dv = amps
  , wid = Subj
  , within = .(Attention, RDK)
  , type=2
  , detailed=T
)
a

###
RT<-read.csv('allRT.csv')
RT<-mutate(RT, RDK = ifelse (RDK == 1, 'red', 'blue'))
RT<-mutate(RT, Condition = ifelse (Condition == 1, 'attend red', 
                              ifelse (Condition ==2, 'attend blue', 
                                      'attend both')))
Red<-RT%>%
  filter(RDK=='red')%>%
  mutate(Attention=ifelse(Condition=='attend red', 'attended', 
                          ifelse(Condition=='attend blue', 'unattended', 
                                 'neutral')))

Blue<-RT%>%
  filter(RDK=='blue')%>%
  mutate(Attention=ifelse(Condition=='attend blue', 'attended', 
                          ifelse(Condition=='attend red', 'unattended', 
                                 'neutral')))

dataRT<-rbind(Red, Blue)
sumRT<-dataRT%>%
  select(RT, Attention, RDK, Subj)%>%
  filter(Subj!=14)%>%
  group_by(RDK, Attention, Subj)%>%
  summarise(mean_rt=mean(RT))


dataRT$Attention<-as.factor(dataRT$Attention)
plot=dataRT%>%
  ggplot(aes(x=RT,group=Attention, colour=Attention)) + 
  geom_density()+
  facet_wrap(~Subj)
plot

plot=dataRT%>%
  ggplot(aes(x=RT,group=Attention, colour=Attention)) + 
  geom_density()
plot

RTsum=summary_se_within(dataRT, 'RT', 'Subj', withinvars = c('Attention', 'RDK'))
RTsum$RDK=as.factor(RTsum$RDK)

plot=ggplot(RTsum, aes(x=Attention, y=RT, group = RDK, col = RDK)) + 
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin=RT-CI, ymax=RT+CI), 
                width=.01) + 
  geom_point(size=6, shape=21,fill="white")+
  scale_y_continuous(name = 'RT')+
  scale_colour_discrete(labels = c("Red", "Blue"))+
  theme_classic()+  theme(axis.text=element_text(size=14))+facet_wrap(~RDK)
plot

summary<-dataRT%>%
  group_by(Subj, RDK, Attention)%>%
  summarise(mn=mean(RT))

plot=ggplot(summary, aes(x=Attention, y=mn, group=RDK, col = RDK)) + 
  geom_line(size = 0.5)+
  geom_point(size=2, shape=21,fill="white")+
  scale_y_continuous(name = 'RT')+
  scale_colour_discrete(labels = c("Red", "Blue"))+
  theme_classic()+  theme(axis.text=element_text(size=14))+facet_wrap(~Subj)
plot


###correlations###
head(dataRT)
head(data)

RT<-dataRT%>%
  dplyr::select(RT, Attention, RDK, Subj)%>%
  group_by(Attention, RDK, Subj)%>%
  summarise(RT=mean(RT))

Amps<-data%>%
  dplyr::select(amps, Attention, RDK, Subj)

RTAmps<-inner_join(RT, Amps)
RTAmps$Subj<-as.factor(RTAmps$Subj)
RTAmps$RDK<-as.factor(RTAmps$RDK)
RTAmps$Attention<-as.factor(RTAmps$Attention)
RTAmps$RDK<-relevel(RTAmps$RDK, 'red')

ggplot(RTAmps, aes(x=amps, y=RT, col=RDK))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+
  facet_wrap(~RDK)

cor.test(RTAmps$amps[RTAmps$RDK=='red'], RTAmps$RT[RTAmps$RDK=='red'])


RTDiffs<-RTAmps%>%
  dplyr::select(RDK, Subj, RT, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, RT)%>%
  mutate(RTenhance=attended-neutral, 
         RTsuppress=neutral-unattended)%>%
  dplyr::select(RDK, Subj, RTenhance, RTsuppress)
  
AmpDiffs<-RTAmps%>%
  dplyr::select(RDK, Subj, amps, Attention)%>%
  group_by(Subj, RDK)%>%
  spread(Attention, amps)%>%
  mutate(Ampenhance=attended-neutral, 
         Ampsuppress=neutral-unattended)%>%
  dplyr::select(Ampenhance, Ampsuppress)

Diffs<-full_join(RTDiffs, AmpDiffs)
head(Diffs)

ggplot(Diffs, aes(x=Ampenhance, y=RTenhance, col=RDK))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)

cor.test(Diffs$Ampenhance, Diffs$RTenhance)


ggplot(Diffs, aes(x=Ampsuppress, y=RTsuppress, col=RDK))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)

cor.test(Diffs$Ampsuppress, Diffs$RTsuppress)
