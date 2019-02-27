################################################################################
##### Fit individual-level IG-GAM mixture
##### Sample-based solution
##### Helen Steingroever
##### Last updated: August 2016
################################################################################

rm(list=ls())
# Load required libraries
source('C:/Users/s02na7/Dropbox/functions.R')
library(R2jags)
library(statmod)     # for rinvgauss 
load.module("wald")  # for the JAGS distribution dswald()
                     # To activate the extension, modules have to be loaded
                     # explicetly. 
library(tidyverse)
################################################################################

################################################################################
##### Genarate data
################################################################################
set.seed(as.numeric(Sys.time()))

RT<-read.csv('allRT.csv')
alpha<-list()
theta<-list()
xi<-list()


for (i in 1:max(RT[,4])) 
  {
  for (c in 1:3) 
    {
    for (s in 1:2) 
      {
      data<-filter(RT, Condition==c, RDK==s, Subj==i )
      data<-data$RT/1000
      minRT<-min(data)
      T<-length(data)
      params <- c("alpha", "theta", "mu_xi", "sig2_xi")
      dat <- list(RT=data, T=T, minRT=minRT)
      
      samples <- jags(dat, inits=NULL, params,  # inits=NULL
                      model.file="Model_IG_GAM_ind.txt", n.chains=3, n.iter=50000, 
                      n.burnin=1200, n.thin=10, DIC=T)  
      
      print(samples)
      
      
      out <- capture.output(print(samples))
      
      cat("DDMFits", out, file="summary_of_latest_DDM.txt", sep="n", append=TRUE)
      
      
      alpha[length(alpha)+1]<-samples$BUGSoutput$mean$alpha
      theta[length(theta)+1]<-samples$BUGSoutput$mean$theta
      xi[length(xi)+1]<-samples$BUGSoutput$mean$mu_xi
}
}
}
alpha
theta 
xi

#RT<-as.numeric(RT$Var1)/1000
#minRT <- min(RT)
#T<-length(RT)
################################################################################

################################################################################ 
##### JAGS
################################################################################
# Parameters to be monitored:	
#params <- c("alpha", "theta", "mu_xi", "sig2_xi")
#params <- c("mu_xi", "sig2_xi")

# Data to be passed on to JAGS
#dat <- list(RT=RT, T=T, minRT=minRT)

# Change the working directory to the folder containing the model file
#setwd("examples/SWmixturesCode")

# Collect samples from posterior distributions
#samples <- jags(dat, inits=NULL, params,  # inits=NULL
#	 			model.file="Model_IG_GAM_ind.txt", n.chains=3, n.iter=20000, 
#	 			n.burnin=1200, n.thin=10, DIC=T)  


# Print a summary of the posterior samples
#samples
#alpha<-samples$BUGSoutput$mean$alpha
#theta<-samples$BUGSoutput$mean$theta
#xi<-samples$BUGSoutput$mean$mu_xi

#samples.mcmc<-as.mcmc(samples)
#require(lattice)
#xyplot(samples.mcmc)
#densityplot(samples.mcmc)
#gelman.plot(samples.mcmc)
#gelman.diag(samples.mcmc)
################################################################################  
s = rep(c(1,2),30)
c = rep(c(1,1,2,2,3,3),10)
p = rep(c(1:10),each=6)
data=cbind(s,c,p)
Alphadata=cbind(alpha,theta,xi,data)
alpha_data=data.frame(Alphadata)
tail(alpha_data)
head(alpha_data)
colnames(alpha_data)<-c('alpha', 'theta', 'xi','RDK', 'Condition', 'Subj')

factordata<-mutate(alpha_data, RDK = ifelse ( RDK==1, 'red', 'blue'))
factordata<-mutate(factordata, Attention = ifelse(Condition==1, 'attend red', 
                                                  ifelse(Condition==2, 'attend blue', 'attend both')))
head(factordata)
factordata<-select(factordata,alpha,theta,xi,RDK,Attention,Subj)
head(factordata)
factordata <- transform(factordata,Subj=unlist(Subj))
factordata <- transform(factordata,alpha=unlist(alpha))
factordata <- transform(factordata,theta=unlist(theta))
factordata <- transform(factordata,xi=unlist(xi))
factordata$Attention<-factor(factordata$Attention)
factordata$RDK<-factor(factordata$RDK)
factordata$Subj<-factor(factordata$Subj)

str(factordata)

factordata$Attention = factor(factordata$Attention,levels(factordata$Attention)[c(3,2,1)])
factordata$RDK = factor(factordata$RDK,levels(factordata$RDK)[c(2,1)])
factordata$Subj = factor(factordata$Subj)

str(factordata)

alphaSum = summary_se_within(factordata, 'alpha', 'Subj', withinvars = c('RDK', 'Attention'))
thetaSum = summary_se_within(factordata, 'theta', 'Subj', withinvars = c('RDK', 'Attention'))
xiSum = summary_se_within(factordata, 'xi', 'Subj', withinvars = c('RDK', 'Attention'))

alphaSum$Attention<-as.factor(alphaSum$Attention)
alphaSum$Attention = factor(alphaSum$Attention,levels(alphaSum$Attention)[c(3,2,1)])
alphaSum$Attention = factor(alphaSum$Attention,levels(alphaSum$Attention)[c(3,2,1)])


plot=ggplot(alphaSum, aes(x=Attention, y=alpha, group = RDK, col = RDK)) + 
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin=alpha-CI, ymax=alpha+CI), 
                width=.01) + 
  geom_point(size=6, shape=21,fill="white")+
  scale_y_continuous(name = 'threshold')+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot

plot=ggplot(xiSum, aes(x=Attention, y=xi, group = RDK, col = RDK)) + 
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin=xi-CI, ymax=xi+CI), 
                width=.01) + 
  geom_point(size=6, shape=21,fill="white")+expand_limits(y = c(6, 8))+
  scale_y_continuous(name = 'Drift rate')+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot


plot=ggplot(thetaSum, aes(x=Attention, y=theta, group = RDK, col = RDK)) + 
  geom_line(size = 1.5)+
  geom_errorbar(aes(ymin=theta-CI, ymax=theta+CI), 
                width=.01) + 
  geom_point(size=6, shape=21,fill="white")+
  scale_y_continuous(name = 'Shift')+
  scale_colour_discrete(labels = c("Red", "Blue"))+
  theme_classic()+  theme(axis.text=element_text(size=14))
plot
