## Simulate phylogeny effects
library(mvtnorm)
library(reshape2)

source("functions.R")

#distance
d<-as.matrix(cophenetic(phy))
d<-d/max(d)

##Attraction
lambda=2
omega=1
gamma=1

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
colnames(r)<-colnames(vCov)
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Besleria solanoides`))) + geom_point() + coord_equal()

a_effect<-cor_effect(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

##Repulsion
lambda=1
omega=1
gamma=10

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=solve(vCov)))
colnames(r)<-colnames(vCov)
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Besleria solanoides`))) + geom_point() + coord_equal()

a_effect<-cor_effect_repulsion(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

