## Simulate phylogeny effects
library(mvtnorm)
library(reshape2)

cor_effect<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
  colnames(r)<-colnames(vCov)
  cormatrix<-cor(r)
  data.frame(lambda,Distance=as.numeric(D),Correlation=as.numeric(cormatrix))
}

##Attraction
lambda=2
omega=1
gamma=8

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
colnames(r)<-colnames(vCov)
range(r)
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal() + geom_density_2d()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Besleria solanoides`))) + geom_point() + coord_equal()

a_effect<-cor_effect(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

##Repulsion
cor_effect_repulsion<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,sigma=solve(vCov)))
  colnames(r)<-colnames(vCov)
  cormatrix<-cor(r)
  data.frame(lambda,Distance=as.numeric(D),Correlation=as.numeric(cormatrix))
}
lambda=1
omega=1
gamma=5

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=solve(vCov)))
colnames(r)<-colnames(vCov)
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal() + geom_density2d()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Besleria solanoides`))) + geom_point() + coord_equal()

a_effect<-cor_effect_repulsion(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

