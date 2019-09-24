## Simulate phylogeny effects
library(mvtnorm)
library(reshape2)

cor_effect<-function(lambda,Dint,omega,gamma){
  C<-exp(-lambda*Dint)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
  colnames(r)<-colnames(vCov)
  cormatrix<-cor(r)
  data.frame(lambda,Distance=as.numeric(Dint),Correlation=as.numeric(cormatrix))
}

##Attraction
lambda=1
omega=1
gamma=1

means<-rep(0,nrow(Dint))
C<-exp(-lambda*Dint)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
colnames(r)<-colnames(vCov)
range(r)
#Closely related
ggplot(r,aes(x=inv.logit(`Kohleria affinis`),y=inv.logit(`Glossoloma oblongicalyx`))) + geom_point() + coord_equal() + geom_density_2d()

#Distance related
ggplot(r,aes(x=inv.logit(`Glossoloma purpureum`),y=inv.logit(`Drymonia tenuis`))) + geom_point() + coord_equal()+ geom_density_2d()

a_effect<-cor_effect(lambda=lambda,Dint=Dint,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

##Repulsion
cor_effect_repulsion<-function(lambda,Dint,omega,gamma){
  C<-exp(-lambda*Dint)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,sigma=solve(vCov)))
  colnames(r)<-colnames(vCov)
  cormatrix<-cor(r)
  data.frame(lambda,Distance=as.numeric(Dint),Correlation=as.numeric(cormatrix))
}
lambda=1
omega=1
gamma=5

means<-rep(0,nrow(Dint))
C<-exp(-lambda*Dint)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,sigma=solve(vCov)))
colnames(r)<-colnames(vCov)
ggplot(r,aes(x=inv.logit(`Glossoloma oblongicalyx`),y=inv.logit(`Columnea kucyniakii`))) + geom_point() + coord_equal() + geom_density2d()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Besleria solanoides`))) + geom_point() + coord_equal()

a_effect<-cor_effect_repulsion(lambda=lambda,Dint=Dint,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")
