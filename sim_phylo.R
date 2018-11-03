## Simulate phylogeny effects
library(mvtnorm)
library(reshape2)

source("functions.R")

##Attraction
lambda=5
omega=1
gamma=3

#distance
#d<-as.matrix(cophenetic(phy))
#d<-d/max(d)


random_traits<-traits
random_traits$Plant<-sample(random_traits$Plant)
rownames(random_traits)<-random_traits[,1]
Dint<-as.matrix(dist(random_traits[,-1]))
d<-Dint/max(Dint)


#distance
d<-as.matrix(cophenetic(phy))
d<-d/max(d)

##Attraction
lambda=5
omega=1
gamma=1

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma

a_effect<-cor_effect(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")

#repulsion
##Attraction
lambda=5
omega=1
gamma=1

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma

a_effect<-cor_effect_repulsion(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Correlation),lower=quantile(Correlation,0.05),upper=quantile(Correlation,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Correlation")


d["Drymonia collegarum","Drymonia teuscheri"]

vCov["Drymonia collegarum","Drymonia teuscheri"]

r<-data.frame(rmvnorm(1e4,mean=means,sigma=vCov))
colnames(r)<-colnames(vCov)
#ggplot(r,aes(x=`Drymonia teuscheri`,y=`Drymonia collegarum`)) + geom_point() + coord_equal()
ggplot(r,aes(x=inv.logit(`Drymonia teuscheri`),y=inv.logit(`Drymonia collegarum`))) + geom_point() + coord_equal()

ggplot(r,aes(x=inv.logit(`Gasteranthus lateralis`),y=inv.logit(`Columnea ciliata`))) + geom_point()+ coord_equal()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Drymonia teuscheri`)) + geom_point()+ coord_equal()

a_effect<-cor_effect(lambda=lambda,D=D,omega=omega,gamma=gamma) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Covariance),lower=quantile(Covariance,0.05),upper=quantile(Covariance,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Covariance")

##Calculate correlation
sim_vals<-function(lambda=1,omega=1,gamma=1){
  C<-exp(-lambda*d)
  vCov=(omega*C[,] + (1-omega) * I)*gamma
  
  r<-mvtnorm::rmvnorm(1e4,mean=means,vCov[,])
  colnames(r)<-phy$tip.label
  
  mvcov<-melt(data.frame(inv.logit(r)))
  dat<-mvcov %>% mutate(lambda=lambda,gamma=gamma,omega=omega)
  return(dat)
}
results<-list()

gammas<-seq(1,5,0.5)

for(y in 1:length(gammas) ){
  results[[y]]<-sim_vals(gamma=gammas[[y]])
}

results<-bind_rows(results)

ggplot(results) + geom_density(aes(x=value,fill=as.factor(gamma))) + facet_wrap(~variable) 

##Calculate correlation
sim_cor<-function(lambda=1,omega=1,gamma=1){
  C<-exp(-lambda*d)
  vCov=(omega*C[,] + (1-omega) * I)*gamma
  
  r<-data.frame(mvtnorm::rmvnorm(1e4,mean=means,vCov[,]))
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(d)
  colnames(mvcov)<-c("sp1","sp2","distance")
  
  dat<-cor_matrix %>% inner_join(mvcov)  %>% mutate(lambda=lambda,gamma=gamma,omega=omega)
  return(dat)
}

results<-list()

gammas<-seq(0,20,5)

for(y in 1:length(gammas) ){
  results[[y]]<-sim_cor(gamma=gammas[[y]])
}

results<-bind_rows(results)

ggplot(results,aes(x=distance,y=cor,col=gamma)) + geom_point() + geom_line(aes(group=gamma)) 

##Repulsion

#distance
lambda=1
omega=1
gamma=1

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,solve(vCov)))

colnames(r)<-phy$tip.label
ggplot(r,aes(x=inv.logit(`Gasteranthus lateralis`),y=inv.logit(`Gasteranthus quitensis`))) + geom_point() + coord_equal()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Columnea ciliata`)) + geom_point()+ coord_equal()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Drymonia teuscheri`)) + geom_point()+ coord_equal()

a_effect<-effect_repulsion(lambda=lambda,D=D,omega=omega,gamma=0.1) %>% group_by(Distance) %>% filter(!Distance==0) %>% summarize(mean=mean(Covariance),lower=quantile(Covariance,0.05),upper=quantile(Covariance,0.95))
ggplot(a_effect,aes(x=Distance,y=mean)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5) + theme_bw() + labs(x="Distance",y="Covariance")


##Calculate correlation
sim_cor<-function(lambda,omega=1,gamma=1){
  means<-rep(0,nrow(d))
  C<-exp(-lambda*d)
  vCov=(omega*C[,] + (1-omega) * I)*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,solve(vCov)))
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(d)
  colnames(mvcov)<-c("sp1","sp2","distance")
  
  dat<-cor_matrix %>% inner_join(mvcov)  %>% mutate(lambda=lambda)
  return(dat)
}

for(x in c(0.1,0.5,1:10)){
  print(x)
  gamma=x
  means<-rep(0,nrow(d))
  C<-exp(-lambda*d)
  vCov=(omega*C[,] + (1-omega) * I)*gamma
  r<-data.frame(rmvnorm(1e4,mean=means,solve(vCov)))
  
  colnames(r)<-phy$tip.label
  p<-ggplot(r,aes(x=inv.logit(`Gasteranthus lateralis`),y=inv.logit(`Gasteranthus quitensis`))) + geom_point() + coord_equal()  + ggtitle(gamma) + ylim(0,1) + xlim(0,1)
print(p)
  }
