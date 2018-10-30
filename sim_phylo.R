## Simulate phylogeny effects
library(mvtnorm)
library(reshape2)

##Attraction
lambda=2
omega=1
gamma=1

#distance
d<-as.matrix(cophenetic(phy))
d<-d/max(d)

#d<-Dint
means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,vCov))
colnames(r)<-phy$tip.label
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Gasteranthus quitensis`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Columnea ciliata`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Drymonia teuscheri`)) + geom_point()

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
lambda=0.01
omega=1
gamma=1

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
vCov=(omega*C[,] + (1-omega) * I)*gamma
r<-data.frame(rmvnorm(1e4,mean=means,solve(vCov)))

colnames(r)<-phy$tip.label
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Gasteranthus quitensis`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Columnea ciliata`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus lateralis`,y=`Drymonia teuscheri`)) + geom_point()

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

results<-list()

lambdas<-seq(0.01,5,0.5)

for(y in 1:length(lambdas) ){
  results[[y]]<-sim_cor(lambda=lambdas[[y]])
}

results<-bind_rows(results)

results %>% filter(!distance==0) %>% ggplot(.,aes(x=distance,y=cor,col=lambda)) + geom_point() + geom_line(aes(group=lambda)) 

