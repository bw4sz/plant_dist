library(mvtnorm)
library(reshape2)
sigma<-0.1
lambda<-0.1
  
vcov<-vcv(phy,corr = T)
#vcov2<-vcv(corBlomberg(2, phy),corr=T)

C<-lambda * solve(vcov2[,]) + (1-lambda) * I[,]
r<-data.frame(rmvnorm(1e4,mean=0,sigma^2*C))
ggplot(r,aes(x=`Gasteranthus.lateralis`,y=`Gasteranthus.quitensis`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus.lateralis`,y=`Columnea.ciliata`)) + geom_point()
ggplot(r,aes(x=`Gasteranthus.lateralis`,y=`Drymonia.teuscheri`)) + geom_point()


## Simulation 1

##Pagels lambda
##Calculate correlation

sim_cor<-function(sigma,lambda){
  C<-lambda * vcov[,] + (1-lambda) * I[,]
  r<-data.frame(rmnorm(1e4,mean=0,sigma^2*C))
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(vcov)
  colnames(mvcov)<-c("sp1","sp2","relatedness")
  
  dat<-cor_matrix %>% inner_join(mvcov) %>% filter(!sp1==sp2) %>% mutate(sigma=sigma,lambda=lambda)
  return(dat)
}

results<-list()

sigmas<-1
lambdas<-seq(0.1,1,0.1)
for(x in 1:length(sigmas) ){
  sub_result<-list()
  for(y in 1:length(lambdas) ){
    sub_result[[y]]<-sim_cor(sigmas[[x]],lambdas[[y]])
  }
  results[[x]]<-bind_rows(sub_result)
}

results<-bind_rows(results)

ggplot(results,aes(x=relatedness,y=cor,col=as.factor(sigma))) + geom_point() + geom_line() + facet_wrap(~lambda)

#repulsion

sim_cor<-function(sigma,lambda){
  C<-lambda * solve(vcov[,]) + (1-lambda) * I[,]
  r<-data.frame(rmnorm(1e4,mean=0,sigma^2*C))
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(vcov)
  colnames(mvcov)<-c("sp1","sp2","relatedness")
  
  dat<-cor_matrix %>% inner_join(mvcov) %>% filter(!sp1==sp2) %>% mutate(sigma=sigma,lambda=lambda)
  return(dat)
}

results<-list()

sigmas<-1
lambdas<-seq(0.1,1,0.1)

for(x in 1:length(sigmas) ){
  sub_result<-list()
  for(y in 1:length(lambdas) ){
    sub_result[[y]]<-sim_cor(sigmas[[x]],lambdas[[y]])
  }
  results[[x]]<-bind_rows(sub_result)
}

results<-bind_rows(results)

ggplot(results,aes(x=relatedness,y=cor,col=as.factor(lambda))) + geom_point() + geom_line() + facet_wrap(~sigma)

##########################################
##Exponential increase with branch lengths
##########################################
co<-cophenetic(phy)
co<-co/max(co)

##Calculate correlation
sim_cor<-function(lambda){
  C<-exp(lambda * co[,])
  r<-data.frame(rmnorm(1e4,mean=0,C[,]))
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(vcov)
  colnames(mvcov)<-c("sp1","sp2","relatedness")
  
  dat<-cor_matrix %>% inner_join(mvcov) %>% filter(!sp1==sp2) %>% mutate(sigma=sigma,lambda=lambda)
  return(dat)
}

results<-list()

lambdas<-seq(0.1,1,0.1)

for(y in 1:length(lambdas) ){
  results[[y]]<-sim_cor(lambda=lambdas[[y]])
}

results<-bind_rows(results)

ggplot(results,aes(x=relatedness,y=cor,col=as.factor(lambda))) + geom_point() + geom_line() 

## Simulation 3

##Exponential repulsion
##Calculate correlation

sim_cor<-function(sigma,lambda){
  C<-exp(lambda * solve(vcov[,]))
  r<-data.frame(rmnorm(1e4,mean=0,C[,]))
  
  
  #rename columns to match
  colnames(r)<-phy$tip.label
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(vcov)
  colnames(mvcov)<-c("sp1","sp2","relatedness")
  
  dat<-cor_matrix %>% inner_join(mvcov) %>% filter(!sp1==sp2) %>% mutate(sigma=sigma,lambda=lambda)
  return(dat)
}

results<-list()

sigmas<-c(0.1,1)
lambdas<-seq(0.1,1,0.1)/1000

for(x in 1:length(sigmas) ){
  sub_result<-list()
  for(y in 1:length(lambdas) ){
    sub_result[[y]]<-sim_cor(sigmas[[x]],lambdas[[y]])
  }
  results[[x]]<-bind_rows(sub_result)
}

results<-bind_rows(results)

ggplot(results,aes(x=relatedness,y=cor,col=as.factor(lambda))) + geom_point() + geom_line() + facet_wrap(~sigma)


