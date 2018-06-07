## Simulate traits
library(mvtnorm)
library(reshape2)

##Attraction

lambda=5
#distance
d<-as.matrix(dist(traits[,-1]))
d<-d/max(d)

means<-rep(0,nrow(d))
C<-exp(-lambda*d)
r<-data.frame(rmvnorm(1e4,mean=means,C))
colnames(r)<-rownames(traits)
ggplot(r,aes(x=`Glossoloma purpureum`,y=`Gasteranthus quitensis`)) + geom_point() + ggtitle("Distant in trait space")
ggplot(r,aes(x=`Columnea strigosa`,y=`Columnea medicinalis`)) + geom_point() + ggtitle("Close in trait space")

##Calculate correlation
sim_cor<-function(lambda){
  C<-exp(-lambda*d)
  r<-data.frame(mvtnorm::rmvnorm(1e4,mean=means,C[,]))
  
  #rename columns to match
  colnames(r)<-rownames(traits)
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(d)
  colnames(mvcov)<-c("sp1","sp2","distance")
  
  dat<-cor_matrix %>% inner_join(mvcov)  %>% mutate(lambda=lambda)
  return(dat)
}

results<-list()

lambdas<-seq(0,10,1)

for(y in 1:length(lambdas) ){
  results[[y]]<-sim_cor(lambda=lambdas[[y]])
}

results<-bind_rows(results)

ggplot(results,aes(x=distance,y=cor,col=lambda)) + geom_point() + geom_line(aes(group=lambda)) 

##Repulsion

#distance
lambda=5
d<-as.matrix(dist(traits[,-1]))
d<-d/max(d)
means<-rep(0,nrow(d))
C<-exp(-lambda*d)
C<-solve(C)
r<-data.frame(rmvnorm(1e4,mean=means,C))

colnames(r)<-rownames(traits)
ggplot(r,aes(x=`Glossoloma purpureum`,y=`Gasteranthus quitensis`)) + geom_point() + ggtitle("Distant in trait space")
ggplot(r,aes(x=`Columnea strigosa`,y=`Columnea medicinalis`)) + geom_point() + ggtitle("Close in trait space")

##Calculate correlation
sim_cor<-function(lambda){
  C<-exp(-lambda*d)
  C<-solve(C)
  r<-data.frame(mvtnorm::rmvnorm(1e4,mean=means,C[,]))
  
  #rename columns to match
  colnames(r)<-rownames(traits)
  cor_matrix<-melt(cor(r))
  colnames(cor_matrix)<-c("sp1","sp2","cor")
  mvcov<-melt(d)
  colnames(mvcov)<-c("sp1","sp2","distance")
  
  dat<-cor_matrix %>% inner_join(mvcov)  %>% mutate(lambda=lambda)
  return(dat)
}

results<-list()

lambdas<-seq(0.1,10,0.5)

for(y in 1:length(lambdas) ){
  results[[y]]<-sim_cor(lambda=lambdas[[y]])
}

results<-bind_rows(results)

results %>% filter(!distance==0) %>% ggplot(.,aes(x=distance,y=cor,col=lambda)) + geom_point() + geom_line(aes(group=lambda)) + labs(x="Trait Distance",y="Correlation in effect of julian day")

