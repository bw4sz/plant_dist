#Source functions
tauC_repulsion<-function(omega,gamma,I,D,lambda_cov){
  C = exp(-lambda_cov * D)
  vCov=omega*C + (1-omega) * I
  result<-solve(vCov*gamma)
  diag(result)<-NA
  result<-reshape2::melt(result)
  return(result)
}

tauC_attraction<-function(omega,gamma,I,D,lambda_cov){
  C = exp(-lambda_cov * D)
  vCov=omega*C + (1-omega) * I
  result<-vCov*gamma
  diag(result)<-NA
  result<-reshape2::melt(result)
  return(result)
}
effect_repulsion<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=solve(vCov*gamma)
  data.frame(lambda,Distance=as.numeric(D),Covariance=as.numeric(vCov))
}

effect<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  data.frame(lambda,Distance=as.numeric(D),Covariance=as.numeric(vCov))
}

cor_effect<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=vCov*gamma
  cormatrix<-cov2cor(vCov)
  data.frame(lambda,Distance=as.numeric(D),Correlation=as.numeric(cormatrix))
}

cor_effect_repulsion<-function(lambda,D,omega,gamma){
  C<-exp(-lambda*D)
  vCov=(omega*C[,] + (1-omega) * I)
  vCov=solve(vCov*gamma)
  cormatrix<-cov2cor(vCov)
  data.frame(lambda,Distance=as.numeric(D),Correlation=as.numeric(cormatrix))
}

all_plants<-function(dat){
  data.frame(dat,Plant=unique(sumf$Plant),n=0)
}

