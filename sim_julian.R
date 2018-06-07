##Julian day simulation

predict_plant<-function(alpha,beta,beta2,julian){
  result<-exp(alpha + beta * julian + beta2 * julian^2)
  return(data.frame(alpha,beta,beta2,julian,value=result))
}

sim<-function(alpha,beta,beta2,julian){
  
  df<-data.frame(alpha,beta,beta2,julian)
  dflist<-replicate(3, df, simplify = FALSE)
  for(x in 1:length(dflist)){
    dflist[[x]]$Iteration<-x
  }
  result<-bind_rows(dflist)
  
  
  preds<- result %>% group_by(Iteration) %>% do(predict_plant(.$alpha,.$beta,.$beta2,julian=julian)) %>% group_by(julian) %>% summarize(mean=mean(value),lower=quantile(value,0.05),upper=quantile(value,0.95))
  
  ggplot(preds,aes(x=julian,y=mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.5)
}

sim(alpha=-4.13,beta=0.05,beta2=-5e-05,julian=unique(indat$julian))
