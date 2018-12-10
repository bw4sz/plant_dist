sink("model/threshold_baseline_site.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation of a flowering plant
    Y[x] ~ dbern(p[x])
    logit(p[x]) <- alpha[Plant[x],Site[x]] 
    
    #Residuals
    discrepancy[x] <- abs(Y[x] - p[x])
    
    #Assess Model Fit
    Ynew[x] ~ dbern(p[x])
    discrepancy.new[x]<-abs(Ynew[x] - p[x])
    }
    
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction
    
    for(x in 1:Npreds){
    #predict value
    
    #Observation - probability of flowering
    prediction[x] ~ dbern(p_new[x])
    logit(p_new[x])<-alpha[PredPlant[x],NewSite[x]]
    
    #predictive error
    pred_error[x] <- abs(Ypred[x] - p_new[x])
    }
    
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
    #Priors
    
    #Species level priors
    
    for(i in 1:Sites){
    for (j in 1:Plants){
    
    #Intercept flowering probability
    alpha[j,i] ~ dnorm(0,0.386)
    
    } 
    }
    }
    ",fill=TRUE)

sink()
