sink("model/poisson_baseline.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation of a flowering plant
    Y[x] ~ dpois(lambda[Plant[x]])
    
    #Residuals
    discrepancy[x] <- pow(Y[x] - lambda[Plant[x]],2)
    
    #Assess Model Fit
    Ynew[x] ~ dpois(lambda[Plant[x]])
    discrepancy.new[x]<-pow(Ynew[x] - lambda[Plant[x]],2)
    
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction
    
    for(i in 1:Npreds){
    
    #predict value
    
    #Observation - probability of flowering
    prediction[i] ~ dpois(lambda[Ypred_plant[i]])
    
    #squared predictive error
    pred_error[i] <- pow(Ypred[i] - prediction[i],2)
    }
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    #Intercept flowering count
    lambda[j] ~ dunif(0,100)
    
    } 
    
    }
    ",fill=TRUE)

sink()
