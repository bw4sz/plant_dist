sink("model/threshold_baseline.jags")
cat("
    model {
    
    for (x in 1:Dates){
    for (y in 1:Plants){
    #Observation of a flowering plant
    Y[x,y] ~ dbern(p[x,y])
    logit(p[x,y]) <- alpha[y]
    
    #Residuals
    discrepancy[x,y] <- pow(Y[x,y] - p[x,y],2)/p[x,y]
    
    #Assess Model Fit
    Ynew[x,y] ~ dbern(p[x,y])
    discrepancy.new[x,y]<-pow(Ynew[x,y] - p[x,y],2)/ p[x,y]
    }
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)
    fitnew<-sum(discrepancy.new)
    
    #Prediction
    
    for (x in 1:NewDates){
    for (y in 1:Plants){
    
    #predict value
    
    #Observation - probability of flowering
    prediction[x,y] ~ dbern(p_new[x,y])
    log(p_new[x,y])<-alpha[y]
    
    #squared predictive error
    pred_error[x,y] <- pow(Ypred[x,y] - prediction[x,y],2)/prediction[x,y]
    }
    }
    
    #Predictive Error
    fitpred<-sum(pred_error)
    
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    #Intercept flowering count
    alpha[j] ~ dnorm(0,0.386)
    
    } 

    }
    ",fill=TRUE)

sink()
