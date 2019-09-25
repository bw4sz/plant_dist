sink("model/conditional_month.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation of a flowering plant
    Y[x] ~ dbern(psi[x])
    
    #Conditional probability flowering|occurrence
    psi[x] = z[x] * rho[Plant[x],Month[x]]
    
    #probability of occurrence
    z[x] ~ dbern(occ[x])
    logit(occ[x]) <-  alpha[Plant[x]] + beta[Plant[x]] * elevation[x]
    
    #Residuals
    discrepancy[x] <- abs(Y[x] - psi[x])
    
    #Assess Model Fit
    Ynew[x] ~ dbern(psi[x])
    discrepancy.new[x]<-abs(Ynew[x] - psi[x])
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction, same model as above, different index.
    
    for(x in 1:Npreds){
    
    prediction[x] ~ dbern(psi_new[x])
    
    #probability of occurrence
    psi_new[x] =  z_new[x] * rho[NewPlant[x],NewMonth[x]]
    z_new[x] ~ dbern(occ_new[x])
    logit(occ_new[x])<- alpha[NewPlant[x]] + beta[NewPlant[x]] * new_elevation[x]
    
    #predictive error
    pred_error[x] <- abs(Ypred[x] - psi_new[x])
    }
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
    #Priors
    
    for (j in 1:Plants){
    #elevation occurrence priors
    alpha[j] ~ dnorm(0,0.386)
    beta[j] ~ dnorm(0,0.386)
    
    #Intercept flowering probability
    for(k in 1:Months){
    rho[j,k] ~ dbeta(1,1)
    }
    }
    
    }
    ",fill=TRUE)

sink()
