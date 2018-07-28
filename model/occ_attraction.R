sink("model/Poisson_attraction.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
      #Observation of a flowering plant
      Y[x] ~ dpois(p[x])
      log(p[x]) <- alpha[Plant[x]] + e[Plant[x],Date[x]]
  
      #Residuals
      discrepancy[x] <- pow(Y[x] - p[x],2)
      
      #Assess Model Fit
      Ynew[x] ~ dpois(p[x])
      discrepancy.new[x]<-pow(Ynew[x] - p[x],2)
    
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction
    
    for(i in 1:Npreds){
    
    #predict value
    
    #Observation - probability of flowering
    prediction[i] ~ dpois(p_new[i])
    log(p_new[i])<-alpha[Ypred_plant[i]] + e[Ypred_plant[i],Ypred_date[i]]
    
    #squared predictive error
    pred_error[i] <- pow(Ypred[i] - p_new[i],2)
    }
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
    #########################
    #autocorrelation in error
    #########################
    
    #For each of observation
    for(k in 1:Dates){
      e[1:Plants,k] ~ dmnorm(zeros[],tauC[,])
    }
    
    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda_cov * D[i,j])
    }
    }
    
    ## Covert variance to precision for each parameter, allow omega to shrink to identity matrix
    vCov = omega*C[,] + (1-omega) * I
    tauC=inverse(vCov*gamma)
    
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    #Intercept flowering count
    alpha[j] ~ dnorm(0,0.0001)
    
    } 
    
    #Autocorrelation priors
    gamma ~ dunif(0,20)
    
    #Strength of covariance decay
    lambda_cov = 5
    omega ~ dbeta(1,1)
    }
    ",fill=TRUE)

sink()
