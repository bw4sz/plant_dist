sink("model/threshold_attraction.jags")
cat("
    model {
    
    for (x in 1:Dates){
      for (y in 1:Plants){
        #Observation of a flowering plant
        Y[x,y] ~ dbern(p[x,y])
        logit(p[x,y]) <- alpha[y] + e[x,y]
  
        #Residuals
        discrepancy[x,y] <- pow(Y[x,y] - p[x,y],2)/ p[x,y]
        
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
      log(p_new[x,y])<-alpha[y] + e_new[x,y]
      
      #squared predictive error
      pred_error[x,y] <- pow(Ypred[x,y] - prediction[x,y],2)/prediction[x,y]
      }
}
    
    #Predictive Error
    fitpred<-sum(pred_error)
    
    #########################
    #autocorrelation in error
    #########################
    
    #For each of observation
    for(k in 1:Dates){
      e[k,1:Plants] ~ dmnorm(zeros,tauC[,])
    }
    
    #For each prediction
    for(k in 1:NewDates){
      e_new[k,1:Plants] ~ dmnorm(zeros,tauC[,])
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
    alpha[j] ~ dnorm(0,0.386)
    
    } 
    
    #Autocorrelation priors
    gamma ~ dunif(0,20)
    
    #Strength of covariance decay
    lambda_cov = 1
    omega ~ dbeta(1,1)
    }
    ",fill=TRUE)

sink()
