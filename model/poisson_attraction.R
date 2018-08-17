sink("model/Poisson_attraction.jags")
cat("
    model {
    
    for (x in 1:Dates){
      for (y in 1:Plants){
        #Observation of a flowering plant
        Y[x,y] ~ dpois(p[x,y])
        log(p[x,y]) <- alpha[y] + e[x,y]
  
        #Residuals
        discrepancy[x,y] <- pow(Y[x,y] - p[x,y],2)
        
        #Assess Model Fit
        Ynew[x,y] ~ dpois(p[x,y])
        discrepancy.new[x,y]<-pow(Ynew[x,y] - p[x,y],2)
      }
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction
    
    for (x in 1:NewDates){
      for (y in 1:Plants){
    
      #predict value
      
      #Observation - probability of flowering
      prediction[x,y] ~ dpois(p_new[x,y])
      log(p_new[x,y])<-alpha[y] + e_new[x,y]
      
      #squared predictive error
      pred_error[x,y] <- pow(Ypred[x,y] - prediction[x,y],2)
      }
}
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
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
    alpha[j] ~ dnorm(0,0.001)
    
    } 
    
    #Autocorrelation priors
    gamma ~ dunif(0,20)
    
    #Strength of covariance decay
    lambda_cov = 1
    omega ~ dbeta(1,1)
    }
    ",fill=TRUE)

sink()
