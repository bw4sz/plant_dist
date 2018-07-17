sink("model/poisson_occ_elev.jags")
cat("
    model {
    
    for (x in 1:Nobs){

      #Observation - count
      Y[x] ~ dpois(lambda[x])

      #Intensity is the probability of presence, species intercept and the phylogenetic covariance
      log(lambda[x]) = alpha[Plant[x]]
      
      #Residuals
      discrepancy[x] <- pow(Y[x] - lambda[x],2)

      #Assess Model Fit
      Ynew[x] ~ dpois(lambda[x])
      discrepancy.new[x]<-pow(Ynew[x] - lambda[x],2)
    
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)
    fitnew<-sum(discrepancy.new)

    #Prediction

    for(i in 1:Npreds){

      #predict value

      #Observation - count
      prediction[i] ~ dpois(lambda_new[i])

      #Intensity is the probability of presence, species intercept and the phylogenetic covariance
      log(lambda_new[i]) = alpha[Ypred_plant[i]]

      #squared predictive error
      pred_error[i] <- pow(Ypred[i] - prediction[i],2)
    }

    #Predictive Error
    fitpred<-sum(pred_error)

    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    #Intercept flowering count
    alpha[j] ~ dnorm(0,0.0001)

    } 
    
    }
    ",fill=TRUE)

sink()
