sink("model/normal_attraction.jags")
cat("
    model {
    
    for (x in 1:Nobs){

      #observation
      mu[x] <-  alpha + beta[Plant[x]] * ele[x] + e[Plant[x]]
      Yobs[x] ~ dnorm(mu[x],tau[Plant[x]])T(0,365)
      
      #Residuals
      residuals[x] <- Yobs[x] - mu[x]
      
      #squared error
      sq[x]<-pow(residuals[x],2)

      #Assess Model Fit - squared residuals
      Ynew[x] ~ dnorm(mu[x],tau[Plant[x]])T(0,365)
      sq.new[x]<-pow(Ynew[x] - mu[x],2)
    
    }
    
    #Root mean squared error
    fit<-sqrt(sum(sq[])/Nobs)
    fitnew<-sqrt(sum(sq.new[])/Nobs)

    #autocorrelation in error
    e[1:Plants] ~ dmnorm(zeros[],tauC[,])
    
    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda * D[i,j])
    }
    }
    
    ## Covert variance to precision for each parameter
    
    iC=inverse(omega*C[,])
    tauC=iC
    tauC2=iC
    
    ###########
    #Prediction
    ###########

    for(i in 1:Npreds){
    
    #predict value
    mu_new[i] <-  alpha + beta[Ypred_plant[i]] * ele_new[i] + e[Ypred_plant[i]]
    prediction[i] ~ dnorm(mu_new[i],tau[Ypred_plant[i]])T(0,365)
    
    #squared predictive error
    pred_error[i] <- pow(Ypred[i] - prediction[i],2)
    }
    
    #Root Mean Squared Predictive Error
    fitpred<-sqrt(sum(pred_error)/Npreds)
    
    # Priors #
    ##########

    #Species level priors
    
    for (j in 1:Plants){

    beta[j] ~ dnorm(0,0.0001)

    #variance
    sigma[j] ~ dunif(0,75)
    tau[j] <- pow(sigma[j], -2)
    } 

    #Intercept
    alpha ~ dnorm(0,0.0001)
    
    #Strength of covariance decay
    lambda ~ dunif(0,5)

    #Magnitude of the phylogenetic effect
    omega ~ dunif(0,20)
    
    }
    ",fill=TRUE)

sink()
