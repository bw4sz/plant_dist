sink("model/normal_julian.jags")
cat("
    model {
    
    for (x in 1:Nobs){

      #observation
      Yobs[x] ~ dnorm(mu[Plant[x]],tau[Plant[x]])T(0,365)
      
      #Residuals
      residuals[x] <- Yobs[x] - mu[Plant[x]]
      
      #squared error
      sq[x]<-pow(residuals[x],2)

      #Assess Model Fit - squared residuals
      Ynew[x] ~ dnorm(mu[Plant[x]],tau[Plant[x]])T(0,365)
      sq.new[x]<-pow(Ynew[x] - mu[Plant[x]],2)
    
    }
    
    #sum of squared error
    #Root mean squared error
    fit<-sqrt(sum(sq[])/Nobs)
    fitnew<-sqrt(sum(sq.new[])/Nobs)

    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    mu[j] ~ dnorm(0,0.0001)

    #variance
    sigma[j] ~ dunif(0,50)
    tau[j] <- pow(sigma[j], -2)
    } 
    
    }
    ",fill=TRUE)

sink()
