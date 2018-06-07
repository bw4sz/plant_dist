sink("model/julian.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    log(phi[x])<-alpha[Plant[x]]  + beta[Plant[x]] * julian[x] + beta2[Plant[x]] * julian[x]^2
    
    Yobs[x] ~ dpois(phi[x])
    
    }
    

    #Assess Model Fit
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
      #Intercept
      #poisson regression prior
      beta[j] ~ dnorm(0,0.0001)
      beta2[j] ~ dnorm(0,0.0001)
      alpha[j] ~ dnorm(0,0.0001)
    } 
    
    }
    ",fill=TRUE)

sink()
