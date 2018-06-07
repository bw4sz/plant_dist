sink("model/julian.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    mu[x]<-alpha[Plant[x]]
    
    Yobs[x] ~ dnorm(mu[Plant[x]],sigma[Plant[x]])
    
    }
    
    #species-specific responses to julian day. Polynomial model.
    #autocorrelation in error
    e[1:Plants] ~ dmnorm(beta.mu[],tauC[,])

    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda * D[i,j])
    }
    }
    
    ## Since the response to julian is X^2 polynomial, need covariance effects on both terms.
    ## These terms share everything except for the scaling factor sigma. The decay of effect is the same. 
    ## covert variance to precision for each parameter
    
    iC=inverse(C[,])
    
    tauC=iC
    tauC2=iC
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    alpha[j] ~ dnorm(0,0.0001)
    } 
    
    #Strength of covariance decay
    lambda ~ dunif(0,5)

    }
    ",fill=TRUE)

sink()
