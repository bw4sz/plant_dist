sink("model/poisson_interactions_detection.jags")
cat("
    model {
    
    # Bird presence at each camera (placed on a single flower Plant (j))
    for (i in 1:Birds){
    for( j in 1:Plants){
    for(k in 1:Cameras){
    
    #Latent Interaction State for each camera,
    z[i,j,k]~dpois(lambda[i,j])
    } 
    }
    }
    
    #Observed interactions
    
    for (x in 1:Nobs){
    #Interaction Observation
    Yobs[x] ~ dbin(omega[Bird[x]],z[Bird[x],Plant[x],Camera[x]])
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    
    #Detection prior, group effect
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    logit(omega[i])<-dcam[i]
    dcam[i]~dnorm(omega_mu,omega_tau)
    #omega[i] ~ dbeta(1,1)

    #Intensity prior
    for (j in 1:Plants){
    
    #Flat interaction prior
    lambda[i,j] ~ dunif(0,20)
    } 
    }
    
    #Observation group prior
    omega_mu ~ dnorm(0,0.386)
    omega_tau ~ dunif(0,10)

    }
    ",fill=TRUE)

sink()
