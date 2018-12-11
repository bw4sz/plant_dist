sink("model/poisson_interactions_detection.jags")
cat("
    model {
    
    #Latent Interaction probability
    for (i in 1:Birds){
    for (j in 1:Plants){
    #P(Interaction)
    logit(phi[i,j])<-alpha[Bird[i],Plant[j]] 
    } 
    }
    
    
    # Bird presence at each camera (placed on a single flower Plant (j))
    for (i in 1:Birds){
    for( j in 1:Plants){
    for(k in 1:Cameras){
    
    #Latent Interaction State for each camera, conditional on presence
    eff.phi[i,j,k] = occ[i,j,k] * phi[i,j]
    z[i,j,k]~dbern(eff.phi[i,j,k])
    } 
    }
    }
    
    #Observed interactions
    
    for (x in 1:Nobs){
    
    #Probability of detection, conditional on presence and interaction
    psi[x] = z[Bird[x],Plant[x],Camera[x]] * omega[Bird[x]]
    
    #Interaction Observation
    Yobs[x] ~ dbern(psi[x])
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    
    #Detection prior
    omega[i] ~ dbeta(1,1)
    
    #Flat elevation intercept prior
    alpha_elev[i] ~ dnorm(0,0.386)
    
    #Elevation presence priors
    beta[i] ~ dnorm(0,0.386)
    
    for (j in 1:Plants){
    
    #Flat interaction prior
    alpha[i,j] ~ dbeta(1,1)
    } 
    }
    
    }
    ",fill=TRUE)

sink()
