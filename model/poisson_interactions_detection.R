sink("model/poisson_interactions_detection.jags")
cat("
    model {

    # Bird presence at each camera (placed on a single flower Plant (j))
    for (i in 1:Birds){
      for( j in 1:Plants){
        for(k in 1:Cameras){
          #Latent Interaction State for each camera
          z[i,j,k]~dpois(alpha[i,j])
              } 
          }
      }
    
    #Observed interactions
    for (x in 1:Nobs){

      #Probability of detection, conditional on interaction
      Yobs[x] ~ dbin(omega[Bird[x]],z[Bird[x],Plant[x],Camera[x]])
    }
    
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
      for (j in 1:Plants){

        #Flat interaction intensity prior
        alpha[i,j] ~ dunif(0,10)
      } 
      }
    
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    for(x in 1:Birds){
    logit(omega[x])<-dcam[x]
    dcam[x] ~ dnorm(omega_mu,omega_tau)
    }
    
    omega_mu ~ dnorm(0,0.386)
    omega_tau <- pow(omega_sigma,-2)
    omega_sigma ~ dunif(0,0.25)
    }
    ",fill=TRUE)

sink()