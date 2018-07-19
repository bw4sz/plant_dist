sink("model/intercept_interactions_detection.jags")
cat("
    model {
    
    #Latent Interaction probability
    for (i in 1:Birds){
      for (j in 1:Plants){
        #P(Interaction)
        logit(phi[i,j])<-alpha[Bird[i],Plant[j]] 
      } 
    }

#    #Additional elevation ranges - estimated from full dataset?
#    for(i in 1:Birds){
#    for(j in 1:AllCameras){
#      logit(p_elev[i,j]) = alpha_elev[i] + beta_elev[i] * ele[j]
#      Yele[i,j] ~ dbern(p_elev[i,j])
#    }
#    }
    
    
    # Bird presence at each camera (placed on a single flower Plant (j))
    for (i in 1:Birds){
      for( j in 1:Plants){
        for(k in 1:Cameras){

          #Presence
          logit(p_camera[i,j,k]) <- omega[i] +  beta[i] * ele[k]
          occ[i,j,k] ~ dbern(p_camera[i,j,k])

          #Latent Interaction State for each camera, conditional on presence
          eff.phi[i,j,k] = occ[i,j,k] * phi[i,j]
          z[i,j,k]~dbern(eff.phi[i,j,k])
              } 
          }
      }
    
    #Observed interactions

    for (x in 1:Nobs){

      #Probability of interaction, conditional on detection and presence
      psi[x] = z[Bird[x],Plant[x],Camera[x]] * phi[Bird[x],Plant[x]]
  
      #Interaction Observation
      Yobs[x] ~ dbern(psi[x])
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    
      #Flat detection prior
      omega[i] ~ dnorm(0,0.386)
  
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
