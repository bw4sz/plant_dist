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

#    #Additional elevation information from full dataset.
     for(i in 1:NElev_Obs){
        logit(p_elev[i]) <- alpha_elev[Elev_Bird[i]] + beta[Elev_Bird[i]] * Elev_ele[i]
        Elev_Y[i] ~ dbern(p_elev[i])
     }

    
    # Bird presence at each camera (placed on a single flower Plant (j))
    for (i in 1:Birds){
      for( j in 1:Plants){
        for(k in 1:Cameras){

          #Presence
          logit(p_camera[i,j,k]) <- alpha_elev[i] +  beta[i] * ele[k]
          occ[i,j,k] ~ dbern(p_camera[i,j,k])

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
