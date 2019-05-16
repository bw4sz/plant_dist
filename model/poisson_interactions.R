sink("model/poisson_interactions.jags")
cat("
    model {
    
    #Observed interactions
    
    for (x in 1:Nobs){
    #Interaction Observation
    Yobs[x] ~ dpois(lambda[Bird[x],Plant[x]])
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    #Intensity prior
    for (j in 1:Plants){
    
    #Flat interaction prior
    lambda[i,j] ~ dunif(0,20)
    } 
    }
    
    }
    ",fill=TRUE)

sink()
