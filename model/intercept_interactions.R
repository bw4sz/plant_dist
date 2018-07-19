sink("model/intercept_interactions.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    logit(phi[x])<-alpha[Bird[x],Plant[x]] 
    
    Yobs[x] ~ dbern(phi[x])
    
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    for (j in 1:Plants){
    
    #Intercept
    #logit prior, then transform for plotting
    alpha[i,j] ~ dnorm(0,0.386)
    } 
    }

    }
    ",fill=TRUE)

sink()
