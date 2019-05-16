sink("model/poisson_interactions.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    Yobs[x] ~ dpois(alpha[Bird[x],Plant[x]] )
    
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    for (j in 1:Plants){
    
    #Intercept
    #logit prior, then transform for plotting
    alpha[i,j] ~ dnorm(0,0.001)
    } 
    }

    }
    ",fill=TRUE)

sink()
