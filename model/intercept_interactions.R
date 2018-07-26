sink("model/intercept_interactions.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    Yobs[x] ~ dbern(alpha[Bird[x],Plant[x]] )
    
    }
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    for (i in 1:Birds){
    for (j in 1:Plants){
    
    #Intercept
    #logit prior, then transform for plotting
    alpha[i,j] ~ dbeta(1,1)
    } 
    }

    }
    ",fill=TRUE)

sink()
