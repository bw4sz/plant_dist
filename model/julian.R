sink("model/julian.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    log(phi[x])<-alpha[Plant[x]]  + beta[Plant[x]] * julian[x] + beta2[Plant[x]] * julian[x]^2
    
    Yobs[x] ~ dpois(phi[x])
    
    }
    
    #phylogenetic covariance among plants
    #covert variance to precision
    #tauC[1:Plants,1:Plants]=inverse((sigma^2)*C[,])
    
    #e[1:Plants] ~ dmnorm(zeros[],tauC[,])
    
    #Effect of phylogeny, calculate pagels lambda using variance-covariance matrix and identity matrix
    #C[1:Plants,1:Plants] = lambda * vcov[,] + (1-lambda) * I[,]
    
    #Assess Model Fit
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
      #Intercept
      #poisson regression prior
      beta[j] ~ dnorm(0,0.0001)
      beta2[j] ~ dnorm(0,0.0001)
      alpha[j] ~ dnorm(0,0.0001)
    } 
    
    #pagels lambda
    #lambda ~ dbeta(1,1)
    
    #variance in phylogenetic effect
    #sigma ~ dunif(0,10)
    
    #Prediction


    }
    ",fill=TRUE)

sink()
