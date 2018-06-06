sink("model/julian_trait.jags")
cat("
    model {
    
    #observation - species specific intercept and phylogenetic covariance in julian day
    
    for (x in 1:Nobs){
    
    log(phi[x])<- alpha[Plant[x]]  + beta[Plant[x]] * julian[x] + beta2[Plant[x]] * julian[x]^2
    
    Yobs[x] ~ dpois(phi[x])
    
    }
    
    #species-specific responses to julian day. Polynomial model.
    for(x in 1:Plants){
      mu.beta[x] = beta.sp[x] 
    }

    #beta 1
    beta[1:Plants] ~ dmnorm(zeros[],tauC[,])
    beta2[1:Plants] ~ dmnorm(zeros[],tauC2[,])
    
    ##Phylogenetic covariance in effect of julian day
    #Effect of phylogeny, calculate pagels lambda using variance-covariance matrix and identity matrix
    
    C[1:Plants,1:Plants] = exp(-lambda * dist[,])
    
    ## Since the response to julian is X^2 polynomial, need phylogenetic effects on both terms.
    ## These terms share everything except for the scaling factor sigma. The phylogenetic signal is the same. 
    ## covert variance to precision for each parameter
    
    tauC[1:Plants,1:Plants]=inverse((sigma^2)*C[,])
    tauC2[1:Plants,1:Plants]=inverse((sigma2^2)*C[,])
    
    ## **************************
    ##         Priors
    ## **************************
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    alpha[j] ~ dnorm(0,0.0001)
    } 
    
    #pagels lambda
    lambda ~ dbeta(1,1)
    
    #variance in phylogenetic effect
    #beta term
    sigma ~ dunif(0,10)
    #beta2 term
    sigma2 ~ dunif(0,10)
    
    
    
    }
    ",fill=TRUE)

sink()
