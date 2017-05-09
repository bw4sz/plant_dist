
sink("Bayesian/NmixturePoissonRagged.jags")

cat("
    model {
    #Compute intensity for each pair of birds and plants
    for (i in 1:Birds){
    for (j in 1:Plants){
    for (k in 1:Times){
    
    #Process Model 
    #mean intensity
    
    #log transformed variance
    log(lambda[i,j,k]) <- alpha[i,j] 
    
    #For each Time - there is a latent count, log transformed intensity
    N[i,j,k] ~ dpois(lambda[i,j,k])
    }
    }
    }
    
    #Observed counts for each day of sampling at that Time
    for (x in 1:Nobs){
    
    #Observation Process
    Yobs[x] ~ dbin(detect[Bird[x]],N[Bird[x],Plant[x],Time[x]])    
    
    #Assess Model Fit
    
    #Fit discrepancy statistics
    eval[x]<-detect[Bird[x]]*N[Bird[x],Plant[x],Time[x]]
    E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
    
    ynew[x]~dbin(detect[Bird[x]],N[Bird[x],Plant[x],Time[x]])
    E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
    
    }
    
    ##Priors##
    
    #Observation model
    #Detect prior
    
    detect ~ dunif(0,1)
    
    #Process Model
    #Species level priors
    for (i in 1:Birds){
      for (j in 1:Plants){
        alpha[i,j] ~ dnorm(alpha_mu,alpha_tau)
      }
    
    #Group process priors
    
    #Intercept 
    alpha_mu ~ dnorm(0,0.01)
    alpha_tau ~ dt(0,1,1)I(0,)
    alpha_sigma<-pow(1/alpha_tau,0.5) 
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])

    }
    ",fill=TRUE)

sink()
