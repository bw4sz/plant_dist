sink("model/poisson_occ_elev_repulsion.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation - count
    Y[x] ~ dpois(lambda[x])
    
    #Intensity is the probability of presence, species intercept and the phylogenetic covariance
    log(lambda[x]) = z[x] * (alpha[Plant[x]] + e[Plant[x]])
    
    #Probability of presence
    z[x] ~ dbern(psi[x])
    logit(psi[x])<- alpha2[Plant[x]] + beta[Plant[x]] * ele[x]
    
    #Residuals
    discrepancy[x] <- pow(Y[x] - lambda[x],2)/lambda[x]
    
    #Assess Model Fit
    Ynew[x] ~ dpois(lambda[x])
    discrepancy.new[x]<-pow(Ynew[x] - lambda[x],2)/lambda[x]
    
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)
    fitnew<-sum(discrepancy.new)
    
    ############
    #Prediction
    ############
    
    for(i in 1:Npreds){
    
    #predict value
    
    #Observation - count
    prediction[i] ~ dpois(lambda_new[i])
    
    #Intensity is the probability of presence, species intercept and the phylogenetic covariance
    log(lambda_new[i]) = z_new[i] * (alpha[Ypred_plant[i]] + e[Ypred_plant[i]])
    
    #Probability of presence
    z_new[i] ~ dbern(psi_new[i])
    logit(psi_new[i])<- alpha2[Ypred_plant[i]] + beta[Ypred_plant[i]] * ele_new[i]
    
    #squared predictive error
    pred_error[i] <- pow(Ypred[i] - prediction[i],2)
    }
    
    #Sum Predictive Error
    fitpred<-pred_error/Npreds
    
    #########################
    #autocorrelation in error
    #########################
    
    e[1:Plants] ~ dmnorm(zeros[],tauC[,])
    
    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda_cov * D[i,j])
    }
    }
    
    ## Covert variance to precision for each parameter, allow omega to shrink to identity matrix
    #For the sake of clarity, we would inverse again for repulsion matrix, no need since its a precision matrix
    vCov = omega*C[,] + (1-omega) * I
    tauC=vCov*gamma

    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept
    #Intercept flowering count
    alpha[j] ~ dnorm(0,0.0001)
    
    #Internet probability of presence
    alpha2[j] ~ dnorm(0,0.0001)
    
    #Effect of elevation
    beta[j] ~ dnorm(0,0.0001)
  
    } 
    
    #Autocorrelation priors
    gamma ~ dunif(0,20)
    
    #Strength of covariance decay
    lambda_cov = 5
    omega ~ dbeta(1,1)
    }
    ",fill=TRUE)

sink()
