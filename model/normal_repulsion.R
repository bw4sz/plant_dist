sink("model/normal_repulsion.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #observation
    mu[x] <- alpha + e[Plant[x]]
    Yobs[x] ~ dnorm(mu[x],tau[Plant[x]])T(0,365)
    
    #Residuals
    residuals[x] <- Yobs[x] - mu[x]
    
    #squared error
    sq[x]<-pow(residuals[x],2)
    
    #Assess Model Fit - squared residuals
    Ynew[x] ~ dnorm(mu[x],tau[Plant[x]])T(0,365)
    sq.new[x]<-pow(Ynew[x] - mu[x],2)
    
    }
    
    #sum of squared error
    #Root mean squared error
    fit<-sqrt(sum(sq[])/Nobs)
    fitnew<-sqrt(sum(sq.new[])/Nobs)
    
    #autocorrelation in error
    e[1:Plants] ~ dmnorm(zeros[],tauC[,])
    
    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda * D[i,j])
    }
    }
    
    #Inverse matrix correlation for repulsion - see Ives and Helmus 2011
    #For clarity sake this is performed, even though it is undone by converting to precision
    iC=inverse(omega * C[,])
    
    ## Covert variance to precision for each parameter
    
    iiC=inverse(iC[,])
    tauC=iiC
    tauC2=iiC
    
    ###########
    #Prediction
    ###########

    for(i in 1:Npreds){
    
    #predict value
    prediction[i] ~ dnorm(mu[Ypred_plant[i]],tau[Ypred_plant[i]])T(0,365)
    
    #squared predictive error
    pred_error[i] <- pow(Ypred[i] - prediction[i],2)
    }
    
    #Root Mean Squared Predictive Error
    fitpred<-sqrt(sum(pred_error[])/Npreds)
    
    # Priors #
    ##########
    
    #Julian Intercept
    alpha ~ dnorm(0,0.0001)
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #variance
    sigma[j] ~ dunif(0,75)
    tau[j] <- pow(sigma[j], -2)
    } 
    
    #Strength of covariance decay
    lambda ~ dunif(0,5)
    omega ~ dunif(0,20)
    }
    ",fill=TRUE)

sink()
