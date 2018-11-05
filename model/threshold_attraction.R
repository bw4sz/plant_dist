sink("model/threshold_attraction.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation of a flowering plant
    Y[x] ~ dbern(p[x])
    logit(p[x]) <-  alpha[Plant[x]] + e[Plant[x],Site[x],Month[x]]
    
    #Residuals
    discrepancy[x] <- abs(Y[x] - p[x])
    
    #Assess Model Fit
    Ynew[x] ~ dbern(p[x])
    discrepancy.new[x]<-abs(Ynew[x] - p[x])
    }
    
    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs
    
    #Prediction
    
    for(x in 1:Npreds){
    #predict value
    
    #Observation - probability of flowering
    prediction[x] ~ dbern(p_new[x])
    logit(p_new[x])<- alpha[NewPlant[x]] + e[NewPlant[x],NewSite[x],NewMonth[x]]
    
    #predictive error
    pred_error[x] <- abs(Ypred[x] - prediction[x])
    }
    
    #Predictive Error
    fitpred<-sum(pred_error)/Npreds
    
    #########################
    #autocorrelation in error
    #########################
    
    #For each of observation
    for(y in 1:Sites){
    for(x in 1:Months){
    e[1:Plants,y,x] ~ dmnorm(zeros,tauC[,])
    }
    }

    ##covariance among similiar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda_cov * D[i,j])
    }
    }
    
    ## Covert variance to precision for each parameter, allow omega to shrink to identity matrix
    vCov = omega*C[,] + (1-omega) * I
    tauC=inverse(vCov*gamma)
    
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    
    #Intercept flowering probability
    alpha[j] ~ dnorm(0,0.386)
    
    } 
    #Autocorrelation priors
    gamma = 1
    
    #Strength of covariance decay
    lambda_cov = 2
    omega  = 1
    }
    ",fill=TRUE)

sink()
