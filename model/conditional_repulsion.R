sink("model/conditional_attraction.jags")
cat("
    model {

    for (x in 1:Nobs){

    #Observation of a flowering plant
    Y[x] ~ dbern(psi[x])

    #Conditional probability flowering|occurrence
    psi = z[x] * rho[Plant[x]]

    #probability of occurrence
    z[x] ~ dbern(occ[x])
    logit(occ[x]) <-  alpha[Plant[x]] + beta[Plant[x]] * elevation[x]

    #Residuals
    discrepancy[x] <- abs(Y[x] - p[x])

    #Assess Model Fit
    Ynew[x] ~ dbern(p[x])
    discrepancy.new[x]<-abs(Ynew[x] - p[x])
    }

    #Sum discrepancy
    fit<-sum(discrepancy)/Nobs
    fitnew<-sum(discrepancy.new)/Nobs

    #Prediction, same model as above, different index.

    for(x in 1:Npreds){

    prediction[x] ~ dbern(psi_new[x])
    psi_new = z_new[x] * rho[NewPlant[x],NewMonth[x]]

    #probability of occurrence
    z_new[x] ~ dbern(occ_new[x])
    logit(occ_new[x])<- alpha[NewPlant[x]] + beta[NewPlant[x]] * new_elevation[x]

    #predictive error
    pred_error[x] <- abs(Ypred[x] - p_new[x])
    }

    #Predictive Error
    fitpred<-sum(pred_error)/Npreds

    #Priors
    #########################
    #autocorrelation in error
    #########################

    #For each of observation
    for(x in 1:Months){
    rho[1:Plants,x] ~ dmnorm(zeros,tauC[,])
    }

    ##covariance among similar species
    for(i in 1:Plants){
    for(j in 1:Plants){
    C[i,j] = exp(-lambda_cov * D[i,j])
    }
    }

    ## Covert variance to precision for each parameter, allow omega to shrink to identity matrix
    vCov = omega*C[,] + (1-omega) * I
    tauC=vCov*gamma

    #Species level priors

    for (j in 1:Plants){
      #elevation occurrence priors
      alpha[j] ~ dnorm(0,0.386)
      beta[j] ~ dnorm(0,0.386)
    }

    #Autocorrelation priors
    gamma  ~ dunif(0,5)

    #Strength of covariance decay
    lambda_cov ~ dunif(0,5)
    omega = 1

    }
    ",fill=TRUE)

sink()
