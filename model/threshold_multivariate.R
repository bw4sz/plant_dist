sink("model/threshold_multivariate.jags")
cat("
    model {
    
    for (x in 1:Nobs){
    
    #Observation of a flowering plant
    Y[x] ~ dbern(p[x])
    logit(p[x]) <-  e[Plant[x],Site[x],Month[x]]
    
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
    logit(p_new[x])<-  e[NewPlant[x],NewSite[x],NewMonth[x]]
    
    #predictive error
    pred_error[x] <- abs(Ypred[x] - p_new[x])
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
    
    #Priors
    
    #Species level priors
    
    for (j in 1:Plants){
    for (k in 1:Sites){
    #Intercept flowering probability
    alpha[j,k] ~ dnorm(0,0.386)

    }
    } 
    
    #Covariance wishart prior
    tauC[1:Plants,1:Plants] ~ dwish(I,Plants+2)
    
    }
    ",fill=TRUE)

sink()
