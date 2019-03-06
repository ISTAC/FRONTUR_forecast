# State Space Model forecast
#
# Author: Elisa M. Jorge González <elisajg0@gmail.com>

#' @title Get forecast
#' @description Executes the Basic Structural Model (BSM) with the inclusion of exogenous variables and seasonal
#' @param X Times series or set of time series to be forecast  
#' @param Y Exogenous variables
#' @param n Number of available data
#' @return mylist List of forecast of a time series or set of time series and the value of MAPE for the estimations  
#' @example 
#'   get_forecast(myseries,myexogenous_va,n=24)

get_forecast <- function(X,Y,n){
  
  # CREATION OF OUTPUT VARIABLES
  # Variable for MAPE values
  MAPE <- matrix(NA, ncol = ncol(X), nrow = 1)
  # Variable for forecast values 
  Prediction <- matrix(0, ncol = ncol(X), nrow = nrow(X))
  
  # Loop for executes BSM with the inclusion of exogenous variables and seasonal
  
  if (ncol(X) > 1)
  {for (i in 1:ncol(X))
  {
    ssKi <- SSModel(log(as.numeric(X[,i])) ~ SSMtrend(degree = 2, Q = list(matrix(NA),matrix(NA)))+
                      SSMseasonal(12, Q = matrix(NA))+
                      SSMregression(~Y, Q =matrix(0)),
                    H = matrix(NA))
    fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
    out <- KFS(fitSSKI$model, filtering = "state")
    dfSSKSIStates <- ts(out$alphahat, start = time(X)[1], frequency = 12)
    
    for (j in 1:ncol(Y))
    {
      Prediction[,i] <- Prediction[,i] + dfSSKSIStates[,j]*Y[,j]
    }
    Prediction[,i] <- Prediction[,i] + dfSSKSIStates[, ncol(Y) + 1] + dfSSKSIStates[, ncol(Y) + 3] 
    Prediction[,i] <- exp(Prediction[,i])
    
    MAPE[,i] <- (mean(abs((Prediction[1:n,i]) - as.numeric(X[1:n,i]))/as.numeric(X[1:n,i])))*100
  }
  }
  else
  {
    ssKi <- SSModel(log(X) ~ SSMtrend(degree = 2, Q = list(matrix(NA),matrix(NA)))+
                      SSMseasonal(12, Q = matrix(NA))+
                      SSMregression(~Y, Q =matrix(0)),
                    H = matrix(NA))
    fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
    out <- KFS(fitSSKI$model, filtering = "state")
    dfSSKSIStates <- ts(out$alphahat, start = time(X)[1], frequency = 12)
    
    for (j in 1:ncol(Y))
    {
      Prediction <- Prediction + dfSSKSIStates[,j]*Y[,j]
    }
    Prediction <- Prediction + dfSSKSIStates[, ncol(Y) + 1] + dfSSKSIStates[, ncol(Y) + 3] 
    Prediction <- exp(Prediction)
    
    MAPE <- (mean(abs(Prediction[1:n] - as.numeric(X[1:n]))/as.numeric(X[1:n])))*100
  }
  
  
  
  mylist <- list(MAPE,Prediction)
  names(mylist) <- c("MAPE","Prediction")
  
  return(mylist)
  
}
