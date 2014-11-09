#######################################################################
  ## Function to evaluate log likelihood of a state space model ##
#######################################################################

# Arguments: 
# y = univariate time series
# a0 = mean of initial state vector
# p0 = covariance matrix of the initial state vector
# Z = measurement equation coefficient matrix
# T = transition matrix
# H = covariance matrix of the observation equation disturbances
# Q = covariance matrix of the state disturbances

# TO DO:
# 1. Make the system matrices time varying
# 2. Include Rt in the state equation

llSSF <- function (y, a0, p0, Z, T, H, Q) {
    
    m <- length(a0)

    ## ## TO DO: change this
    ## T <- phi[1:m,1:m]
    ## Z <- phi[m+1,]
    ## P <- sigma[1:m,1:m]
    
    ## save estimates
    a <- rbind(a0, matrix(0, nrow = NROW(y),
                          ncol = length(a0))) 
    
    p <- vector(mode = "list", length = NROW(y) + 1)
    p[[1]] <- p0
    
    ## optimal estimators for state variables
    a.pred <- matrix(0, nrow = NROW(y), ncol = m)
    ## covariance matrix of the estimation error
    p.pred <- vector(mode = "list", length = NROW(y)) 
    
    ## CHECK: Ft is a scalar in case of a univariate time series
    Ft <- vector(mode = "list", length = NROW(y))
    
    ## prediction error
    vt <- rep(NA, NROW(y))
    
    ## error variances
    ## Q <- diag(omega[1,1], nrow = m, ncol = m)
    ## H <- matrix(omega[2,2])
    
    for (i in 1:length(y)) {
        
        ## prediction equations
        a.pred[i,] <-  T %*% a[i,] 
    
        p.pred[[i]] <- T %*% p[[i]] %*% t(T) + Q 

        ## CHECK THIS
        ## covariance matrix of the conditional distribution of y
        Ft[[i]] <- Z %*% p.pred[[i]] %*% t(Z) + H
        
        ## updating equations
        ## TO DO: check the formula (t(Z))
        a[i+1,] <- a.pred[i,] + p.pred[[i]] %*% t(Z) %*%
            solve(Ft[[i]]) %*% 
                (y[i] - (Z %*% a.pred[i,]))

        ## CHECK: covariance of estimation error
        p[[i+1]] <- p.pred[[i]] -
            p.pred[[i]] %*% t(Z) %*% solve(Ft[[i]]) %*% Z %*%
                p.pred[[i]]  
        
        ## prediction error
        vt[i] <- y[i] - Z %*% a.pred[i,]
    }    

    ## TO DO: change this
    Ft <- unlist(Ft)

    ## log likelihood of ssf 
    ## TO DO: Check this (conflict in values from dlm)
    ll <- - ((NROW(y)/2) * (log(2*pi)) - (0.5 * sum(log(abs(Ft)))) -
             (0.5 * sum(((vt*vt)/Ft))))
    
    return(ll)
}

## optim(par = c(100, 200), llSSF, y = rnorm(5,5,3), m = 2,
##       method = "L-BFGS-B") 


## ## ssf pack matrix representation of ssm
## getSSF <- function (a0, p0, Z, H, T, Q) {
##     sigma <- rbind(p0, t(a0))
##     phi <- rbind(T, Z)
## ## TO DO: change this
##     ##omega <- diag(0, nrow = m, ncol = m) 
##     omega <- diag(c(Q, H), nrow = m, ncol = m)
##     retVal <- list(phi, omega, sigma)
##     names(retVal) <- c("phi", "omega", "sigma")
##     return(retVal)
## }

## y = rnorm(6,5,3)
## m <- 2 # dim of the state vector
## a0 <- c(1,2)
## p0 <- diag(1000, nrow = m, ncol = m)
## Z = matrix(1, nrow = 1, ncol = m)
## H = 100
## T = diag(1, nrow = m, ncol = m)
## Q = diag(c(110,200), nrow = m, ncol = m)
    
