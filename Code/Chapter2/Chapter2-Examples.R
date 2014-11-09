#===============================================================================
# purpose: recreate the empirical examples in Chapter 2 of Koopman & Commandeur (2007)
# author: tirthankar chakravarty
# comments:
# TODO:
# 1. Fit the deterministic level model using KFAS
# 2. Fit the stochastic level models using KFAS
#   Use KFAS with matrices directly instead of using SSMtrend() etc.
# 3. Fit a local level model to Norwegian fatalities
# Comments: 
# 1. when the update function to update the parameters of the model is
#   not supplied, then fitSSM automatically updates all the parameters of the
#   variance matrices Q and H which are NA. 
#   In so doing, it takes logs of the parameters of the covariance matrices, 
#   hence, in order to retrieve the fitted values, need to exponentiate
#   Note that this matches with the output in the book on page 16 
#===============================================================================

dfKSI = readRDS("Data/Processed/dfKSI.rds")

#==========================================================
# 1. Fit a deterministic level model to the KSI data
#==========================================================
# specify the model, and the unknown parameters
ssKSI = SSModel(logDrivers ~ SSMtrend(degree = 1, Q = list(matrix(0))), 
                H = matrix(NA),
                data = dfKSI) 

# estimate the model using MLE and diffuse initialization
fitSSKSI = fitSSM(ssKSI, inits = c(1), method = "BFGS")

# get esimates of the parameters
exp(fitSSKSI$optim.out$par)

# compute filtered estimates of the states
KFS(fitSSKSI$model, filtering = "state")

#==========================================================
# 2. Fit a local level model to the KSI data: SSMtrend
#==========================================================
# specify the model
ssKSI = SSModel(logDrivers ~ SSMtrend(degree = 1, Q = list(matrix(NA))), 
                H = matrix(NA),
                data = dfKSI) 

# estimate the unknown parameters by MLE with diffuse initialization
fitSSKSI = fitSSM(ssKSI, inits = c(1, 1), method = "BFGS")

# get estimates of the parameters
exp(fitSSKSI$optim.out$par)

# plot the filtered states
kfsKSI = KFS(fitSSKSI$model, filtering = "state")
dfKSILL = data.frame(date = dfKSI$Date, locLevelFilt = as.numeric(kfsKSI$alphahat))
ggplot(dfKSILL, aes(x = date, y = locLevelFilt)) +
  geom_line() 

# plot the actual versus the predicted
predKSI = predict(fitSSKSI$model)  
dfAvP = data.frame(actual = dfKSI$logDrivers, predicted = as.numeric(predKSI),
           date = dfKSI$Date)
dfAvPLong = melt(dfAvP, id.vars = "date")
ggplot(dfAvPLong, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Date") + ylab("(log) KSI") +
  scale_color_discrete(name = "Variable")

#==========================================================
# 3. Fit a local level model to the Norwegian fatalities data: SSMtrend
#==========================================================
# read in the data
dfNorway = readRDS("Data//Processed/dfNorway.rds")

# specify the model
ssNorway = SSModel(logFatalities ~ SSMtrend(degree = 1, Q = list(matrix(NA))), 
                H = matrix(NA),
                data = dfNorway) 

# estimate the unknown parameters by MLE with diffuse initialization
fitSSNorway = fitSSM(ssNorway, inits = c(1, 1), method = "BFGS")

# get estimates of the parameters
exp(fitSSNorway$optim.out$par)

# plot the filtered states
kfsNorway = KFS(fitSSNorway$model, filtering = "state")
dfNorwayLL = data.frame(date = dfNorway$Year, 
                        locLevelFilt = as.numeric(kfsNorway$alphahat))
ggplot(dfNorwayLL, aes(x = date, y = locLevelFilt)) +
  geom_line() 

# plot the actual versus the predicted
predNorway = predict(fitSSNorway$model)  
dfAvP = data.frame(actual = dfNorway$logFatalities, 
                   predicted = as.numeric(predNorway),
                   date = dfNorway$Year)
dfAvPLong = melt(dfAvP, id.vars = "date")
ggplot(dfAvPLong, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Date") + ylab("(log) Norway") +
  scale_color_discrete(name = "Variable")