#====================================================================
# purpose: Examples and exercises in Chapter 3 in Koopman and Commandeur (2007)
# author: tirthankar chakravarty
# comments:
# TODO:
# 1. Read the chapter through.
# 2. Add functions for heteroskedasticity and AC testing to TCMisc package.
# 3. 
#====================================================================

#==========================================================
# Section 3.1: Deterministic level and slope
#==========================================================
dfKSI = readRDS("Data//Processed//dfKSI.rds")

# fit the deterministic slope and level model
ssKSI = SSModel(logDrivers ~ SSMtrend(degree = 2, Q = list(matrix(0), matrix(0))), 
                H = matrix(NA), data = dfKSI)
fitSSKSI = fitSSM(ssKSI, inits = c(1.7), method = "BFGS")
exp(fitSSKSI$optim.out$par)

# compute the filtered estimates of the states
KFS(fitSSKSI$model, filtering = "state")

# compute the log-likelihood & AIC
logLik(fitSSKSI$model)/192
(1/192)*(-2*logLik(fitSSKSI$model) +2*(2+1))
# NOTE: this does not match to the likelihood reported in the book = 0.414

# extract the residuals from the model
residKSI = dfKSI$logDrivers - predict(fitSSKSI$model)

# compute the Goldfeld-Quandt test of heteroskedasticity
# compute the normality tests
# compute the tests of independence

#==========================================================
# Section 3.2: Stochastic level and slope
#==========================================================
# fit the stochastic slope and level model
ssKSI = SSModel(logDrivers ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA))), 
                H = matrix(NA), data = dfKSI)
fitSSKSI = fitSSM(ssKSI, inits = c(1.1, 1.3, 1.7), method = "BFGS")
exp(fitSSKSI$optim.out$par)

# compute the filtered estimates of the states
dfSSKSIStates = KFS(fitSSKSI$model, filtering = "state")$alphahat

# compute the log-likelihood & AIC
logLik(fitSSKSI$model)/192
(1/192)*(-2*logLik(fitSSKSI$model) +2*(2+1))

# extract the residuals from the model
residKSI = dfKSI$logDrivers - predict(fitSSKSI$model)

# compute the Goldfeld-Quandt test of heteroskedasticity
# compute the normality tests
# compute the tests of independence

# plot the trend, slope and irregular component of the model
dfSSKSI = data.frame(date = dfKSI$Date, resid = as.numeric(residKSI), dfSSKSIStates)
dfSSKSILong = melt(dfSSKSI, id.vars = "date")

ggplot(dfSSKSILong, aes(x = date, y = value, color = variable)) +
  geom_line() + facet_wrap(~variable, nrow = 3, scales = "free") 

#==========================================================
# Section 3.3: Stochastic level and deterministic slope
#==========================================================
# fit the deterministic slope and stochastic level model
ssKSI = SSModel(logDrivers ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(0))), 
                H = matrix(NA), data = dfKSI)
fitSSKSI = fitSSM(ssKSI, inits = c(1.1, 1.3), method = "BFGS")
exp(fitSSKSI$optim.out$par)

# compute the filtered estimates of the states
dfSSKSIStates = KFS(fitSSKSI$model, filtering = "state")$alphahat

# compute the log-likelihood & AIC
logLik(fitSSKSI$model)/192
(1/192)*(-2*logLik(fitSSKSI$model) +2*(2+1))

# extract the residuals from the model
residKSI = dfKSI$logDrivers - predict(fitSSKSI$model)

# compute the Goldfeld-Quandt test of heteroskedasticity
# compute the normality tests
# compute the tests of independence

# plot the trend, slope and irregular component of the model
dfSSKSI = data.frame(date = dfKSI$Date, resid = as.numeric(residKSI), mu = dfSSKSIStates[, 1])
dfSSKSILong = melt(dfSSKSI, id.vars = "date")

ggplot(dfSSKSILong, aes(x = date, y = value, color = variable)) +
  geom_line() + facet_wrap(~variable, nrow = 2, scales = "free") 

#==========================================================
# Section 3.4: local linear trend model for Finnish casualties
#==========================================================
dfNorway$logFinland = log(dfNorway$Finland)

# construct the model
ssFin = SSModel(logFinland ~ SSMtrend(
  degree = 2, Q = list(matrix(NA), matrix(NA))), H = matrix(NA), data = dfNorway)

# fit the model
fitSSFin = fitSSM(ssFin, inits = c(1.1, 1.2, 1.3))

# print the model parameters
exp(fitSSFin$optim.out$par)

# get the filtered states
dfSSFin = KFS(fitSSFin$model, filtering = "state")$alphahat

# compute the log-likelihood & AIC
logLik(fitSSFin$model)/192
(1/192)*(-2*logLik(fitSSKSI$model) +2*(2+1))

#==========================================================
# Section 3.4: stochastic slope + deterministic level for Finnish casualties
#==========================================================
# construct the model
ssFin = SSModel(logFinland ~ SSMtrend(
  degree = 2, Q = list(matrix(0), matrix(NA))), H = matrix(NA), data = dfNorway)

# fit the model
fitSSFin = fitSSM(ssFin, inits = c(1.1, 1.2))

# print the model parameters
exp(fitSSFin$optim.out$par)

# get the filtered states
dfSSFin = KFS(fitSSFin$model, filtering = "state")$alphahat

# compute the log-likelihood & AIC
logLik(fitSSFin$model)/192
(1/192)*(-2*logLik(fitSSKSI$model) +2*(2+1))


