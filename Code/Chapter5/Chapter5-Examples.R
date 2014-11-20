#====================================================================
# purpose: Examples and exercises in Chapter 5 in Koopman and Commandeur (2007)
# author: tirthankar chakravarty
# comments: 
#   1. Assess the relationship between KSI drivers in the UK over time and 
#   petrol prices
#   2. Note that predict called on the fitted SSModel object returns the predicted
#   values.
# TODO:
#   1. 
#====================================================================

rm(list = ls())
gc()

dfKSI = readRDS("Data/Processed/dfKSI.rds")

#==========================================================
# 5.1 Deterministic level and explanatory variable
#==========================================================
lmKSI = lm(logDrivers ~ logPetrol, data = dfKSI)
summary(lmKSI)

# implement the same thing using KFAS
ssPetrol = SSModel(logDrivers ~ SSMregression( ~ -1 + logPetrol, data = dfKSI, Q = matrix(0)), 
                   H = matrix(NA), data = dfKSI)
fitSSPetrol = fitSSM(ssPetrol, inits = c(1.2), method = "BFGS")
exp(fitSSPetrol$optim.out$par)

KFS(fitSSPetrol$model, filtering = "state")

# plot the actual versus predicted 
ggplot(data.frame(Date = dfKSI$Date, 
                  actual = dfKSI$logDrivers, 
                  predicted = as.numeric(predict(fitSSPetrol$model))),
       aes(x = Date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  theme_bw() + 
  scale_color_manual(name = "Actual/Predicted", 
                     values = c("Actual" = "red", "Predicted" = "blue"))

# plot the residuals and horizontal line
ggplot(data.frame(residual = as.numeric((dfKSI$logDrivers - predict(fitSSPetrol$model))), 
                  Date = dfKSI$Date), aes(x = Date, y = residual)) +
  geom_point() + geom_hline()

# plot the bivariate relationship together with a regression line
ggplot(dfKSI, aes(x = logPetrol, y = logDrivers)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) 

#==========================================================
# 5.2 Stochastic level and explanatory variable
#==========================================================
ssPetrol = SSModel(logDrivers ~ SSMtrend(degree = 1, Q = matrix(NA)) +
                     SSMregression( ~ -1 + logPetrol, data = dfKSI, Q = matrix(NA)), 
                   H = matrix(NA), data = dfKSI)
fitSSPetrol = fitSSM(ssPetrol, inits = c(1.2, 1.1, 1.3), method = "BFGS")
# fitted variances
exp(fitSSPetrol$optim.out$par)

# filetered estimates of the underlying states (level, and time-varying coefficient)
statesPetrol = KFS(fitSSPetrol$model, filtering = "state")$alphahat

# plot the actual versus predicted 
ggplot(data.frame(Date = dfKSI$Date, 
                  actual = dfKSI$logDrivers, 
                  predicted = as.numeric(predict(fitSSPetrol$model))),
       aes(x = Date)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  theme_bw() + 
  scale_color_manual(name = "Actual/Predicted", 
                     values = c("Actual" = "red", "Predicted" = "blue")) + 
  ylab("log(KSI)")

# plot the filtered values of the state variables
statesPetrolLong = melt(data.frame(Date = dfKSI$Date, 
                                   KFS(fitSSPetrol$model, filtering = "state")$alphahat),
                        id.vars = "Date")
ggplot(statesPetrolLong, aes(x = Date, y = value, color = variable)) +
  geom_line() + facet_wrap(~ variable, scales = "free", ncol = 1) +
  theme_bw() + 
  ylab("State")
