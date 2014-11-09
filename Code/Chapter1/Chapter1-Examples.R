#===============================================================================
# purpose: recreate the empirical examples in Chapter 1 of Koopman & Commandeur (2007)
# author: tirthankar chakravarty
# comments:
# TODO:
# 1. Add the ACF plots in ggplot2
# 2. Arrange the plots using gridExtra
# 3. Plot the residuals with shading to indicate intensity
#===============================================================================

dfKSI = loadRDS("Data/Processed/dfKSI.rds")

#================================================
# Figure 1.1: 
#================================================
# regression fit
lmKSI = lm(logDrivers ~ Date, data = dfKSI)
dfKSI$residKSI = resid(lmKSI)
dfKSI$posKSI = sign(dfKSI$residKSI)

ggKSI = ggplot(dfKSI, aes(x = Date, y = logDrivers)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE) 

# add the residuals
ggKSIResid = ggplot(dfKSI, aes(x = Date, y = residKSI)) + 
  geom_bar(aes(y = residKSI, fill = residKSI), stat = "identity",
           position = "identity") +
  scale_fill_continuous(name = "Residuals")

# add the correlogram of the residuals
acfResidKSI = qacf(dfKSI$residKSI, lag.max = 12)

# put together
grid.arrange(arrangeGrob(ggKSI, ggKSIResid, acfResidKSI))
