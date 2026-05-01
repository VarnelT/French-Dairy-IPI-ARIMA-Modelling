# ***************************************************************************
# ******************** TIME SERIES PROJECT ***************************
# ***************************************************************************

# Authors : Varnel TIENTCHEU et Rivalien MAGUETSWET 

rm(list=ls(all=TRUE))

#Chargement des packages 
install.packages("strucchange")
library(urca)
library(lmtest)
library(readr)
library(zoo)
library(tseries)
library(forecast)
library(ellipse)
library(strucchange)
library(changepoint)
library(ggplot2)
library(ggcorrplot)
library(patchwork)

# Chargement et nettoyage de la base 
valeurs_mensuelles <- read_delim("valeurs_mensuelles.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
base <- valeurs_mensuelles[-c(1:3),c(1:2)]
colnames(base) <- c("t","Xt")

# Conversion en Date (Format YYYY-MM)
#base$t <- as.Date(paste0(base$t, "-01"), format="%Y-%m-%d")
base$Xt <- as.numeric(base$Xt)

# On réordonne la série
base <- base[order(base$t), ]
View(base)

# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE I : DATA                                   #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# ============================= Question 1 ================================ #
# ========================================================================= #

# Logarithmic transformation
xt <- ts(base$Xt, start = c(1990, 1), frequency = 12)
lxt <- log(xt)

# Extract trend with a moving average
trend <- ma(lxt, order = 12, centre = TRUE)

par(mfrow=c(1,1))
# Visualizing the series
plot(lxt, 
     ylab = "Log-Indice", 
     xlab = "period",
     col = "gray",
     lwd = 2,
     ylim = c(3.5, 5.4))

lines(trend, col = "firebrick", lwd = 1) 

legend("bottomleft", 
       legend = c("Log(Xt)", "Trend"),
       col = c("gray", "firebrick"), 
       lty = 1, 
       lwd = c(1, 1))

# Visualizing the series without trend
plot(lxt-trend,
     ylab = "log(Xt)-Trend", 
     xlab = "t",
     col = "gray",
     lwd = 1,
     ylim = c(-0.3, 0.3))

#mvt de fond de Xt-trend
tr <- ma(lxt-trend, order = 12, centre = TRUE)
lines(tr, col = "firebrick", lwd = 1)

legend("bottomleft", 
       legend = c("Log(Xt)-trend"),
       col = c("gray"), 
       lty = 1, 
       lwd = c(1, 1))

## Montplot and Boxplots to check for seasonality
par(mfrow=c(1,2))
boxplot(lxt~cycle(lxt),col="lightblue",
        pch=20,cex=0.5, 
        ylab = "IPI index", xlab = "Month")
monthplot(lxt, xlab = "Month")


# ========================================================================= #
# ============================= Question 2 ================================ #
# ========================================================================= #


#---------------------test de changement structurel----------------------------------------------------
n=length(lxt)
t=time(1:n)
cumsum=efp(lxt~t,type="OLS-CUSUM")
plot(cumsum,xlab="year", main="CUSUM test IPI index",col="blue")

sctest(cumsum) # TEST DE CUSUM
#-------------------------------------------------------------------

            #--- 1. TEST AUGMENTED DICKEY-FULLER (ADF) ---
adf_trend <- ur.df(lxt, type = "trend", lags = 12, selectlags = "BIC")
summary(adf_trend)
            # ADF with(Type = "drift")
adf_drift <- ur.df(lxt, type = "drift", lags = 12, selectlags = "BIC")
summary(adf_drift)
            # ADF without drift nor trend (Type = "none")
adf_none <- ur.df(lxt, type = "none", lags = 12, selectlags = "BIC")
summary(adf_none)

           #--- 2. PHILLIP-PERRON TEST ---
pp_test <- ur.pp(lxt, type = "Z-tau", model = "constant", lags = "short") 
summary(pp_test)

          #--- 3. KPSS TEST ---
kpss_test <- ur.kpss(lxt, type="tau", lags="short", use.lag = 24)
summary(kpss_test)

          #--- 3. Zivot and Andrews Test (Because there are breakpoints) ---
xt_intercept <- ur.za(lxt, model = "intercept")
summary(xt_intercept)

xt_trend <- ur.za(lxt, model = "trend")
summary(xt_trend)

xt_both <- ur.za(lxt, model = "both")
summary(xt_both)

# ========================================================================= #
# ============================= Question 3 ================================ #
# ========================================================================= #
   
dlxt <- diff(lxt)

par(mfrow = c(1, 2)) 
plot(lxt, col = "blue", ylab = "Log-Index", main="Before", ylim = c(3.5, 5.4))
plot(dlxt, col = "gray", main="After", ylim = c(-0.3, 0.3))
abline(h = 0, lty = 2)

lines(ma(dlxt, order = 12, centre = TRUE), col = "firebrick", lwd = 1) 

legend("bottomleft", 
       legend = c("diff.log(Xt)", "trend"),
       col = c("gray", "firebrick"), 
       lty = 1, 
       lwd = c(1, 1))


# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE II : ARMA Models                           #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# ============================= Question 1 ================================ #
# ========================================================================= #

# ACF and PACF
par(mfrow = c(1, 2))
aacf <- ggAcf(dlxt, lag.max = 50, plot = T)
ppacf <- ggPacf(dlxt, lag.max = 50, plot = T)
cb <- aacf + ppacf
cb

# function to perfom the Ljung-Box test 
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

# Funtion to test significance of coefficients
signif <- function(estim){
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}
  #----------------Model training----------------
model1= arima(dlxt,c(1,0,0), method = "ML")
model2=arima(dlxt,c(2,0,0), method = "ML")
model3=arima(dlxt,c(3,0,0), method = "ML")
model4=arima(dlxt,c(0,0,1), method = "ML")
model5=arima(dlxt,c(1,0,1), method = "ML")
model6=arima(dlxt,c(2,0,1), method = "ML")
model7=arima(dlxt,c(3,0,1), method = "ML")
model8=arima(dlxt,c(0,0,2), method = "ML")
model9=arima(dlxt,c(1,0,2), method = "ML")
model10=arima(dlxt,c(2,0,2), method = "ML")
model11=arima(dlxt,c(3,0,2), method = "ML")
#-----Coefficents checking--------------------------
signif(model1)
signif(model2)
signif(model3)
signif(model4)
signif(model5)
signif(model6)
signif(model7)
signif(model8)
signif(model9)
signif(model10)
signif(model11)

#---------Residuals analysis------------------------
Qtests(model1$residuals,24,1)
Qtests(model2$residuals,24,2)
Qtests(model3$residuals,24,3)
Qtests(model4$residuals,24,1)
Qtests(model5$residuals,24,2)
Qtests(model8$residuals,24,2)
#--------Information criteria-----------------------
aic <- AIC(model1, model2, model3, model4, model5, model6, model7)
bic <- BIC(model1, model2, model3, model4, model5, model6, model7)
which.min(bic$BIC)
which.min(aic$AIC)

   #---Analyse des résidus------
checkresiduals(model3,lag = 12)

