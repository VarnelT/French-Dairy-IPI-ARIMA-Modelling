# ***************************************************************************
# ******************** TIME SERIES PROJECT ***************************
# ***************************************************************************

# Authors : Varnel TIENTCHEU et Rivalien MAGUETSWET 

rm(list=ls(all=TRUE))

#Chargement des packages 
#install.packages("strucchange")
#install.packages("changepoint")
#install.packages("patchwork")
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
library(forecast)


# Chargement et nettoyage de la base 
setwd("C:/Users/pc hp/Desktop/projet/French-Dairy-IPI-ARIMA-Modelling")
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
checkresiduals(model5,lag = 12)


# **************************************************************************#
#                                                                           #
#                       PART III: Forecasting                               #
#              Selected model: ARIMA(1,1,1) on lxt                          #
#                                                                           #
# **************************************************************************#
#   lxt     : log-transformed ts  (log(Xt))
#   model5  : arima(dlxt, c(1,0,1), method="ML")   <=> ARIMA(1,1,1) on lxt


# =========================================================================
# Retrieve model parameters
# =========================================================================

# Re-estimate directly on lxt so that predict() returns level-log forecasts
final_model <- arima(lxt, order = c(1, 1, 1), method = "ML")

phi1   <- final_model$coef["ar1"]     # AR(1) coefficient
theta1 <- final_model$coef["ma1"]     # MA(1) coefficient
sigma2 <- final_model$sigma2          # innovation variance

T <- length(lxt)

cat("=== Selected model: ARIMA(1,1,1) on ln(Xt) ===\n")
cat(sprintf("phi1   = %.4f\n", phi1))
cat(sprintf("theta1 = %.4f\n", theta1))
cat(sprintf("sigma2 = %.6f  (sigma = %.4f)\n", sigma2, sqrt(sigma2)))
cat(sprintf("Series length T = %d\n\n", T))

# =========================================================================
# Questions 6 & 7 — Confidence region for (X_{T+1}, X_{T+2})
# =========================================================================
#
# For an ARIMA(1,1,1), Yt = diff(lxt) follows an ARMA(1,1):
#   Y_t = phi1*Y_{t-1} + eps_t + theta1*eps_{t-1}
#
# The h-step-ahead forecast errors (on Yt) are:
#   e(T+1|T) = eps_{T+1}
#   e(T+2|T) = eps_{T+2} + (phi1 + theta1)*eps_{T+1}
#
# Converting back to lxt (cumulating differences):
#   err_lxt(T+1|T) = eps_{T+1}
#   err_lxt(T+2|T) = eps_{T+1}*(1 + phi1 + theta1) + eps_{T+2}
#
# => Variance-covariance matrix of (err_{T+1}, err_{T+2}):
#
#   Var(err_{T+1})            = sigma2
#   Var(err_{T+2})            = sigma2 * [1 + (1 + phi1 + theta1)^2]
#   Cov(err_{T+1}, err_{T+2}) = sigma2 * (1 + phi1 + theta1)

psi1  <- 1 + phi1 + theta1     # coefficient of eps_{T+1} in err_{T+2}

Var1  <- sigma2
Var2  <- sigma2 * (1 + psi1^2)
Cov12 <- sigma2 * psi1

Sigma <- matrix(c(Var1,  Cov12,
                  Cov12, Var2), nrow = 2, byrow = TRUE)

cat("=== Variance-Covariance matrix Sigma of forecast errors ===\n")
print(round(Sigma, 8))

cat("\nEigenvalues of Sigma (ellipse axes):\n")
eig <- eigen(Sigma)
print(round(eig$values, 8))
cat("Eigenvectors:\n")
print(round(eig$vectors, 4))

# =========================================================================
# Point forecasts (centre of the ellipse)
# =========================================================================

pred <- predict(final_model, n.ahead = 2, se.fit = TRUE)
hat_lxt_T1 <- as.numeric(pred$pred[1])
hat_lxt_T2 <- as.numeric(pred$pred[2])
centres <- c(hat_lxt_T1, hat_lxt_T2)

cat(sprintf("\n=== Point forecasts (log scale) ===\n"))
cat(sprintf("  hat_ln(X_{T+1}) = %.4f  =>  hat_X_{T+1} = %.2f\n",
            hat_lxt_T1, exp(hat_lxt_T1)))
cat(sprintf("  hat_ln(X_{T+2}) = %.4f  =>  hat_X_{T+2} = %.2f\n",
            hat_lxt_T2, exp(hat_lxt_T2)))

# =========================================================================
# Question 8 — Graphical representation of the 95% confidence region
# =========================================================================

alpha <- 0.05
ell   <- ellipse(Sigma, centre = centres, level = 1 - alpha, npoints = 10000)

x_range <- range(ell[,1])
y_range <- range(ell[,2])
pad <- 0.003

png("Ellipse_Confidence_Region_95.png", width = 800, height = 680, res = 110)

plot(ell,
     type = "l",
     col  = "steelblue",
     lwd  = 2,
     xlim = c(x_range[1] - pad, x_range[2] + pad),
     ylim = c(y_range[1] - pad, y_range[2] + pad),
     xlab = expression(ln(X[T+1]) ~ "(forecast)"),
     ylab = expression(ln(X[T+2]) ~ "(forecast)"),
     main = "95% Joint Confidence Region\nARIMA(1,1,1) — French Dairy IPI (log scale)")

# Fill the ellipse
polygon(ell, col = adjustcolor("steelblue", alpha.f = 0.15), border = NA)
lines(ell, col = "steelblue", lwd = 2)

# Central forecast point
points(centres[1], centres[2],
       pch = 23, bg = "firebrick", col = "firebrick", cex = 2, lwd = 2)

# Marginal 95% prediction intervals (for reference)
z <- qnorm(1 - alpha/2)
abline(v = centres[1] + c(-z, z) * sqrt(Var1),
       lty = 2, col = "darkgreen", lwd = 1.3)
abline(h = centres[2] + c(-z, z) * sqrt(Var2),
       lty = 2, col = "darkorange", lwd = 1.3)

legend("topright",
       legend = c("95% Joint Region (ellipse)",
                  "Central forecast (T+1, T+2)",
                  "Marginal 95% CI — T+1",
                  "Marginal 95% CI — T+2"),
       col    = c("steelblue", "firebrick", "darkgreen", "darkorange"),
       lty    = c(1, NA, 2, 2),
       pch    = c(NA, 23, NA, NA),
       pt.bg  = c(NA, "firebrick", NA, NA),
       lwd    = c(2, 2, 1.3, 1.3),
       bg     = "white", cex = 0.85)

dev.off()
cat("-> Ellipse plot saved: 'Ellipse_Confidence_Region_95.png'\n")

# =========================================================================
# Standard univariate forecast plot (on lxt)
# =========================================================================

png("Forecast_Plot_lxt.png", width = 900, height = 450, res = 110)
plot(forecast(final_model, h = 2, level = 95),
     main = "2-step Forecast — ARIMA(1,1,1)\nFrench Dairy IPI (log scale)",
     xlab = "Year", ylab = "ln(IPI)")
dev.off()
cat("-> Forecast plot saved: 'Forecast_Plot_lxt.png'\n")

# =========================================================================
# Question 9 — Open question: improving X_{T+1} using Y_{T+1}
# =========================================================================
cat("
=== Question 9 — Open Question ===

Let Yt be a stationary time series available from t=1 to T, where Y_{T+1}
becomes available before X_{T+1}.

CONDITIONS for Y_{T+1} to improve the forecast of X_{T+1}:

1. GRANGER CAUSALITY:
   Y must Granger-cause X, i.e. Y_t must carry predictive information about
   X_{t+1} beyond the past of X alone:
     E[X_{T+1} | X_T, ..., Y_T, ...] ≠ E[X_{T+1} | X_T, ...]

2. CONTEMPORANEOUS CROSS-CORRELATION:
   Cov(X_{T+1}, Y_{T+1} | past) ≠ 0

Testing procedure:

Test (1) — Granger causality:
  In a bivariate VAR(k) model (Xt, Yt):
    H0: Y does not Granger-cause X  <=>  all Y coefficients in X's equation = 0
  In R: grangertest(lxt ~ lyt, order = k)   [lmtest package]

Test (2) — Cross-correlation function:
  ccf(dlxt, dlyt, lag.max = 12)
  => test significance at lag = 0 (and nearby lags)

If both tests reject H0, Y_{T+1} can be included as an exogenous regressor
in an ARIMAX model:
  arima(lxt, order = c(1,1,1), xreg = lyt)
and the forecast of X_{T+1} will be improved in terms of mean squared error.
")