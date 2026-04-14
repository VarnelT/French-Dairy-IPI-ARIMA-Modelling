# ***************************************************************************
# ******************** Projet de s?ries temporelles 2022 ********************
# ***************************************************************************

# Auteurs : Daniel NKAMENI et H?l?ne RONDEY

rm(list=ls(all=TRUE))

#Chargement des packages 
install.packages("urca")
library(urca)
library(FitAR)
library(lmtest)
library(readr)
library(zoo)
library(fUnitRoots)
library(tseries)
library(forecast)
library(ellipse)
library(ggplot2)
library(ggcorrplot)
library(astsa)
library(portes)
library(tsoutliers)
library(expsmooth)

# Chargement et nettoyage de la base 

library(readr)
valeurs_mensuelles <- read_delim("C:/Users/quent/OneDrive/Documents/ENSAE/Linear_time_series/dnkameni_hrondey/valeurs_mensuelles_IPI_PBAD.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(valeurs_mensuelles)
base <- valeurs_mensuelles[-c(1:3),c(1:2)]
colnames(base) <- c("P?riode","Indice_CVS_CJO")
View(base)


# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE I : Les donn?es                            #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# ============================= Question 1 ================================ #
# ========================================================================= #


# Cr?ation d'une s?rie temporelle ? partir des IPI
bois_alc <- ts(as.numeric(base[[2]]), start = 1990, frequency = 12)
bois_alc <- window(bois_alc, end=c(2022,2))

# Visualisation

## S?rie temporelle (Figure 1)
qplot(y=bois_alc, x =as.yearmon(base[[1]]), geom = c("point","smooth","line"), xlab = "", ylab = "Indice de production de boissons alcooliques")

# Analyses et graphiques annexes

# Saisonnalit? (Figure 7):

## Saisonnalit? ? l'aide des droites de profil (Season plot)
ggseasonplot(bois_alc, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Indice de production de boissons alcooliques") +
  xlab("Mois") +
  ggtitle("Droites de profils mensuels")

## Season plot en coordonn?es polaires
ggseasonplot(bois_alc, polar=TRUE) + ylab("Indice de production de boissons alcooliques")+ xlab("Mois")

#On ne remarque aucun changement particulier dans l'?volution de la s?rie selon le mois.

# Boxplots mensuels (Figure 8)
## Les Boxplots mensuels permettent aussi de v?rifier la saisonnalit?
boxplot(bois_alc~cycle(bois_alc),col="lightblue",pch=20,cex=0.5, main = "Boxplots mensuels", ylab = "Indice de production de boissons alcooliques", xlab = "Mois")

#Les 12 Boxplots sont similaires, aucun mois ne se d?marque des autres.

# Tendance: 

## D?composition de la s?rie temporelle afin d'analyser le trend
bois_alc_dec <- decompose(bois_alc,type="multiplicative")
autoplot(bois_alc_dec, main = "D?composition de la s?rie", xlab ="Ann?e")

#Valeurs aberrantes (Figure 9):

## La s?rie pr?sente visiblement des cas de valeurs aberrantes: nous analysons donc les outliers.
abber <- tso(bois_alc, types = c("AO", "LS", "TC","IO"))
abber
plot(abber)


# Etude de l'autocorr?lation de la s?rie d'origine:

## Scatter plot des autocorr?lations
lag.plot(bois_alc, lags = 9, do.lines = FALSE)

## ACF : fonction d'autocorr?lation
ggAcf(bois_alc, lag.max=200, plot=T)

## PACF : fonction d'autocorr?lation partielle
ggPacf(bois_alc,lag.max=200, plot=T)

## R?capitulatif
ggtsdisplay(bois_alc)


# ========================================================================= #
# =============================== Question 2 ============================== #
# ========================================================================= #

# Avant de continuer, centrons la s?rie pour annuler l'effet de l'ordonn?e ? l'origine

bois_alc_c <- bois_alc - mean(bois_alc)

# Etude de la stationnarit? de la s?rie centr?e
pmax = 9

## Tests de stationnarit? (Tableau 1)

## Test de Dickey-Fuller Augment?
adf <- adfTest(bois_alc_c, lags=pmax, type = "ct")
adf
summary(adf@test$lm)

## Test de Phillips Perron
pp.test(bois_alc_c) 

## Test de KPSS
kpss.test(bois_alc_c, null = "Trend") 
kpss.test(bois_alc_c) 

## On observe une s?rie (I(1)+T) avec ADF et I(1) avec KPSS. On conclut que la s?rie est stationnaire en diff?rences premi?res

# Construction de la s?rie diff?renci?e
d_bois_alc = diff(bois_alc)
plot(d_bois_alc)

# Tests de stationnarit? de la s?rie diff?renci?e (Tableau 2)

## Test de Dickey-Fuller Augment?
adf_sta <- adfTest(d_bois_alc, lags=pmax, type = "ct")
adf_sta
summary(adf_sta@test$lm)

## Test de Phillips Perron
pp.test(d_bois_alc) 

## Test de KPSS
kpss.test(d_bois_alc) 

## Apr?s diff?rentiation, la s?rie devient stationnaire
qplot(y=d_bois_alc, x =as.yearmon(base[[1]])[-c(1)],geom = "line", xlab = "", ylab = "Indice (diff?renci?) de production de boissons alcooliques")


# ========================================================================= #
# =============================== Question 3 ============================== #
# ========================================================================= #

# Visualisation de la s?rie initiale et de la s?rie stationnaris?e (Figure 2)
par(mfrow=c(1,2))
plot(y=bois_alc, x =as.yearmon(base[[1]]),type = "line", main="S?rie initiale", xlab = "", ylab = "Indice de production de boissons alcooliques")
plot(y=d_bois_alc, x =as.yearmon(base[[1]])[-c(1)],type = "line", main="S?rie stationnaire", xlab = "", ylab = "Indice (diff?renci?) de production de boissons alcooliques")
par(mfrow=c(1,1))

# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE II : Mod?les ARMA                          #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# =============================== Question 4 ============================== #
# ========================================================================= #

# Pour choisir le mod?le ARIMA adapt?, nous commen?ons par visualiser les ACF et PACF de la s?rie stationnaire (Figure 3)
par(mfrow=c(1,2))
ggAcf(d_bois_alc, plot=T)
ggPacf(d_bois_alc, plot=T)
par(mfrow=c(1,1))
ggtsdisplay(d_bois_alc)

# Les ACF et PACF nous font penser ? un AR(3) et un MA(2). Nous allons tester tous les mod?les ARMA tels que p<=3 et q<=2
model1=sarima(bois_alc, 3, 1, 0)
model2=sarima(bois_alc, 3, 1, 1)
model3=sarima(bois_alc, 3, 1, 2)
model4=sarima(bois_alc, 2, 1, 0)
model5=sarima(bois_alc, 2, 1, 1)
model6=sarima(bois_alc, 2, 1, 2)
model7=sarima(bois_alc, 1, 1, 0)
model8=sarima(bois_alc, 1, 1, 1)
model9=sarima(bois_alc, 1, 1, 2)
model10=sarima(bois_alc, 0, 1, 1)
model11=sarima(bois_alc, 0, 1, 2)

# Coefficients estim?s
model1$ttable
model2$ttable
model3$ttable
model4$ttable
model5$ttable
model6$ttable
model7$ttable
model8$ttable
model9$ttable
model10$ttable
model11$ttable

# Les mod?les 1, 4, 7, 8, 10 et 11 ont tous leurs coefficients significatifs: ce sont de bons candidats.

# Test de Ljung-Box (non-autocorr?lation des r?sidus)
LjungBox(model1$fit)
LjungBox(model2$fit)
LjungBox(model3$fit)
LjungBox(model4$fit)
LjungBox(model5$fit)
LjungBox(model6$fit)
LjungBox(model7$fit)
LjungBox(model8$fit)
LjungBox(model9$fit)
LjungBox(model10$fit)
LjungBox(model11$fit)

# Tests joints et visualisation des r?sidus
checkresiduals(model1$fit)
checkresiduals(model2$fit)
checkresiduals(model3$fit)
checkresiduals(model4$fit)
checkresiduals(model5$fit)
checkresiduals(model6$fit)
checkresiduals(model7$fit)
checkresiduals(model8$fit)
checkresiduals(model9$fit)
checkresiduals(model10$fit)
checkresiduals(model11$fit)

#Parmi les mod?les dont tous les coefficients sont significatifs, les mod?les 8 et 11 passent le test de Ljung-Box.


# S?lection du meilleur mod?le sur la base des crit?res d'information
# AIC
aic <- AIC(model1$fit, model2$fit, model3$fit, model4$fit, model5$fit, model6$fit, model7$fit, model8$fit, model9$fit, model10$fit, model11$fit)
aic
which.min(aic$AIC)
## Le mod?le ARIMA(1,1,1) pr?sente les meilleurs r?sultats pour le crit?re AIC.

# BIC
bic <- BIC(model1$fit, model2$fit, model3$fit, model4$fit, model5$fit, model6$fit, model7$fit, model8$fit, model9$fit, model10$fit, model11$fit)
bic
which.min(bic$BIC)
## Le mod?le ARIMA(1,1,1) pr?sente les meilleurs r?sultats ?galement pour le crit?re BIC.

# ========================================================================= #
# =============================== Question 5 ============================== #
# ========================================================================= #


# Am?lioration du mod?le final en prenant en compte les valeurs aberrantes (outliers)
arima111=arima(bois_alc, order=c(1,1,1))
res=locate.outliers.oloop(bois_alc, arima111, cval = 5, types = c("AO", "LS", "TC","IO"), delta = 0.7)
res$outliers
model_out <- discard.outliers(res, bois_alc, method = "en-masse", tsmethod.call = arima111$call, cval=5)
model_out$outliers
final_model <- model_out$fit

# Coefficients du mod?le ARIMA final
coefficients(final_model) 


# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE III : Pr?vision                            #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# =========================== Questions 6 & 7 ============================= #
# ========================================================================= #

# L'?quation v?rifi?e par l'intervalle de confiance est pr?sent?e dans le document
## Intervalle de confiance bivari?
# On construit la matrice sigma avec les ?carts-types et le coefficient de corr?lation
sigma_g1 <- sqrt(final_model$sigma2)
sigma_g2 <- sqrt(final_model$sigma2*(1+(1 + final_model$coef[1] - final_model$coef[2])^2))
rho <- final_model$sigma2*(1 + final_model$coef[1] - final_model$coef[2])
Sigma <- rbind(c(sigma_g1^2, rho), 
               c(rho, sigma_g2^2))
Sigma

# On d?finit les valeurs propres et vecteurs propres de la matrice consid?r?e
eigen(Sigma)
eigen(Sigma)$values
eigen(Sigma)$vectors
# Le grand axe de l'ellipse est dirig? par le vecteur propre de Sigma associ? ? la plus grande valeur propre. 
# On affiche graphiquement l'ellipse
Pred <- predict(final_model,n.ahead = 2, newxreg = matrix(c(0,0),nrow = 1, ncol = 2, byrow = TRUE), se.fit = TRUE)

ell <- ellipse(Sigma,centre=Pred$pred,level=0.95,
               npoints =10000)
plot(ell,xlab="Pr?vision Mars 2022 (T+1)", ylab="Pr?vision Avril 2022 (T+2)")
points(x=Pred$pred[1],y=Pred$pred[2],type="p",lwd=7, pch = 23)

# ========================================================================= #
# =============================== Question 8 ============================== #
# ========================================================================= #

# Pr?visions pour les deux p?riodes suivantes en prenant en compte les effets des valeurs aberrantes
par(mfrow =c(1 ,1))
prev_T2 <- sarima.for(bois_alc,n.ahead=2, p=1,d=1,q=1,xreg = model_out$xreg, newxreg =  matrix(c(0,0),nrow = 1, ncol = 2, byrow = TRUE)) # Visualisation graphique
prev_T2

