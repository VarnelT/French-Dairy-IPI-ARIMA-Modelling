# ***************************************************************************
# ******************** TIME SERIES PROJECT ***************************
# ***************************************************************************

# Authors : Varnel TIENTCHEU et Rivalien MAGUETSWET 

rm(list=ls(all=TRUE))

#Chargement des packages 
install.packages("FitAR")
library(urca)
library(lmtest)
library(readr)
library(zoo)
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
valeurs_mensuelles <- read_delim("valeurs_mensuelles.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
base <- valeurs_mensuelles[-c(1:3),c(1:2)]
colnames(base) <- c("t","Xt")

# Conversion en Date (Format YYYY-MM)
base$t <- as.Date(paste0(base$t, "-01"), format="%Y-%m-%d")
base$Xt <- as.numeric(base$Xt)

# On réordonne la série
base <- base[order(base$t), ]
View(base)

# **************************************************************************#
#                                                                           #
#                                                                           #
#                         PARTIE I : DATA                          #
#                                                                           #
#                                                                           #
# **************************************************************************#

# ========================================================================= #
# ============================= Question 1 ================================ #
# ========================================================================= #

# Passage au logarithme
xt <- ts(base$Xt, start = c(1990, 1), frequency = 12)
lxt <- log(xt)

# Extraction du mouvement de fond 
trend <- ma(lxt, order = 12, centre = TRUE)

par(mfrow=c(1,2))
# Visualisation propre
plot(lxt, 
     main = "Evolution of Log IPI Index",
     ylab = "Log-Indice", 
     xlab = "period",
     col = "gray",
     lwd = 2)

lines(trend, col = "firebrick", lwd = 1) 

legend("bottomright", 
       legend = c("Log(Xt)", "Trend"),
       col = c("gray", "firebrick"), 
       lty = 1, 
       lwd = c(1, 1))

# Visualisation de la série tedantanciée
plot(lxt-trend, 
     main = "Serie without the trend",
     ylab = "Xt-Trend", 
     xlab = "Temps",
     col = "blue",
     lwd = 1)
