<p align="center">
<img src="https://readme-typing-svg.demolab.com?font=Georgia&size=35&color=8B0000&center=true&width=900&height=60&lines=Short-term+Forecasting+of+Industrial+Output%3A&repeat=false"/>
</p>

<p align="center">
<img src="https://readme-typing-svg.demolab.com?font=Georgia&size=35&color=8B0000&center=true&width=900&height=60&lines=An+ARIMA(p%2Cd%2Cq)+Application+to+French+Dairy+Products&repeat=false"/>
</p>

## $$\color{darkred}{\textbf{Project Overview}}$$
This repository contains the work for the **Linear Time Series Assignment**[cite: 1]. [cite_start]The objective is the **ARIMA modelling** of a specific French economic time series to perform estimation and prediction[cite: 2, 12].

The project follows the Box-Jenkins methodology to identify, estimate, and validate a linear model for observed data[cite: 8, 22, 23].

## $$\color{darkred}{\textbf{Dataset}}$$
* **Series:** Industrial Production Index (IPI) - Manufacture of dairy products[cite: 13].
* **Insee ID:** `010767631`.
* **Characteristics:** * Corrected from seasonal variations and working days (**CVS-CJO**)[cite: 14].
    * Monthly frequency[cite: 14].
    * Contains more than 100 observations[cite: 14].
    * Includes only observed data (no price or financial data)[cite: 14, 15].

## $$\color{darkred}{\textbf{Methodology}}$$

The analysis is divided into three main parts[cite: 11]:


### $\color{darkred}{\textbf{Part I: Data Analysis and Stationarity}}$

* Graphical representation of the series[cite: 20].
* Logarithmic transformation and differentiation to achieve stationarity[cite: 17, 18].
* Rigorous justification of processing choices[cite: 19].

### $$\color{darkred}{\textbf{Part II: ARMA Models }}$$
* Identification and justification of an $ARMA(p, q)$ model for the corrected series $X_t$[cite: 22].
* Parameter estimation and model validation[cite: 23].
* Formal specification of the $ARIMA(p, d, q)$ model[cite: 24].

### $$\color{darkred}{\textbf{Part III: Prediction}}$$
* Derivation of the equation for the confidence region of level $\alpha = 95\%$ for $(X_{T+1}, X_{T+2})$[cite: 27, 29].
* Graphical representation of the prediction region[cite: 29].
* Analysis of predictive improvement using auxiliary stationary time series $Y_t$[cite: 30, 31].


* **Environment:** GitHub for collaborative development.
