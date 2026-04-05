# Short-term Forecasting of Industrial Output: An $ARIMA(p, d, q)$ Application to French Dairy Products

## Project Overview
[cite_start]This repository contains the work for the **Linear Time Series Assignment**[cite: 1]. [cite_start]The objective is the **ARIMA modelling** of a specific French economic time series to perform estimation and prediction[cite: 2, 12].

[cite_start]The project follows the Box-Jenkins methodology to identify, estimate, and validate a linear model for observed data[cite: 8, 22, 23].

## Dataset
* [cite_start]**Series:** Industrial Production Index (IPI) - Manufacture of dairy products[cite: 13].
* **Insee ID:** `010767631`.
* [cite_start]**Characteristics:** * Corrected from seasonal variations and working days (**CVS-CJO**)[cite: 14].
    * [cite_start]Monthly frequency[cite: 14].
    * [cite_start]Contains more than 100 observations[cite: 14].
    * [cite_start]Includes only observed data (no price or financial data)[cite: 14, 15].

## Methodology
[cite_start]The analysis is divided into three main parts[cite: 11]:

### Part I: Data Analysis & Stationarity
* [cite_start]Graphical representation of the series[cite: 20].
* [cite_start]Logarithmic transformation and differentiation to achieve stationarity[cite: 17, 18].
* [cite_start]Rigorous justification of processing choices[cite: 19].

### Part II: ARMA Models
* [cite_start]Identification and justification of an $ARMA(p, q)$ model for the corrected series $X_t$[cite: 22].
* [cite_start]Parameter estimation and model validation[cite: 23].
* [cite_start]Formal specification of the $ARIMA(p, d, q)$ model[cite: 24].

### Part III: Prediction
* [cite_start]Derivation of the equation for the confidence region of level $\alpha = 95\%$ for $(X_{T+1}, X_{T+2})$[cite: 27, 29].
* [cite_start]Graphical representation of the prediction region[cite: 29].
* [cite_start]Analysis of predictive improvement using auxiliary stationary time series $Y_t$[cite: 30, 31].

## Submission Requirements
* [cite_start]**Deadline:** Before Sunday, May 10 at midnight[cite: 4].
* **Deliverables:** A zip file named `fname1 fname2` containing:
    * [cite_start]The PDF report (maximum 6 pages, in English)[cite: 6, 9].
    * [cite_start]The R script used for the analysis[cite: 7, 8].
    * [cite_start]The time series dataset in `.csv` format[cite: 7].
* [cite_start]**Grading Criteria:** Rigor, exactitude of econometric tools, conciseness, and clarity of results[cite: 8].

## Tools
* [cite_start]**Language:** R[cite: 8].
* **Environment:** GitHub for collaborative development.
