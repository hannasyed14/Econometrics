# Econometrics
Econometrics DAT 411

## Exam 1
📊 DAT 411 Exam 1 – Hanna Syed
In this exam, I analyzed financial data to assess the relationship between industry excess returns (specifically the Games industry) and market factors. I used R to:

- Import and summarize the data (ER and ERM) using stargazer

- Conduct multiple hypothesis tests on population means and directional claims

- Estimate and interpret a CAPM regression model of Games excess returns on ERM

- Evaluate model fit using R² and Adjusted R², and interpret regression coefficients

- Visualize actual vs. predicted values using ggplot2 to assess model performance

- Extend the model using the Fama-French 3-Factor model (ERM, SMB, HML)

- Compare model improvements and perform individual t-tests on each coefficient

The project demonstrates hypothesis testing, simple and multiple regression, model evaluation, and financial interpretation within the context of excess returns in the Games industry.

## Exam 2
📊 Exam 2 – DAT 411
In this exam, I conducted a comprehensive time series analysis using data on excess returns and factor models. I built and interpreted a multiple linear regression model explaining the “Other” industry’s return using the Fama-French factors: EMR, SMB, and HML. I tested for heteroskedasticity using both the Breusch-Pagan and White tests, and applied robust standard errors to correct it. I also tested for serial correlation using the Breusch-Godfrey test and further adjusted the model using Newey-West standard errors.

I conducted Augmented Dickey-Fuller tests to confirm the stationarity of the variables, and performed Granger causality tests to assess predictive relationships between the factors and the dependent variable. Finally, I built and compared time series models—AR, ARMA, and multiple ARDL specifications—using AIC/BIC values for model selection. The ARDL(5,1) model was chosen as the best fit.


## Exam 3
🧠 Exam 3 - Macroeconomic Time Series Analysis
In this exam, I conducted a time series econometric analysis using U.S. macroeconomic data and housing price indices (HPI) for Washington, D.C. The analysis involved:

- Importing and cleaning two datasets: macroeconomic indicators and HPI data

- Merging data by year and quarter

- Testing for stationarity using the Augmented Dickey-Fuller (ADF) test

- Transforming variables using percent changes to achieve stationarity

- Estimating a baseline regression model to explain Housing Price Returns (HPR) using lagged values of HPR and inflation

- Testing for heteroskedasticity (Breusch-Pagan and White Tests) and correcting standard errors using robust and Newey-West methods

- Testing for serial correlation using the Breusch-Godfrey test

- Comparing models with different lag structures using AIC and BIC to determine optimal lag length

The code also includes a sequence of models with increasing lags to evaluate the predictive performance of each. Statistical corrections were applied to improve model reliability and inference.







