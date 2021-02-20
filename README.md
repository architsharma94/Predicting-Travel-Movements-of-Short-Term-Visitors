# Predicting-Travel-Movements-of-Short-Term-Visitors

## Introduction

Visiting Australia means encountering opportunities; with its young, multicultural, and ever-growing economy. Australia also attracts humankind with its impeccable coastlines, charming cities and breath-taking landscapes, the immense country is a must on anyone's travel bucket list. We are glad be living here and thus making the most of this chance, the analysis and prediction of travel movements of short-term visitors arriving in Australia has been attempted through this report.
Administrative information on people arriving in Australia is collected via various processing systems, passport documents, visa information, and incoming passenger cards. In order to understand and predict the number of visitors Australia welcomes, the administrative data (Statistics, 2019) collected by the Australian Government’s Department of Home Affairs has been accessed.

## Methodology

A high-level view of the methodology used is discussed in this section.
1.	The dataset is examined and visualized to summarize its characteristics like trend, variance, seasonality, moving average and autoregressive behavior
2.	The stationarity of the series is ensured (by transformations, d order differencing, ADF test, etc.)
3.	Depending upon the characteristics of time series, especially trend, variance, stationarity and seasonality, the candidate models are chosen from linear, quadratic, cosine, and cyclical trend models along with ARIMA and SARIMA 
4.	The seasonality aspect will be handled by finding the parameter estimates for seasonal (P,D,Q)  ARIMA model
5.	The parameter values for models pertaining to ARIMA are identified using the following processes:
a.	ACF & PACF Plots
b.	EACF Matrix
c.	BIC Table
6.	The models are fitted on the time series data using ML and CSS methods; and their significance and extent-of-fit are checked by comparing AIC and BIC scores
7.	The overparameterized models, or overfitted models are then checked for their suitability
8.	Coefficient tests are performed to test the significance of parameters estimates of shortlisted models, after checking for overfitting
9.	The residuals of the best-fit models are examined for normality and resemblance to white noise
10.	The model that is found to be optimal in the tests mentioned above will be chosen to give forecasts for the next 10 units of time, i.e. next 10 months

## About Dataset

The dataset consists of over 300 observations of monthly numbers of short-term arrivals in the country from January 1991 to November 2019. The total monthly visitor numbers range from 161 thousand to 1 million 57 thousand approximately, with a mean of 455 thousand arrivals. 
