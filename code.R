---
title: "filefor report"
output: word_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Importing all the necessary packages
library(knitr)
library(TSA)
library(tseries)
library(lmtest)
library(dLagM)
library(FitAR)
library(forecast)
library(readxl)
library(fUnitRoots)
```

```{r echo=FALSE }
# Function for AIC BIC Sort - ML Models
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

# Function for AIC BIC Sort - CSS Models
calc_aic_bic <- function(model,data)
{
  len <- length(data)  
  aic=list()
  bic=list()
  k=list()
  df=list()
  for(i in 1:length(model)){
    k[[i]] <- length(model[[i]]$coef)
    aic[[i]] <- -2*model[[i]]$loglik +2*k[[i]]
    bic[[i]] <- -2*model[[i]]$loglik +k[[i]]*log(len)
    df[[i]] <- k[[i]] +1+1-1
  }
  return(list(aic,bic,df)) 
}

# Function for Residual Analysis on shortlisted ML and CSS Models
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH"
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"
  
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  acf(res.model,main="ACF of standardised residuals")
  pacf(res.model,main="PACF of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  k=0
  #Ljung Box independence test for every lag H0:independent, H1: series is correlated at lags
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
}

```



```{r }
# Setting the path for file access
setwd("C:\\Users\\DELL\\OneDrive\\Studies\\Semester 3\\Time Series\\Assignment 3")
# Reading the data into R
tourism <- read_excel("A3.xls", sheet = "Data1", range = "BR11:BR357", col_names = FALSE)
# Displaying the first 6 rows of the series
head(tourism)
```



```{r }
# Converting the dataframe into a time series object
tourism.ts = ts(tourism,start=c(1991,1),end=c(2019,11),frequency = 12)
class(tourism.ts)
head(tourism.ts)
```

```{r }
# Plotting the Time Series Plot
par(mfrow=c(1,1))
# Plotting the time series 
plot(tourism.ts,type='l',ylab='No. of Arrivals', main="Time series plot of the number of visitor arrivals")
points(y=tourism.ts,x=time(tourism.ts), pch=as.vector(season(tourism.ts)))

```

```{r }
# Scatter plot of observations for checking correlation in consecutive values of Neighboring number of visitor arrivals
plot(y=tourism.ts,x=zlag(tourism.ts),ylab='Number of Visitor Arrivals', xlab='Previous Year Visitor Arrivals',main = "Scatter plot of the number of visitor arrivals in successive years")
```

```{r }
# Determining the correlation between succeeding data points
y = tourism.ts
x = zlag(tourism.ts) 
ind = 2:length(x)
cor(y[ind],x[ind])
```

```{r }
#ACF and PACF
par(mfrow=c(1,2))
acf(tourism.ts,  lag.max = 36,main="The sample ACF of visitor arrivals")
pacf(tourism.ts,  lag.max = 36,main="The sample PACF of visitor arrivals")
```

```{r }
# Implementing ADF Test on the series
order = ar(diff(tourism.ts))$order
adfTest(tourism.ts, lags = order,  title = NULL,description = NULL)

```

```{r }
########### Fitting Deterministic Trend Models ##########

# Defining the Linear regression model
model1 = lm(tourism.ts~time(tourism.ts)) 
plot(tourism.ts,type='o',ylab='No. of Visitor Arrivals',main ="Fitting Linear Trend Model to monthly number of visitor arrivals in Australia")
legend ("topleft", lty = 1,bty = "n",text.width = 8,text.size ,col = c("black","red"),c("No. of Arrivals","Fitted Linear Trend Line"),cex = 0.75)
abline(model1, col="red") # add the fitted least squares line to the model
summary(model1)
```

```{r }
# Defining the Quadratic regression model
t = time(tourism.ts)
t2 = t^2
quadmodel2 = lm(tourism.ts~t+t2) # label the quadratic trend model as quadmodel2
summary(quadmodel2) # determining the summary 
plot(ts(fitted(quadmodel2)), 
     ylim=c(min(c(fitted(quadmodel2),as.vector(tourism.ts))),
            max(c(fitted(quadmodel2),as.vector(tourism.ts)))),
     col=c('red'), ylab='No. of Visitor Arrivals',main = "Fitting Quadratic Trend Model to monthly number of visitor arrivals in Australia")
lines(as.vector(tourism.ts),type="o",col="black")
legend ("topleft", lty = 1,bty = "n",text.width = 5, col = c("black","red"),c("No. of Arrivals","Fitted Quadratic Trend Line"),cex = 0.75)

```

```{r }
# Defining Cubic trend Model
t = time(tourism.ts)
t2 = t^2
t3 = t^3
model3 = lm(tourism.ts~t+t2+t3) 
summary(model3) #Determining the summary 
plot(ts(fitted(model3)), 
     ylim=c(min(c(fitted(model3),as.vector(tourism.ts))),
            max(c(fitted(model3),as.vector(tourism.ts)))),
     col=c('red'), ylab='No. of Visitor Arrivals',main = "Fitting Cubic Trend Model to monthly number of visitor arrivals in Australia")
lines(as.vector(tourism.ts),type="o",col="black")
legend ("topleft", lty = 1,bty = "n",text.width = 10, col = c("black","red"),c("No. of arrivals","Fitted Cubic Trend Line"),cex = 0.75)

```

```{r }
# Defining Seasonal trend Model
month.=season(tourism.ts) # period added to improve table display and this line sets up indicators
model4=lm(tourism.ts~month.-1) # -1 removes the intercept term
summary(model4)
model4.1=lm(tourism.ts~month.) # remove -1 to include the intercept term in the model
summary(model4.1)
```

```{r }
# Defining Harmonic Trend Model 
har.=harmonic(tourism.ts,1)
model.tourism.har=lm(tourism.ts~har.)
summary(model.tourism.har)
plot(ts(fitted(model.tourism.har)), ylim = c(min(c(fitted(model.tourism.har),
                                                   as.vector(tourism.ts))), max(c(fitted(model.tourism.har),as.vector(tourism.ts)))),
     ylab='y' , main = "Fitting Harminic Trend Model to monthly number of visitor arrivals in Australia", type="l",lty=2,col="red")
lines(as.vector(tourism.ts),type="o")
legend ("topleft", lty = 1,bty = "n",text.width = 10, col = c("black","red"),c("No. of arrivals","Fitted Harmonic Trend Line"),cex = 0.75)

```

```{r }
# Data has strong changing variance,implementing log transformation
log.tourism.ts = log(tourism.ts)
par(mfrow=c(1,1))
plot(log.tourism.ts,ylab='log of visitor ',xlab='Year',type='o', main = "Time Series plot of log of monthly arrivals.")
points(log.tourism.ts,cex = .6, col = "black")
plot(tourism.ts,type='o',main="Time series plot of the number of visitor arrivals", ylab = 'No. of Visitors')
points(tourism.ts,cex = .6, col = "black")

```

```{r }

# Seasonal differencing and fitting a plain seasonal model & residual analysis
m1.tourism = arima(log.tourism.ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.tourism);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting ACF and PACF plots
par(mfrow=c(1,2))
acf(res.m1, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m1, lag.max = 36, main = "The sample PACF of the residuals")

```

```{r }
# Implement SARMA(1,0) component 
m2.tourism = arima(log.tourism.ts,order=c(0,0,0),seasonal=list(order=c(1,1,0), period=12))
res.m2 = residuals(m2.tourism);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting ACF and PACF plots
par(mfrow=c(1,2))
acf(res.m2, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m2, lag.max = 36, main = "The sample PACF of the residuals")

```

```{r }
# Implement SARMA(2,0) component
m3.tourism = arima(log.tourism.ts,order=c(0,0,0),seasonal=list(order=c(2,1,0), period=12))
res.m3 = residuals(m3.tourism);  
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting ACF and PACF plots
par(mfrow=c(1,2))
acf(res.m3, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m3, lag.max = 36, main = "The sample PACF of the residuals")

```



```{r }
# Performing first ordinary difference with SARMA(1,0) component
m4.tourism = arima(log.tourism.ts,order=c(0,1,0),seasonal=list(order=c(1,1,0), period=12))
res.m4 = residuals(m4.tourism);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting ACF and PACF plots
par(mfrow=c(1,2))
acf(res.m4, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m4, lag.max = 36, main = "The sample PACF of the residuals")

```

```{r }
# Performing first ordinary difference with SARMA(2,0) component
m5.tourism = arima(log.tourism.ts,order=c(0,1,0),seasonal=list(order=c(2,1,0), period=12))
res.m5 = residuals(m5.tourism);  
par(mfrow=c(1,1))
plot(res.m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")

# Plotting ACF and PACF plots
par(mfrow=c(1,2))
acf(res.m5, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m5, lag.max = 36, main = "The sample PACF of the residuals")

```

```{r }
#ADF Test to check if the ordinary differenced series is stationary
order = ar(diff(res.m5))$order
adfTest(res.m5, lags = order,  title = NULL,description = NULL)

```

```{r }
############### Model Building Strategy ################

# Plotting ACF and PACF Plots for the first differenced Monthly Visitor Arrival series
par(mfrow=c(1,2))
acf(res.m5, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m5, lag.max = 36, main = "The sample PACF of the residuals")

```

```{r }
# Plotting EACF Matrix
eacf(res.m5)

```

```{r }
# Plotting the BIC Table 
par(mfrow=c(1,1))
res = armasubsets(y=res.m5,nar=7,nma=7,y.name='test',ar.method='ols')
plot(res)
title('BIC Table for monthly total number of visitor arrivals', line = 6)

```


```{r }
# Checking the candidate models: Residual analysis using ACF and PACF plots

#SARIMA(1,1,1)x(2,1,0) with Method ML
m111_ml.tourism = arima(log.tourism.ts,order=c(1,1,1),seasonal=list(order=c(2,1,0), period=12),  method="ML")
res.m111_ml = residuals(m111_ml.tourism)  
par(mfrow=c(1,1))
plot(res.m111_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m111_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(1,1,1)x(2,1,0) ")
pacf(res.m111_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(1,1,1)x(2,1,0) ")
# Model Rejected - has significant Correlations

#SARIMA(1,1,1)x(2,1,0) with Method CSS
m111_css.tourism = arima(log.tourism.ts,order=c(1,1,1),seasonal=list(order=c(2,1,0), period=12),  method="CSS")
res.m111_css = residuals(m111_css.tourism)  
par(mfrow=c(1,1))
plot(res.m111_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m111_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(1,1,1)x(2,1,0)")
pacf(res.m111_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(1,1,1)x(2,1,0)")
# Model Rejected - has significant Correlations

#SARIMA(2,1,1)x(2,1,0) with Method ML
m211_ml.tourism = arima(log.tourism.ts,order=c(2,1,1),seasonal=list(order=c(2,1,0), period=12),method="ML")
res.m211_ml = residuals(m211_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m211_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m211_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(2,1,1)x(2,1,0)")
pacf(res.m211_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(2,1,1)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(2,1,1)x(2,1,0) with Method CSS
m211_css.tourism = arima(log.tourism.ts,order=c(2,1,1),seasonal=list(order=c(2,1,0), period=12),method="CSS")
res.m211_css = residuals(m211_css.tourism);  
par(mfrow=c(1,1))
plot(res.m211_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m211_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(2,1,1)x(2,1,0) ")
pacf(res.m211_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(2,1,1)x(2,1,0)")
# Model Rejected - has significant Correlations

#SARIMA(3,1,1)x(2,1,0) with method ML
m311_ml.tourism = arima(log.tourism.ts,order=c(3,1,1),seasonal=list(order=c(2,1,0), period=12),method="ML")
res.m311_ml = residuals(m311_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m311_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m311_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(3,1,1)x(2,1,0)")
pacf(res.m311_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(3,1,1)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(3,1,1)x(2,1,0) with method CSS
m311_css.tourism = arima(log.tourism.ts,order=c(3,1,1),seasonal=list(order=c(2,1,0), period=12),method="CSS")
res.m311_css = residuals(m311_css.tourism);  
par(mfrow=c(1,1))
plot(res.m311_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m311_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(3,1,1)x(2,1,0)")
pacf(res.m311_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(3,1,1)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(0,1,4)x(2,1,0) with method ML
m014_ml.tourism = arima(log.tourism.ts,order=c(0,1,4),seasonal=list(order=c(2,1,0), period=12),method="ML")
res.m014_ml = residuals(m014_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m014_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m014_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(0,1,4)x(2,1,0)")
pacf(res.m014_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(0,1,4)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(0,1,4)x(2,1,0) with method CSS
m014_css.tourism = arima(log.tourism.ts,order=c(0,1,4),seasonal=list(order=c(2,1,0), period=12),method="CSS")
res.m014_css = residuals(m014_css.tourism);  
par(mfrow=c(1,1))
plot(res.m014_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m014_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(0,1,4)x(2,1,0)")
pacf(res.m014_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(0,1,4)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(1,1,4)x(2,1,0) with method ML
m114_ml.tourism = arima(log.tourism.ts,order=c(1,1,4),seasonal=list(order=c(2,1,0), period=12),method = "ML")
res.m114_ml = residuals(m114_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m114_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m114_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(1,1,4)x(2,1,0)")
pacf(res.m114_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(1,1,4)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(1,1,4)x(2,1,0) with method CSS
m114_css.tourism = arima(log.tourism.ts,order=c(1,1,4),seasonal=list(order=c(2,1,0), period=12),method = "CSS")
res.m114_css = residuals(m114_css.tourism);  
par(mfrow=c(1,1))
plot(res.m114_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m114_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(1,1,4)x(2,1,0)")
pacf(res.m114_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(1,1,4)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(0,1,5)x(2,1,0) with method ML
m015_ml.tourism = arima(log.tourism.ts,order=c(0,1,5),seasonal=list(order=c(2,1,0), period=12),method = "ML")
res.m015_ml = residuals(m015_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m015_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m015_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(0,1,5)x(2,1,0)")
pacf(res.m015_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(0,1,5)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(0,1,5)x(2,1,0) with method CSS
m015_css.tourism = arima(log.tourism.ts,order=c(0,1,5),seasonal=list(order=c(2,1,0), period=12),method = "CSS")
res.m015_css = residuals(m015_css.tourism);  
par(mfrow=c(1,1))
plot(res.m015_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m015_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(0,1,5)x(2,1,0)")
pacf(res.m015_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(0,1,5)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(4,1,1)x(2,1,0) with method ML
m411_ml.tourism = arima(log.tourism.ts,order=c(4,1,1),seasonal=list(order=c(2,1,0), period=12),method="ML")
res.m411_ml = residuals(m411_ml.tourism);  
par(mfrow=c(1,1))
plot(res.m411_ml,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m411_ml, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(4,1,1)x(2,1,0)")
pacf(res.m411_ml, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(4,1,1)x(2,1,0)")
# Model Considered- White Noise

#SARIMA(4,1,1)x(2,1,0) with method CSS
m411_css.tourism = arima(log.tourism.ts,order=c(4,1,1),seasonal=list(order=c(2,1,0), period=12),method="CSS")
res.m411_css = residuals(m411_css.tourism);  
par(mfrow=c(1,1))
plot(res.m411_css,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m411_css, lag.max = 36, main = "The sample ACF of the residuals for SARIMA(4,1,1)x(2,1,0)")
pacf(res.m411_css, lag.max = 36, main = "The sample PACF of the residuals for SARIMA(4,1,1)x(2,1,0)")
# Model Considered- White Noise

```


```{r }
#AIC Sort for ML Models
sc.AIC = AIC(m211_ml.tourism,m311_ml.tourism,m014_ml.tourism,m114_ml.tourism,m015_ml.tourism,m411_ml.tourism)
sort.score(sc.AIC, score = "aic")

#BIC Sort for ML Models
sc.BIC = BIC(m211_ml.tourism,m311_ml.tourism,m014_ml.tourism,m114_ml.tourism,m015_ml.tourism,m411_ml.tourism)
sort.score(sc.BIC, score = "bic")

```


```{r }
# Performing AIC BIC Sort for CSS Models
Model<-list("m311_css.tourism","m014_css.tourism","m114_css.tourism","m015_css.tourism","m411_css.tourism")
aic_bic <-calc_aic_bic(list(m311_css.tourism,m014_css.tourism,m114_css.tourism,m015_css.tourism,m411_css.tourism),log.tourism.ts)
AIC <-aic_bic[[1]]
BIC <- aic_bic[[2]]
df <-  aic_bic[[3]]
aic_table <- as.data.frame(cbind(Model,AIC,df))
bic_table <- as.data.frame(cbind(Model,BIC,df))

BICtable <- as.data.frame(lapply(bic_table, unlist))
BICtable <- BICtable[order(BICtable$BIC),]
BICtable

AICtable <- as.data.frame(lapply(aic_table, unlist))
AICtable <- AICtable[order(AICtable$AIC),]
AICtable

```

```{r }
########## Overfitting CSS Model #############

# Performing overfitting for CSS Model
m312_css.tourism = arima(log.tourism.ts,order=c(3,1,2),seasonal=list(order=c(2,1,0), period=12),method="CSS")

# Performing coef test on CSS models
coeftest(m311_css.tourism)
coeftest(m312_css.tourism)

```


```{r }
########## Overfitting ML Models #############

# Performing overfitting for shortlisted ML Models
m511_ml.tourism = arima(log.tourism.ts,order=c(5,1,1),seasonal=list(order=c(2,1,0), period=12),method="ML")
m412_ml.tourism = arima(log.tourism.ts,order=c(4,1,2),seasonal=list(order=c(2,1,0), period=12),method="ML")
m212_ml.tourism = arima(log.tourism.ts,order=c(2,1,2),seasonal=list(order=c(2,1,0), period=12),method="ML")

# Performing coef test on ML models
coeftest(m014_ml.tourism)
coeftest(m411_ml.tourism)
coeftest(m511_ml.tourism)
coeftest(m412_ml.tourism)
coeftest(m212_ml.tourism)
coeftest(m211_ml.tourism)

```


```{r }
# Residual Analysis of SARIMA(2,1,1)x(2,1,0) to determine Model Diagnostics
win.graph(width=10, height=10,pointsize=8)
residual.analysis(model = m211_ml.tourism) 
```


```{r }
# Forecasting using SARIMA(2,1,1)x(2,1,0)
par(mfrow=c(1,1))
m211_ml.tourism = Arima(tourism.ts,order=c(2,1,1),seasonal=list(order=c(2,1,0), period=12),method = "ML",lambda = 0)
preds = forecast(m211_ml.tourism, h = 10)
plot(preds,main = "10 months forecast using SARIMA(2,1,1)x(2,1,0) on number of visitor arrivals in Australia",ylab = "No. of Visitor Arrivals",xlab="Time")

```

```{r }
# Determining CI Range
predsdf<-as.data.frame(preds)
predsdf

```
