## clear the console 

rm(list = ls()) 

## import libraries

library("urca")
library(AER)
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)
library(tidyverse)
library(haven)
library(tseries)
library(ggplot2) 

##import data

## set wd 



# read data into r 


data <-dataA2


#Preparing data, convert to time series 


hourwk <- ts(data$hourwk, start = c(1959, 1), end = c(2017, 4), frequency = 4)

rgdp <- ts(data$rgdp, start = c(1959, 1),  end = c(2017, 4), frequency = 4)

gdpdf <- ts(data$gdpdf, start = c(1959, 1),  end = c(2017, 4),  frequency = 4)

 

head(data)

tail(data)

# Q1 Draw the time series plot of each time series 

plot(hourwk)
plot(rgdp)
plot(gdpdf)

#Q2 Test the stationarity of each time series. Then, 
#apply Johansen's cointegration test to the stacked vector, Interpret your findings

#Test the stationarity of each time series 

adf.test(hourwk) 
adf.test(rgdp) 
adf.test(gdpdf)

# apply Cointegration test


johansen=ca.jo(data.frame(gdpdf,hourwk,rgdp), type="trace", K=2, ecdet="none", spec="longrun")
summary(johansen)

#fail to reject the null of  

#Q3 

#transform rgdp and hourwk into growth rates and take the 
#second difference of the logarithm of gdpdf


# 2nd diff of log gdpdf

log_gdpdf <- log(gdpdf)
log_gdpdf_d1 <- diff(log_gdpdf)
log_gdpdf_d2 <- diff(log_gdpdf_d1)

log_gdpdf_d2 

# Transform rgdp into growth rates


log_rgdp <- log(rgdp)  


rgdp_d1 <-- diff(log_rgdp)

rgdp_d1 <- window(rgdp_d1, start = c(1959, 3), )

rgdp_d1

#Transform hourwk into growth rates

log_hourwk <- log(hourwk)

hourwk_d1 <- diff(log_hourwk) 

hourwk_d1 <- window(hourwk_d1, start = c(1959, 3), )  

#check Transformed variables for stationarity 

adf.test(log_gdpdf_d2) 
adf.test(rgdp_d1) 
adf.test(hourwk_d1) 



# johansen's test 

johansen_test2 = ca.jo(data.frame(log_gdpdf_d2,hourwk_d1,rgdp_d1), type="trace", K=2, ecdet="none", spec="longrun")
summary(johansen_test2)



# Q4 

#impose restrictions and 

VAR_data <- window(ts.union(log_gdpdf_d2, hourwk_d1, rgdp_d1), start = c(1959, 3), end = c(2017, 4))

# select lags for VAR model 

VARselect(VAR_data, lag.max=8,type="const")[["selection"]]

# bic suggests 1 lag 


var1 <- VAR(VAR_data, p=1, type="const")

summary(var1) 

# run VAR but its not that good

# add restrictions:

# log_gdpdf_d2 not impactd by rgdp_d1

# hourwk_d1 not impacted by log_gdpdf_d2 or rgdp_d1 


restrict <- matrix(c(1, 1, 0, 1,
                     0, 1, 0, 1, 
                     1, 1, 1, 1),
                   nrow=3, ncol=4, byrow=TRUE) 
var2 <- restrict(var1, method = "man", resmat = restrict)

var2 
coef(var2) 


#Q5 

# impulse response function 

irf_gdp_hourwk <- irf(var2, impulse = "hourwk_d1", response = "rgdp_d1",
n.ahead = 10, ortho = TRUE, runs = 100) 
irf_gdp_hourwk
plot(irf_gdp_hourwk)

