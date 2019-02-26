#https://www.theanalysisfactor.com/covariance-matrices/

library(MASS)
set.seed(60) 
SigmaWI = matrix(rep(.7,20*20),nr=20)
SigmaCA = matrix(rep(.7,20*20),nr=20)
diag(SigmaWI) = 1
diag(SigmaCA) = 1
WItime = runif(20, 5, 40)
WIspent = .5*WItime + 5 + mvrnorm(1, rep(0,20), SigmaWI) # generate 1 random vector of 20 noise terms
CAtime = runif(20, 5, 40)
CAspent = .5*CAtime + 5 + mvrnorm(1, rep(0,20), SigmaCA)
alltime = c(WItime, CAtime)
allspent = c(WIspent, CAspent)

state = c(rep("WI",20), rep("CA",20))

mycol = c( rep("blue",20), rep("red",20) )
mypch = c( rep("W",20), rep("C",20) )
plot(allspent~alltime, col=mycol, pch=mypch, las = 1, xlab="Time", ylab="$ Spent", cex.axis=1.2)
legend("topleft", pch=c("W", "C"), col=c("blue", "red"), legend=c("Wisconsin", "California"))

library(nlme)
fit = gls(allspent~alltime, correlation = corCompSymm(form = ~1 | state)) #compound symmetric covariance matrix
summary(fit)
getVarCov(fit)


#autocorrelation and correlated groups
stock = read.csv("stock_2016.csv")
plot(stock$ClosingPrice[1:125] ~ stock$TradingDay[1:125], main = "Apple Closing Price")

#https://stats.stackexchange.com/questions/175144/what-does-the-1-in-the-r-formula-y-1-mean

#start by using ols regresssions
fit = lm(ClosingPrice ~ logVolume + Company, data = stock)
par(mfrow=c(2,2))
plot(fit)
#autocorrelation, successive data point (or residuals) are correlated. Common in time series data
myres = fit$residuals[stock$Company == "Google"]
par(mfrow=c(1,1))
plot(myres[-125],myres[-1])
points(myres[17], myres[18], col="red", bg="red", cex=1.2, pch=21)
points(myres[18], myres[19], col="blue", bg="blue", cex=1.2, pch=24)

cor(myres[-125],myres[-1])

acf(fit$residuals, ci.type = "ma") #autocorrelation plot, dashed blue lines indicate how near zero to not have autocorrelation
#For a formal hypothesis test for autocorrelation, the Durbin-Watson test or Breusch-Godfrey test can be used.



#library(nlme)
attach(stock)
fit2 = gls(ClosingPrice ~ logVolume + Company, correlation = corAR1(form = ~1 | Company)) #ar1 model, first order auto regressive model
summary(fit2)
getVarCov(fit2)
plot(fit2)

#https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but
#https://stats.stackexchange.com/questions/99895/why-does-one-have-to-use-reml-instead-of-ml-for-choosing-among-nested-var-cova/171529#171529

fit = gls(ClosingPrice ~ logVolume + Company, data = stock) #ar1 model, first order auto regressive model
AIC(fit,fit2)

#change how time is measured
#to account for weekends, change time to DayOfYear
fit3 = gls(ClosingPrice ~ logVolume + Company, data = stock, correlation = corAR1(form = ~DayOfYear | Company))
AIC(fit2,fit3)
#changing time in this case does not help

#http://plantecology.syr.edu/fridley/bio793/mixed2.html