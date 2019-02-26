#are all data points equally reliable?
set.seed(33) 
n = c(sample(10:20, 15, replace=T), sample(100:200,15, replace=T)) # number of customers per week at diff stores
avgtime = rep(-1, 30)
avgspent = rep(-1,30)
for(store in 1:30){
  time = runif(n[store], 5, 40)
  spent = .5*time + 5 + rnorm(n[store], 0, 5)
  avgtime[store] = mean(time)
  avgspent[store] = mean(spent)
}

plot(avgspent~avgtime, col=c("red", "black"), pch=c(4,21), xlab="Average time per customer (minutes)", ylab="Average $ spent per customer", las=1)
legend("topleft", legend=c("Store with 10-20 customers", "Store with 100-200 customers"), pch=c(4,21), col=c("red", "black"))

#using a model to estimate the variance on the noise terms
set.seed(12)
x = runif(100)
y = 1 + 3*x + rnorm(100, 0, 1 + 3*x)
plot(x, y, las=1)
#this produces a scatterplot where variance increases with increasing x.


#iteratively weighted least squares or iwls in R
fit.w = lm(y~x) #fit the model with weights equal to 1

oldcoef = rep(0,length(fit.w$coefficients))
newcoef = fit.w$coefficients
iter = 0

while(sum(abs(oldcoef-newcoef))>.0001 & iter < 100) {
  w = 1/(fit.w$fitted.values^2) #find the weights
  fit.w = lm(y~x, weights = w) #refit using the weights 
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = fit.w$coefficients
}
newcoef
fit.w$coefficients

#a more complicated model for noise terms
set.seed(12)
x = runif(100)
y = 3 + 2*x + rnorm(100, 0, sqrt(1-x)) # rnorm uses standard deviation instead of variance
plot(x, y, las=1)

#while loop for more complicated model
fit.w = lm(y~x)

oldcoef = rep(0, length(fit.w$coef))
newcoef = fit.w$coef
iter = 0

while(sum(abs(oldcoef-newcoef))>.0001 & iter < 100) {
  fit.epsilon = lm(fit.w$residuals^2~x) #regress epsilon^2 on x
  w = 1/fit.epsilon$fitted.values #find the weights, 1/var(epsilon)
  fit.w = lm(y~x, weights = w) #refit using the weights 
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = fit.w$coefficients
}
newcoef
fit.w$coefficients

#when to use iteratively weighted least squares or iwls
#use when the variance of the error term is exactly equal to E(y_i)


#robust regression put less weight on outliers or influential points
#weights decrease as absolute value of residuals increase
#http://www.saedsayad.com/docs/RobustRegression.pdf

library(MASS)
plot(hills$time ~ hills$dist, cex.axis=1.2,las=1, ylab="Time", xlab="Distance")
fit = lm(time ~ dist, data = hills)
abline(fit, lwd=2)
legend("topleft", legend="OLS", lty=1)

par( mfrow = c(2, 2) )
plot(fit)

#while loop for tukey's bisquare method
fit.w = lm(time ~ dist, data = hills)

oldcoef = rep(0, length(fit.w$coef))
newcoef = fit.w$coef
iter = 0

while(sum(abs(oldcoef-newcoef))>.0001 & iter < 100) {
  MAR = median(abs(fit.w$residuals))
  k = 4.685*MAR/0.6745
  w = (pmax(1-(fit.w$residuals/k)^2,0))^2
  fit.w = lm(time ~ dist, data = hills, weights = w)
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = fit.w$coefficients
}
newcoef
fit.w$coefficients

####
library(MASS)
fit.bisquare = rlm(time ~ dist, data = hills, psi = psi.bisquare)
fit.huber = rlm(time ~ dist, data = hills, psi = psi.huber)

par( mfrow = c(1, 1) )
plot(hills$time ~ hills$dist, cex.axis=1.2,las=1, ylab="Time", xlab="Distance")
abline(fit, lwd=2)
abline(fit.bisquare, lwd=2, col="blue", lty=2)
abline(fit.huber, lwd=2, col="red", lty=3)
legend("topleft", legend=c("OLS", "Huber", "Bisquare"), lty=c(1,3,2), col=c("black","red","blue"))



#install.packages("car")
{
library(car)
plot(fit.bisquare$w, las=1, cex.axis=1.2, ylab="Weights")
smallweights = which(fit.bisquare$w < .8)
showLabels(1:dim(hills)[1], fit.bisquare$w, rownames(hills), id.method = smallweights)
}

#https://www.cs.umd.edu/~mount/Papers/lts-manuscript07.pdf