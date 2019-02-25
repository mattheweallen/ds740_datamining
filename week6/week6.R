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

