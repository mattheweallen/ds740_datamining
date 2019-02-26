pmin( c( 7, 2, 9 ), c(3, 7, 9) )
pmax( c( 4, -3, 4 ), 0 )

r=seq(-6,6,.01)
wi = pmax(1-abs(r)/3, 0) 
plot(r, wi, type="l", col="black", lwd=2, las =1, cex.axis=1.2, xlab="Residual", ylab="Weight")


#5a. Using the same vector r as in the previous problem, define Tukey’s bisquare weights as
#Tukey = (pmax(1-(r/4.685)^2,0))^2
#Use matplot to plot Tukey’s bisquare weights and the linear weights wi from the previous problem on the same graph.
Tukey = (pmax(1-(r/4.685)^2,0))^2
matplot(cbind(r,r),cbind(wi,Tukey), type = "l", lwd=2, col=c("black", "red"), las=1, cex.axis=1.2, xlab = "Residual", ylab = "Weight")
legend("topleft", legend=c("Linear", "Tukey"), col=c("black", "red"), lty=c(1,2))


#c. Read the bodyfat.csv data set into R. Load the MASS library. Use the rlm() function to perform robust regression using Tukey’s bisquare weights. What is the slope of the model?
fat=read.csv("bodyfat.csv")
library(MASS)
fit.Tukey = rlm(BodyFatBrozek~Weight, data = fat, psi = psi.bisquare)
summary(fit.Tukey)

#problem 6
#Next, we will implement robust regression using our linear weights. These weights are not available in rlm(), so we’ll use a while loop.
#6a. Start by writing the code that comes before the while loop:
#  Use unweighted linear regression to fit a model for BodyFatBrozek as a function of Weight.
#Initialize the vector newcoef equal to the coefficients from the unweighted linear regression.
#Set the number of iterations equal to 0.
#To determine whether the while loop has converged, we need to be able to compare the most recent set of coefficients (newcoef) to the previous set of coefficients. Initialize a vector oldcoef to store the previous set of coefficients.
#The initial values in oldcoef don’t matter very much, as long as they’re not too close to newcoef. I usually set them equal to 0. 
fit.w = lm(fat$BodyFatBrozek ~ fat$Weight)
oldcoef = rep(0, length(fit.w$coef))
newcoef = fit.w$coef
iter = 0

#6b. Then, create an empty while loop. The while loop should continue iterating as long as
#the number of iterations is less than 100, and
#the difference between the old and new coefficients is too large: |β0,old−β0,new|+|β1,old−β1,new|>.0001


#6b. Inside the while loop, do the following things:
#  Compute the new set of weights. Recall that the R code for our linear weights was
#pmax(1-abs(r)/3, 0)
#You will need to replace r with the vector of residuals from the previous linear regression.
#Perform weighted linear regression with the new set of weights.
#Update the values of oldcoef, newcoef, and the number of iterations.
while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
  w = pmax(1-abs(fit.w$residuals)/3, 0)
  fit.w = lm(fat$BodyFatBrozek~fat$Weight, weights=w)
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = fit.w$coef 
}
summary(fit.w)

plot(fit.w$w)

#Using
#length(which(fit.w$w==0)) / dim(fat)[1]
#finds that 63% of the observations were given a weight of 0. This is probably excessive, because it seems unrealistic to think that 63% of our observations were completely unreliable outliers.
#The linear weights we’ve created here are an inefficient choice for robust regression, because they result in ignoring too much of our data. We might be able to improve the weights by increasing the threshold for |ri| before giving a weight of 0. There are also many other weight functions that we could try. Some of them are listed here.
length(which(fit.w$w==0)) / dim(fat)[1] #63% of points were deemed unreliable this does not seem right.

#problem 8
#In this problem, you will investigate the effects of autocorrelation on linear regression, using a simulated data set. Start by creating a covariance matrix reflecting autocorrelation:
n = 100
Sigmax = matrix(,nr=n, nc=n) # initialize the covariance matrix
Sigmax[1,1] = 1
for(i in 2:n){
  Sigmax[i,i] = 1
  for(j in 1:(i-1)){
    Sigmax[i, j] = .9^abs(i-j)
    Sigmax[j, i] = Sigmax[i, j] # make the covariance matrix 
    # symmetric across the diagonal
  } #end iter over j
} # end iter over i

Sigmax

#Next, set the random seed equal to 15 and load the MASS library, which contains the function mvrnorm. This function generates a random vector from a multivariate normal distribution. Use the following code to simulate x and y:
#  x = runif(n, 0, 1)
#y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) # generate 1 random vector 
# of n noise terms
set.seed(15)
library(MASS)
x = runif(n, 0, 1)
y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) # generate 1 random vector 
# of n noise terms


#d. Use ordinary least squares regression to model y as a function of x. Call your model m1. Look at the diagnostic plots using plot(m1). What evidence do you see that ordinary least squares regression is inappropriate?
m1 = lm(y~x)
plot(m1)

#problem 9
#As you saw in the previous problem, it can be difficult to detect autocorrelation from looking at the usual diagnostic plots of linear regression. (This is also true for detecting compound symmetry.) Often, you will look for autocorrelation or compound symmetry because you have a reason to suspect it based on what you know about the data--for example, time series data often involve autocorrelation.
#a. Compute the correlation between residuals 1-99 and residuals 2-100.
cor(m1$residuals[-100], m1$residuals[-1])

#b. Use the acf() function to plot the autocorrelation for various lags. Which autocorrelations are outside the range we would expect if the noise terms were independent? (Use ci.type = “ma”.)
acf(m1$residuals, ci.type="ma")

#The acf plot and computing the lag-1 autocorrelation gives us good reason to believe that the fit of our model could be improved using an autoregressive model.
#We could also look at a graph of the residuals vs. their index:
#  plot(m1$residuals)
#or use a hypothesis test for autocorrelation, such as the Durbin-Watson test.
plot(m1$residuals)

#c. Load the nlme library and use gls() to fit an AR(1) model.
#You may assume that each pair of consecutive observations is separated by 1 time unit.
#Which of the following segments of code is most appropriate?
library(nlme)
m2 = gls(y ~ x, correlation = corAR1(form = ~1)) 
summary(m2)

m1$coefficients
m2$coefficients

m1_reml = gls(y~x)
m1$coefficients
m1_reml$coefficients
AIC(m1)
AIC(m1_reml)
AIC(m2)
