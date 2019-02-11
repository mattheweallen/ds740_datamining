#logistic curves, from lecture
n = length(Default$default)
y = rep(0, n)
y[which(Default$default=="Yes")] = 1
mycol = rep("red", n)
mycol[which(Default$student=="Yes")] = "blue"
mypch = rep(21, n)
mypch[which(Default$student=="Yes")] = 2
plot(Default$balance, y, col=mycol, pch=mypch, xlim=c(0,2650), ylim=c(0,1), xlab="Balance", ylab="Default", cex.axis=1.2)

xvals = seq(min(Default$balance), max(Default$balance), length = 200)
yvals.student = predict(fit, newdata = data.frame(balance=xvals, student="Yes"), type="response")
yvals.nonStudent = predict(fit, newdata = data.frame(balance=xvals, student="No"), type="response")
par(new = T)
matplot(cbind(xvals, xvals), cbind(yvals.student, yvals.nonStudent), xlim=c(0,2650), ylim=c(0,1), xlab="", ylab="", type="l", col=c("blue", "red"), lwd=2, lty=c(1,2), cex.axis=1.2)
legend("topleft", legend=c("Student", "Non-student"), lty=c(1,2), col=c("blue","red"))



#logistic regression in R
library(ISLR)
data("Default")
summary(Default)
fit = glm(default ~ student + balance, data=Default, family = "binomial")
summary(fit)
contrasts(Default$default)

#cross validation of logistic regression
n = dim(Default)[1] #number of observations
k = 10 #10 fold cross validation
groups = c(rep(1:k,floor(n/k)), 1:(n-floor(n/k)*k))
set.seed(123) # ensures replicable results
cvgroups = sample(groups, n) #places observations in random groups
predictvals = rep(-1,n) #makes trouble shooting easier

for(i in 1:k) {
  groupi = (cvgroups == i) #all observations in group i
  fit = glm(default ~ student + balance, data=Default[!groupi,], family = "binomial")
  predictvals[groupi] = predict(fit, Default[groupi,], type = "response") #response indicates to give predicted probability vs logit or log odds
  
}
predictvals[1:3]
#produce confusion matrix based on threshold
table(predictvals > .5, Default$default)
#             Defaulted?
#                    No  Yes
#Predicted    FALSE 9628  229
#to default?  TRUE    39  104

#true positive rate = 104/(104+229), of all the people that really defaulted what fraction were we successful at predicting     
#also called the power or sensitivity of the test

#false positive rate = 39/(39+9628) of all people who did not default what fraction were we unsuccessful at predicting
#called false positive because the test came back positive that they defaulted when they did not.
#1-Specificity

#ROC Curve
install.packages("pROC")
library(pROC)
myroc = roc(response=Default$default, predictor = predictvals)
plot.roc(myroc)


#from lecture
myroc = roc(response=Default$default, predictor=predictvals)
myroc2 = roc(response=Default$default, predictor=predictvals2)
plot.roc(myroc)
plot.roc(myroc2, add=T, col="red", lty=2)
legend("bottomright", legend=c("Student + Balance", "Student + Income"),lty=c(1,2), col=c("black","red"))

#Poisson Regression
coeff = coef(fit)
xvals = seq(min(Age), max(Age), length=500)
log.means=coeff1+coeff2*xvals

plot(Age, log(Matings))
lines(xvals, log.means, lwd=2,col="red")

plot(Age, Matings)
mean.values=exp(log.means)
lines(xvals,mean.values,lwd=2,col="red")