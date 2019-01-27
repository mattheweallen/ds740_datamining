library(ISLR)
set.seed(1)
train=sample(392,196)
?sample
?lm.fit
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
#pre
#validation set approach
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#leave one out cross validation
library(boot) #contains cv.glm
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err = cv.glm(Auto,glm.fit)
#first delta is standard k fold cross validation estimate, second is bias corrected
#here they are the same
cv.err$delta
#look at different test mse for error
cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
} 
cv.error

#k-fold cross-validation
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
} 
cv.error.10

#bootstrap
data("Portfolio")
head(Portfolio)
summary(Portfolio)
attach(Portfolio)
plot(X,Y)
