#stanford statistical learning chapter 9
library(MASS)
?mvrnorm
#https://github.com/asadoughi/stat-learning/blob/d1e9a3322ca0a42cb91c83b3141bdcdab9c81267/ch9/9.R
#9.R Review Question
class_0 = mvrnorm(n = 50, 0, )
class_1 = mvrnorm(n = 50, )

## Online course quiz question: 9.R
## Explanation: Logistic regression is similar to SVM with a linear kernel.

library(MASS)
#install.packages("e1071")
library(e1071)
# build training set
x0 = mvrnorm(50,rep(0,10),diag(10))
x1 = mvrnorm(50,rep(c(1,0),c(5,5)),diag(10))
train = rbind(x0,x1)
classes = rep(c(0,1),c(50,50))
dat=data.frame(train,classes=as.factor(classes))

# fit
svmfit=svm(classes~.,data=dat)
summary(svmfit)


svm_error <- function() {
  # 1) generate a random training sample to train on + fit
  
  # build training set
  x0 = mvrnorm(50,rep(0,10),diag(10))
  x1 = mvrnorm(50,rep(c(1,0),c(5,5)),diag(10))
  train = rbind(x0,x1)
  classes = rep(c(0,1),c(50,50))
  dat=data.frame(train,classes=as.factor(classes))
  
  # fit
  svmfit=svm(classes~.,data=dat,kernel="linear")
  #svmfit = glm(classes~., data=dat, family="binomial")
  
  # 2) evaluate the number of mistakes we make on a large test set = 1000 samples
  test_x0 = mvrnorm(500,rep(0,10),diag(10))
  test_x1 = mvrnorm(500,rep(c(1,0),c(5,5)),diag(10))
  test = rbind(test_x0,test_x1)
  test_classes = rep(c(0,1),c(500,500))
  test_dat = data.frame(test,test_classes=as.factor(test_classes))
  fit = predict(svmfit,test_dat)
  fit = ifelse(fit < 0.5, 0, 1)
  error = sum(fit != test_dat$test_classes)/1000
  
  return(error)
}

# 3) repeat (1-2) many times and averaging the error rate for each trial
errors = replicate(1000, svm_error())

print(mean(errors))
