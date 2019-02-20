#p3
#a. Download the dataset and load it into R. Define the data frame heart to include all variables, 
#but delete observations 88, 167, 193, 267, 288, and 303 (due to missing values). Define variable log.STdepress to equal log(STdepress+1), 
#and remove the original variable STdepress. 
#Set variables Sex, ChestPain, HighBloodSugar, ECG, ExerAngina, Slope, Thal, and DiseaseStatus to be factors.
input = read.csv("Heart_Disease_Cleveland.csv")
heart = input[-c(88,167,193,267,288,303),]
heart$log.STdepress = log(heart$STdepress+1)
heart$STdepress = NULL
heart$Sex = as.factor(heart$Sex)
heart$ChestPain = as.factor(heart$ChestPain)
heart$HighBloodSugar = as.factor(heart$HighBloodSugar)
heart$ECG = as.factor(heart$ECG)
heart$ExerAngina = as.factor(heart$ExerAngina)
heart$Slope = as.factor(heart$Slope)
heart$Thal = as.factor(heart$Thal)
names(heart)


#b. We want to predict log.STdepress based on all remaining predictors. A plot, plus a zoomed-in version, 
#is shown below of the CV(10) values for the Ridge Regression, LASSO, and Elastic net (with α=0.5) models, 
#plotted against the value of λ, along with a line representing the value of CV(10) for multiple linear regression 
#(constant since the model does not use a penalty).

#c. Using the data, compute the correlations between the different numeric variables. Report the maximum (absolute value) correlation below:
#  Maximum absolute value of correlation
allcors = round(cor(heart[,c(1,4,5,8,11)]),3)
abs(allcors)

#p4
#For the remaining three Webwork problems, we will work with the Boston data set from the MASS library. 
#We will be using penalized regression methods to predict log.crim, the natural log transformation of crim, 
#the per capita crime rate by suburb of Boston, as a function of the other variables. 
#You may use the help(Boston) command to learn more about the data set.
#a. Open the MASS package. Create a new data frame BostonNew as a copy of the Boston data frame. 
#Define variables log.crim to equal log(crim) and log.zn to equal log(zn+1); then delete crim and zn from BostonNew. 
#Set variable chas to be a factor.
#Finally, define predictor matrix x to include the 13 predictors as its columns, and response y to be the variable log.crim.
library(MASS)
BostonNew = Boston
BostonNew$log.crim= log(Boston$crim); BostonNew$crim = NULL
BostonNew$log.zn= log(Boston$zn+1); BostonNew$zn = NULL
BostonNew$chas = factor(Boston$chas)
names(BostonNew)
x = model.matrix(log.crim~.,data=BostonNew)[,-1]
x
y = BostonNew$log.crim

#b. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the ridge regression model at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted ridge regression model with λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn:
#Number of predictors (out of 13) that have non-zero coefficients:  
#install.packages("glmnet")
library(glmnet)
lambdalist = 1:1000/1000
lambdalist
RRfit = glmnet(x, y, lambda=lambdalist, alpha = 0)
coef(RRfit,s=0.05)


#c. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the LASSO model at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted LASSO model with λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn: 
#Number of predictors (out of 13) that have non-zero coefficients: 
LASSOfit = glmnet(x, y, lambda=lambdalist, alpha = 1)
coef(LASSOfit,s=0.05)

#d. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the elastic-net model with α = 0.50 at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted elastic-net model with α = 0.50 and λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn:  
#  Number of predictors (out of 13) that have non-zero coefficients:
ENET50fit = glmnet(x, y, lambda=lambdalist, alpha = 0.5)
coef(ENET50fit,s=0.05)

#pr5
#For this Webwork problem, we will continue to work with the BostonNew data frame as defined in Problem 4.
#We now wish to select between penalized regression models via cross-validation, using the cv.glmnet function in the glmnet package.

#a. We initialize the cross-validation groups to be the same by using set.seed(5) and then sampling from an initial listing of group labels 
#(by default, repeated listings of group numbers 1-10).
n=506
ncv = 10
groups=c(rep(1:10,50),1:6)
set.seed(5); cvgroups = sample(groups,n)

#b. Predicting log.crim based on all 13 predictors, we wish to select the best (as measured by CV(10)) 
#ridge regression model from among those across penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#The ridge regression model assessment results in the value of CV(10) being minimized at λ = 0.016; what is the value of CV(10)?
#  Value of CV(10) for ridge regression at λ = 0.016:
cvRR = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cvgroups)
min(cvRR$cvm)
order(cvRR$cvm)[1]
cvRR$lambda[order(cvRR$cvm)[1]]

#c. Predicting log.crim based on all 13 predictors, we wish to select the best (as measured by CV(10)) 
#LASSO model from among those across penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#The LASSO model assessment results in the value of CV(10) being minimized at λ = 0.021; what is the value of CV(10)?
#  Value of CV(10) for LASSO at λ = 0.021:

cvLASSO = cv.glmnet(x, y, lambda=lambdalist, alpha = 1, nfolds=ncv, foldid=cvgroups)
min(cvLASSO$cvm)
order(cvLASSO$cvm)[1]
cvLASSO$lambda[order(cvLASSO$cvm)[1]]

#d. Predicting log.crim based on all 13 predictors, we wish to select the best (as measured by CV(10)) elastic-net model, 
#with α = 0.50, from among those across penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#The elastic-net model assessment results in the value of CV(10) being minimized at λ = 0.035; what is the value of CV(10)?
#  Value of CV(10) for elastic-net at α = 0.50 and λ = 0.035:
  
cvElasticNet = cv.glmnet(x, y, lambda=lambdalist, alpha = .5, nfolds=ncv, foldid=cvgroups)
min(cvElasticNet$cvm)
order(cvElasticNet$cvm)[1]
cvElasticNet$lambda[order(cvElasticNet$cvm)[1]]

#pr 6
# We finally wish to estimate variability of coefficients for penalized regression models, 
#using the boot function in the corresponding package to produce bootstrapped estimates of the standard deviations of the coefficients.
# a. Predicting log.crim based on all 13 predictors, we use the ridge regression model at penalty tuning parameter value λ = 0.016. 
#Starting with set.seed(5), use the boot function, with R= 1000 resamples, 
#to produce bootstrapped estimates of the standard deviations of the coefficients for the ridge regression model at λ = 0.016.
# Estimated standard deviation for intercept, SEB(β^0):  
#   Estimated standard deviation for coefficient of ptratio, SEB(β^9):  
#   Estimated standard deviation for coefficient of log.zn, SEB(β^13):
library(boot)

bestlambdaRR = 0.016
beta.fn.RR = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  RRfitboot = glmnet(xboot, yboot, alpha = 0,lambda=lambdalist)
  return(coef(RRfitboot,s= bestlambdaRR)[,1])
}
set.seed(5)
RRbootoutput = boot(cbind(y,x),beta.fn.RR,R=1000)
print(RRbootoutput)
SE_RR = round(apply((RRbootoutput$t),2,sd),6)
SE_RR[c(1,10,14)]

 
#   b. Predicting log.crim based on all 13 predictors, we use the LASSO model at penalty tuning parameter value λ = 0.021. 
#Starting with set.seed(5), use the boot function, with R= 1000 resamples, 
#to produce bootstrapped estimates of the standard deviations of the coefficients for the LASSO model at λ = 0.021.
# Estimated standard deviation for intercept, SEB(β^0):  
#   Estimated standard deviation for coefficient of ptratio, SEB(β^9):  
#   Estimated standard deviation for coefficient of log.zn, SEB(β^13):  
   
bestlambdaLASSO = 0.021
beta.fn.LASSO = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  LASSOfitboot = glmnet(xboot, yboot, alpha = 1,lambda=lambdalist)
  return(coef(LASSOfitboot,s= bestlambdaLASSO)[,1])
}
set.seed(5)
LASSObootoutput = boot(cbind(y,x),beta.fn.LASSO,R=1000)
print(LASSObootoutput)
SE_LASSO = round(apply((LASSObootoutput$t),2,sd),6)
SE_LASSO[c(1,10,14)]

#   c. Predicting log.crim based on all 13 predictors, we use the elastic-net model, 
#with α = 0.50, at penalty tuning parameter value λ = 0.035. Starting with set.seed(5), use the boot function, 
#with R= 1000 resamples, to produce bootstrapped estimates of the standard deviations of the coefficients for the elastic-net model, 
#with α = 0.50, at λ = 0.035.
# Estimated standard deviation for intercept, SEB(β^0):  
#   Estimated standard deviation for coefficient of ptratio, SEB(β^9):  
#   Estimated standard deviation for coefficient of log.zn, SEB(β^13):  

bestlambdaElasNet = 0.035
beta.fn.ElasNet = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  ElasNetfitboot = glmnet(xboot, yboot, alpha = .5,lambda=lambdalist)
  return(coef(ElasNetfitboot,s= bestlambdaElasNet)[,1])
}
set.seed(5)
ElasNetbootoutput = boot(cbind(y,x),beta.fn.ElasNet,R=1000)
print(ElasNetbootoutput)
SE_ElasNet = round(apply((ElasNetbootoutput$t),2,sd),6)
SE_ElasNet[c(1,10,14)]

#   d. The corresponding bootstrapped estimates of the standard deviations of the coefficients for the multiple linear (least-squares) 
#regression model are shown below:
#   Estimated standard deviation for intercept, SEB(β^0) = 1.025231
# Estimated standard deviation for coefficient of ptratio, SEB(β^9) = 0.026506
# Estimated standard deviation for coefficient of log.zn, SEB(β^13) = 0.030825
# As expected, the bootstrapped estimates of the standard deviations of the coefficients for the penalized regression methods are: 
#   A. larger than those for multiple linear regression. 
# B. smaller than those for multiple linear regression.