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

#b. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the ridge regression model at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted ridge regression model with λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn:
#Number of predictors (out of 13) that have non-zero coefficients:  
library(MASS)
BostonNew = Boston
BostonNew$log.crim= log(Boston$crim); BostonNew$crim = NULL
BostonNew$log.zn= log(Boston$zn+1); BostonNew$zn = NULL
BostonNew$chas = factor(Boston$chas)
names(BostonNew)
x = model.matrix(log.crim~.,data=BostonNew)[,-1]
y = BostonNew$log.crim


#c. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the LASSO model at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted LASSO model with λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn: 
#Number of predictors (out of 13) that have non-zero coefficients: 

#d. We want to predict log.crim based on all 13 predictors. Using the glmnet function in the corresponding package, 
#fit the elastic-net model with α = 0.50 at penalty tuning parameter values λ = 0.001, 0.002, ..., 0.999, 1.000.
#For the fitted elastic-net model with α = 0.50 and λ = 0.05, report the following information:
#  Fitted coefficient of ptratio:  
#  Fitted coefficient of log.zn:  
#  Number of predictors (out of 13) that have non-zero coefficients:

