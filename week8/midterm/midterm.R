#https://www.mayoclinic.org/tests-procedures/hematocrit/about/pac-20384728
#https://www.rdocumentation.org/packages/alr4/versions/1.0.5/topics/ais
#require(MASS)

ais = read.csv("ais.csv")
#pairs(ais)
#summary(ais)

#sum(ais$Sex[which(ais$Sex == 1)])
#class(ais$Sport)
#class(ais$Sex)
#ais$Sex = as.factor(ais$Sex)
ais$Sex <- factor(ais$Sex,levels = c(0,1),labels = c("Male", "Female"))


summary(ais$Sex)
#males = which(ais$Sex == 0)
#ais$Sex = "Female"  
#ais$Sex[males] = "Male"
#summary(ais$Sport)
#names(ais)
#k nearest neighbors sport

#table(ais$Sport) #response
#table(ais$Sex) 

#plot histograms of variable to check for normality
hist(ais$Bfat, xlab="Body Fat", main = "Distribution of Body Fat") #possible response variable looks skewed
ais$log_bfat = log(ais$Bfat)
shapiro.test(ais$log_bfat)

hist(ais$log_bfat, xlab="Body Fat", main = "Distribution of Log Transformed Body Fat")


hist(ais$Ht)
hist(ais$Wt)
hist(ais$LBM)
hist(ais$RCC)
hist(ais$WCC)
hist(ais$Hc)
hist(ais$Hg)
hist(ais$Ferr)
hist(ais$BMI)
hist(ais$SSF)



#Make variable Ball Sport if 1 if sport involves ball and zero if no ball involved.
#are there variables that predict whether an athletes plays a sport involving a ball.
#ball_sports = c("b_ball", "w_polo", "tennis", "netball")
#ais$ballsport = 0

#ais$ballsport[which(ais$Sport %in% ball_sports)] = 1
#ais$ballsport <- factor(ais$ballsport,levels = c(0,1),labels = c("No Ball", "Ball"))
ais = ais[,c(2:13)]
#table(ais$ballsport)

pairs(ais)

############################################################################################
##### Double cross-validation with a pre-defined cv function for inner model selection #####				 
############################################################################################
#install.packages("glmnet")
library(glmnet)  # use LASSO model from package glmnet 
lambdalist = c(0:10)/100  # defines models to consider


##### model selection ##### reference - PenalizedModelAssessment.R from Lesson 5
fulldata.in = ais
x.in = model.matrix(Bfat~.,data=fulldata.in)[,-1]
y.in = fulldata.in[,1]
k.in = 10 
n.in = dim(fulldata.in)[1]
groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
set.seed(8)
cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
#LASSO cross-validation
cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = 1, nfolds=k.in, foldid=cvgroups.in)
plot(cvLASSOglm.in$lambda,cvLASSOglm.in$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)")
whichlowestcvLASSO.in = order(cvLASSOglm.in$cvm)[1]; min(cvLASSOglm.in$cvm)
bestlambdaLASSO = (cvLASSOglm.in$lambda)[whichlowestcvLASSO.in]; bestlambdaLASSO
abline(v=bestlambdaLASSO)
bestlambdaLASSO  # this is the lambda for the best LASSO model
LASSOfit.in = glmnet(x.in, y.in, alpha = 1,lambda=lambdalist)  # fit the model across possible lambda
LASSObestcoef = coef(LASSOfit.in, s = bestlambdaLASSO); LASSObestcoef # coefficients for the best model fit








##### model assessment OUTER shell #####
# fulldata.out = bodyfat
# k.out = 10 
# n.out = dim(fulldata.out)[1]
# #define the cross-validation splits 
# groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
# set.seed(8)
# cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 
# 
# allpredictedCV.out = rep(NA,n.out)
# for (j in 1:k.out)  {  #be careful not to re-use loop indices
#   groupj.out = (cvgroups.out == j)
#   traindata.out = bodyfat[!groupj.out,]
#   trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
#   trainy.out = traindata.out[,3]
#   testdata.out = bodyfat[groupj.out,]
#   testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
#   testy.out = testdata.out[,3]
#   ### entire model-fitting process ###
#   ###	:	:	:	:	:	:	:  ###
#   ###   :	:	:	:	:	:	:  ###
#   ### resulting in bestlambdaLASSO ###
#   LASSOtrainfit.out = glmnet(trainx.out, trainy.out, alpha = 1,lambda=lambdalist)
#   allpredictedCV.out[groupj.out] = predict(LASSOtrainfit.out,newx=testx.out,s=bestlambdaLASSO)
# }
# #assessment
# y.out = fulldata.out$BodyFatSiri
# CV.out = sum((allpredictedCV.out-y.out)^2)/n.out; CV.out
# R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2); R2.out






##### model assessment OUTER CV (with model selection INNER CV as part of model-fitting) #####
fulldata.out = ais
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the cross-validation splits 
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = ais[!groupj.out,]
  trainx.out = model.matrix(Bfat~.,data=traindata.out)[,-1]
  trainy.out = traindata.out[,1]
  testdata.out = ais[groupj.out,]
  testx.out = model.matrix(Bfat~.,data=testdata.out)[,-1]
  testy.out = testdata.out[,1]
  ### entire model-fitting process ###
  fulldata.in = traindata.out  # only input the data used to fit the model
  x.in = model.matrix(Bfat~.,data=fulldata.in)[,-1]
  y.in = fulldata.in[,1]
  k.in = 10 
  n.in = dim(fulldata.in)[1]
  groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
  #    set.seed(8)   # do not reset seed for each internal loop
  cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
  #LASSO cross-validation
  cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = 1, nfolds=k.in, foldid=cvgroups.in)
  plot(cvLASSOglm.in$lambda,cvLASSOglm.in$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)")
  whichlowestcvLASSO.in = order(cvLASSOglm.in$cvm)[1]; min(cvLASSOglm.in$cvm)
  bestlambdaLASSO = (cvLASSOglm.in$lambda)[whichlowestcvLASSO.in]; bestlambdaLASSO
  abline(v=bestlambdaLASSO)
  ### resulting in bestlambdaLASSO ###
  LASSOtrainfit.out = glmnet(trainx.out, trainy.out, alpha = 1,lambda=lambdalist)
  allpredictedCV.out[groupj.out] = predict(LASSOtrainfit.out,newx=testx.out,s=bestlambdaLASSO)
}
#assessment
y.out = fulldata.out$Bfat
CV.out = sum((allpredictedCV.out-y.out)^2)/n.out
R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2)
CV.out; R2.out



























###############################################################################################
##### Double cross-validation with inner loop for selection and outer loop for assessment #####				 
###############################################################################################

#model list specification
nmodels = 11
Model1 = (Bfat ~ Sex)
Model2 = (Bfat ~ Sex+Ht)
Model3 = (Bfat ~ Sex+Ht+Wt)
Model4 = (Bfat ~ Sex+Ht+Wt+SSF)
Model5 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC)
Model6 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC)
Model7 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC+Hc)
Model8 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC+Hc+Hg)
Model9 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC+Hc+Hg+Ferr)
Model10 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC+Hc+Hg+Ferr+BMI)
Model11 = (Bfat ~ Sex+Ht+Wt+SSF+LBM+RCC+WCC+Hc+Hg+Ferr+BMI)
allModels = list(Model1,Model2,Model3,Model4,Model5,Model6,Model7,
                 Model8,Model9,Model10,Model11)	













# ##### model selection ##### reference - RcmdsCVselection from lesson 2
# fulldata.in = ais
# k.in = 10 
# n.in = dim(fulldata.in)[1]
# groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
# set.seed(8)
# cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
# 
# allpredictedCV.in = matrix(rep(NA,n.in*nmodels),ncol=nmodels)
# for (i in 1:k.in)  {
#   groupi.in = (cvgroups.in == i)
#   #fit the model
#   #placing various model fits INSIDE cross-validation loop makes the program easier-to-adapt to new model fits
#   for (m in 1:nmodels) {
#     lmfitCV.in = lm(formula = allModels[[m]],data=bodyfat,subset=!groupi.in)
#     allpredictedCV.in[groupi.in,m] = predict.lm(lmfitCV.in,fulldata.in[groupi.in,])
#   }
# }
# allmodelCV.in = rep(NA,nmodels) #place-holder for results
# for (m in 1:nmodels) { allmodelCV.in[m] = sum((allpredictedCV.in[,m]-fulldata.in$BodyFatSiri)^2)/n.in}
# plot(1:nmodels,allmodelCV.in,col="red",pch=20)
# bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
# 
# REGRbestcoef = coef(lm(formula = allModels[[bestmodel.in]], data=fulldata.in)); REGRbestcoef # coefficients for the best model fit
# bestmodel.in
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##### model assessment OUTER CV shell #####
# fulldata.out = bodyfat
# k.out = 10 
# n.out = dim(fulldata.out)[1]
# #define the cross-validation splits for assessment
# groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
# set.seed(8)
# cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 
# 
# allpredictedCV.out = rep(NA,n.out)
# for (j in 1:k.out)  {  #be careful not to re-use loop indices
#   groupj.out = (cvgroups.out == j)
#   traindata.out = bodyfat[!groupj.out,]
#   testdata.out = bodyfat[groupj.out,]
#   ### entire model-fitting process ###
#   ###	:	:	:	:	:	:	:  ###
#   ###   :	:	:	:	:	:	:  ###
#   ###  resulting in bestmodel.in   ###
#   lmfitCV.out = lm(allModels[[bestmodel.in]],traindata.out)
#   allpredictedCV.out[groupj.out] = predict.lm(lmfitCV.out,testdata.out)
# }
# #assessment
# y.out = fulldata.out$BodyFatSiri
# CV.out = sum((allpredictedCV.out-y.out)^2)/n.out
# R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2)
# CV.out; R2.out









##### model assessment OUTER CV (with model selection INNER CV as part of model-fitting) #####
fulldata.out = ais
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the cross-validation splits for assessment
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = ais[!groupj.out,]
  testdata.out = ais[groupj.out,]
  ### entire model-fitting process ###
  fulldata.in = traindata.out   # only input the data used to fit the model
  k.in = 10 
  n.in = dim(fulldata.in)[1]
  groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
  #    set.seed(8)   # do not reset seed for each internal loop
  cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
  
  allpredictedCV.in = matrix(rep(NA,n.in*nmodels),ncol=nmodels)  
  for (i in 1:k.in)  {
    groupi.in = (cvgroups.in == i)
    for (m in 1:nmodels) {
      lmfitCV.in = lm(formula = allModels[[m]],data=ais,subset=!groupi.in)
      allpredictedCV.in[groupi.in,m] = predict.lm(lmfitCV.in,fulldata.in[groupi.in,])
    }
  }
  allmodelCV.in = rep(NA,nmodels) #place-holder for results
  for (m in 1:nmodels) { allmodelCV.in[m] = sum((allpredictedCV.in[,m]-fulldata.in$Bfat)^2)/n.in}
  plot(1:nmodels,allmodelCV.in,col="red",pch=20)
  bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
  ###  resulting in bestmodel.in   ###
  lmfitCV.out = lm(allModels[[bestmodel.in]],traindata.out)
  allpredictedCV.out[groupj.out] = predict.lm(lmfitCV.out,testdata.out)
}
#assessment
y.out = fulldata.out$Bfat
CV.out = sum((allpredictedCV.out-y.out)^2)/n.out
R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2)
CV.out; R2.out


#based on the plots use model that has 4 measurements. model does not improve after that. use full data set to estimate coefficients
#pick 4 easiest quantities to measure Sex, Ht, Wt, SSF (sum of skin folds)
#Describe sum of skin folds procedure

#https://www.verywellfit.com/calculate-body-fat-by-measuring-skinfolds-3120273

ModelFinal = (log_bfat ~ Sex+Wt+SSF)
finalModel = lm(formula = ModelFinal,data=ais)
finalModel
summary(finalModel)
hist(ais$Ht)

#in the final model it appears that Wt and Ht are not significant, so can further simplify model to just be SSF and Sex

#show histograms of predictors.

#perform shapiro test to show normality assumption to be true
shapiro.test(ais$Bfat) #response not normal, maybe transform
hist(ais$Bfat)

shapiro.test(ais$Ht)
shapiro.test(ais$Wt)
shapiro.test(ais$LBM) #not normal
shapiro.test(ais$RCC) #not normal
shapiro.test(ais$WCC) #not normal
shapiro.test(ais$Hc) #not normal
shapiro.test(ais$Hg)  #not normal
shapiro.test(ais$Ferr) #not normal
shapiro.test(ais$BMI) #not normal
shapiro.test(ais$SSF)  #not normal


#take log of all variables that are not normal
ais$Bfat = log(ais$Bfat)
ais$LBM = log(ais$LBM)
ais$RCC = log(ais$RCC)
ais$WCC = log(ais$WCC)
ais$Hc = log(ais$Hc)
ais$Hg = log(ais$Hg)
ais$Ferr = log(ais$Ferr)
ais$BMI = log(ais$BMI)
ais$SSF = log(ais$SSF)
