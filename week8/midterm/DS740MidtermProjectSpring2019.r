#R code for ds 740 midterm project
#Matt Allen

#read in the data
ais = read.csv("ais.csv")
pairs(ais)
#make sex into factor variable
ais$Sex <- factor(ais$Sex,levels = c(0,1),labels = c("Male", "Female"))
summary(ais$Sex)

#plot histograms of variable to check for normality
hist(ais$Bfat, xlab="Body Fat", main = "Distribution of Body Fat") #possible response variable looks skewed
shapiro.test(ais$Bfat)

ais$Bfat = log(ais$Bfat) #use log of bfat to fix right skewness
hist(ais$Bfat, xlab="Body Fat", main = "Distribution of Log Transformed Body Fat")

ais = ais[,c(2:13)]

############################################################################################
##### Double cross-validation with a pre-defined cv function for inner model selection #####				 
############################################################################################
#install.packages("glmnet")
library(glmnet)  # use LASSO model from package glmnet 
lambdalist = c(0:10)/100  # defines models to consider
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

ModelFinal = (Bfat ~ Sex+Wt+SSF) # remove ht because it is not significant. SSF is a better predictor
finalModel = lm(formula = ModelFinal,data=ais)
summary(finalModel)
par(mfrow = c(2, 2))
plot(finalModel) #create diagnostic plots

#use bisquare method to put less weight on outliers
library(MASS)
fit.bisquare = rlm(ModelFinal, data = ais, psi = psi.bisquare)
summary(fit.bisquare)
fit.bisquare

#install.packages("car")
#show the weights used in the bisquare
library(car)

plot(fit.bisquare$w, las=1, cex.axis=1.2, ylab="Weights")
smallweights = which(fit.bisquare$w < .1)
smallweights
fit.bisquare$w[200]
fit.bisquare$w[201]
fit.bisquare$w[202]
