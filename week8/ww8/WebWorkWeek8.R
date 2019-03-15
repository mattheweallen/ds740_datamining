#problem 2
# data prep / model specifications
Trees = read.csv("Trees.csv")
library(glmnet)

ncv = 5
lambdalist = exp((-1000:500)/100)
alphalist = c(0,.1,.2,.4,.6,.8,.9,1)
set.seed(8)
##############################
##entire model-fitting process##
x.in = model.matrix(Volume~.,data=Trees)[,-c(1,2)]
y.in = Trees[,2]
n.in = dim(x.in)[1]
if ((n.in%%ncv) == 0) {
  groups.in= rep(1:ncv,floor(n.in/ncv))
} else {
  #account for different-sized input matrices
  groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
}

cvgroups.in = sample(groups.in,n.in)
# with model selection 
alllambdabest = rep(NA,8)
allcv5best = rep(NA,8)
for (m in 1:8) {
  cvfit.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = alphalist[m], 
                       nfolds=ncv.in, foldid=cvgroups.in)
  plot(cvfit.in$lambda,cvfit.in$cvm,xlim=c(0,2),ylim=c(0,15)) 
  abline(v=cvfit.in$lambda[order(cvfit.in$cvm)[1]],col = "red")
  allcv5best[m] = cvfit.in$cvm[order(cvfit.in$cvm)[1]]
  alllambdabest[m] = cvfit.in$lambda[order(cvfit.in$cvm)[1]]
}
whichmodel = order(allcv5best)[1]
bestalpha = alphalist[whichmodel]
bestmodel = glmnet(x.in, y.in, alpha = bestalpha,lambda=lambdalist)
bestlambda = alllambdabest[whichmodel]

bestalpha; bestlambda
##############################


##### model assessment OUTER shell #####
ncv = 5
x.out = model.matrix(Volume~., data=Trees)[ ,-c(1,2)]
y.out = Trees[,2]
n.out = dim(x.out)[1]
# define the cross-validation splits 
groups.out = c(rep(1:ncv,floor(n.out/ncv)), 1:(n.out%%ncv))
# groups.out is a list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 
allpredictedCV.out = rep(NA,n.out)
for (j in 1:ncv)  {  # be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  trainx.out = x.out[!groupj.out, ]
  trainy.out = y.out[!groupj.out]
  testx.out = x.out[groupj.out, ]
  testy.out = y.out[groupj.out]
  ### inputs trainx.out and trainy.out  ###
  ###    :    :    :    :    :    :     ###
  ###    entire model-fitting process   ###
  
    x.in = trainx.out
    y.in = trainy.out
    n.in = dim(x.in)[1]
    if ((n.in%%ncv) == 0) {
      groups.in= rep(1:ncv,floor(n.in/ncv))
    } else {
      #account for different-sized input matrices
      groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
    }
    
    cvgroups.in = sample(groups.in,n.in)
    # with model selection 
    alllambdabest = rep(NA,8)
    allcv5best = rep(NA,8)
    for (m in 1:8) {
      cvfit.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = alphalist[m], 
                           nfolds=ncv.in, foldid=cvgroups.in)
      plot(cvfit.in$lambda,cvfit.in$cvm,xlim=c(0,2),ylim=c(0,15)) 
      abline(v=cvfit.in$lambda[order(cvfit.in$cvm)[1]],col = "red")
      allcv5best[m] = cvfit.in$cvm[order(cvfit.in$cvm)[1]]
      alllambdabest[m] = cvfit.in$lambda[order(cvfit.in$cvm)[1]]
    }
    whichmodel = order(allcv5best)[1]
    bestalpha = alphalist[whichmodel]
    bestmodel = glmnet(x.in, y.in, alpha = bestalpha,lambda=lambdalist)
    bestlambda = alllambdabest[whichmodel]
  ###   :    :    :    :    :    :      ###
  ### resulting in bestlambda, bestalpha ###
  allpredictedCV.out[groupj.out] = predict(bestmodel,
                                           newx = testx.out, s = bestlambda)
}
# assessment
CV.out = sum((allpredictedCV.out-y.out)^2)/n.out
CV.out
R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2)
R2.out


#problem #4
input = read.csv("Heart_Disease_Cleveland.csv")

names(input)
heart = input[,c(1,4,5,8,10)]
heart$HD = rep(0, length(input$DiseaseStatus))
heart$HD[which(input$DiseaseStatus > 0)] = 1
heart$HD = factor(heart$HD)

table(heart$HD)



set.seed(8)
library(MASS)
##############################
##entire model-fitting process##
xy.in = heart
n.in = dim(xy.in)[1]
ncv = 10
if ((n.in%%ncv) == 0) {
  groups.in= rep(1:ncv,floor(n.in/ncv))} else {
    groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
  }

cvgroups.in = sample(groups.in,n.in)
# with model selection 
allpredictedcv10 = matrix(rep(0,n.in*6),ncol=6)
for (i in 1:ncv) {
  newdata.in = xy.in[cvgroups.in==i,]
  lda2fit = lda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,1] = predict(lda2fit,newdata.in)$class
  lda5fit = lda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,2] = predict(lda5fit,newdata.in)$class
  qda2fit = qda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,3] = predict(qda2fit,newdata.in)$class
  qda5fit = qda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,4] = predict(qda5fit,newdata.in)$class
  log2fit = glm(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i), family=binomial)
  log2prob = predict(log2fit,newdata.in,type="response")
  log2fact = rep(0,dim(newdata.in)[1]); log2fact[log2prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,5] = log2fact
  log5fit = glm(HD ~., data= xy.in, subset=(cvgroups.in!=i),family=binomial)
  log5prob = predict(log5fit,newdata.in,type="response")
  log5fact = rep(0,dim(newdata.in)[1]); log5fact[log5prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,6] = log5fact
}
allpredictedcv10[,1:4] = allpredictedcv10[,1:4]-1
allcv10 = rep(0,6)
for (m in 1:6) allcv10[m] = sum(xy.in$HD!=allpredictedcv10[,m])/n.in
bestmodels = (1:6)[allcv10 == min(allcv10)]
##############################


hist(heart$Age)
hist(heart$BloodPressure)
hist(heart$Chol)
hist(heart$MaxHeartRate)
hist(heart$STdepress)


#problem 5
##### model assessment OUTER shell #####
nvalid = 100
xy.out = heart
n.out = dim(xy.out)[1]
#define the validation set
set.seed(8)
validset = sample(1:n.out,nvalid)
trainxy.out = xy.out[-validset,]
testxy.out = xy.out[validset,]
###        inputs trainxy.out       ###
###        :    :    :    :    :    ###
###   entire model-fitting process  ###

  xy.in = trainxy.out
  n.in = dim(xy.in)[1]
  ncv = 10
  if ((n.in%%ncv) == 0) {
    groups.in= rep(1:ncv,floor(n.in/ncv))} else {
      groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
    }
  
  cvgroups.in = sample(groups.in,n.in)
  # with model selection 
  allpredictedcv10 = matrix(rep(0,n.in*6),ncol=6)
  for (i in 1:ncv) {
    newdata.in = xy.in[cvgroups.in==i,]
    lda2fit = lda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
    allpredictedcv10[cvgroups.in==i,1] = predict(lda2fit,newdata.in)$class
    lda5fit = lda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
    allpredictedcv10[cvgroups.in==i,2] = predict(lda5fit,newdata.in)$class
    qda2fit = qda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
    allpredictedcv10[cvgroups.in==i,3] = predict(qda2fit,newdata.in)$class
    qda5fit = qda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
    allpredictedcv10[cvgroups.in==i,4] = predict(qda5fit,newdata.in)$class
    log2fit = glm(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i), family=binomial)
    log2prob = predict(log2fit,newdata.in,type="response")
    log2fact = rep(0,dim(newdata.in)[1]); log2fact[log2prob > 0.5] = 1
    allpredictedcv10[cvgroups.in==i,5] = log2fact
    log5fit = glm(HD ~., data= xy.in, subset=(cvgroups.in!=i),family=binomial)
    log5prob = predict(log5fit,newdata.in,type="response")
    log5fact = rep(0,dim(newdata.in)[1]); log5fact[log5prob > 0.5] = 1
    allpredictedcv10[cvgroups.in==i,6] = log5fact
  }
  allpredictedcv10[,1:4] = allpredictedcv10[,1:4]-1
  allcv10 = rep(0,6)
  for (m in 1:6) allcv10[m] = sum(xy.in$HD!=allpredictedcv10[,m])/n.in
  bestmodels = (1:6)[allcv10 == min(allcv10)]

###        :    :    :    :    :    ###
###      resulting in bestmodels     ###
bestmodel = ifelse(length(bestmodels)==1,bestmodels,sample(bestmodels,1))
if (bestmodel == 1)  {
  lda2fit.train = lda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
  predictvalid = as.numeric(predict(lda2fit.train, testxy.out)$class)-1
}
if (bestmodel == 2)  {
  lda5fit.train = lda(HD ~ ., data=trainxy.out)
  predictvalid = as.numeric(predict(lda5fit.train, testxy.out)$class)-1
}
if (bestmodel == 3)  {
  qda2fit.train = qda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
  predictvalid = as.numeric(predict(qda2fit.train, testxy.out)$class)-1
}
if (bestmodel == 4)  {
  qda5fit.train = qda(HD ~ ., data=trainxy.out)
  predictvalid = as.numeric(predict(qda5fit.train, testxy.out)$class)-1
}
if (bestmodel == 5)  {
  log2fit.train = glm(HD ~ MaxHeartRate + STdepress, data= trainxy.out, family=binomial)
  log2prob.test = predict(log2fit.train,testxy.out,type="response")
  predictvalid = rep(0,dim(testxy.out)[1]); predictvalid[log2prob.test > 0.5] = 1
}
if (bestmodel == 6)  {
  log5fit.train = glm(HD ~ ., data= trainxy.out, family=binomial)
  log5prob.test = predict(log5fit.train,testxy.out,type="response")
  predictvalid = rep(0,dim(testxy.out)[1]); predictvalid[log5prob.test > 0.5] = 1
}
#assessment
CV.valid = sum(testxy.out$HD!=predictvalid)/nvalid
p.valid = 1-CV.valid
