#https://www.mayoclinic.org/tests-procedures/hematocrit/about/pac-20384728
#https://www.rdocumentation.org/packages/alr4/versions/1.0.5/topics/ais
ais = read.csv("ais.csv")
#pairs(ais)
#summary(ais)

#sum(ais$Sex[which(ais$Sex == 1)])
#class(ais$Sport)
#class(ais$Sex)
#ais$Sex = as.factor(ais$Sex)
ais$Sex <- factor(ais$Sex,levels = c(0,1),labels = c("Male", "Female"))


#summary(ais$Sex)
#males = which(ais$Sex == 0)
#ais$Sex = "Female"  
#ais$Sex[males] = "Male"
#summary(ais$Sport)
#names(ais)
#k nearest neighbors sport

#table(ais$Sport) #response
#table(ais$Sex) 

#plot histograms of variable to check for normality
#hist(ais$Bfat) #possible response variable looks skewed

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
ball_sports = c("b_ball", "w_polo", "tennis", "netball")
ais$ballsport = 0

ais$ballsport[which(ais$Sport %in% ball_sports)] = 1
ais$ballsport <- factor(ais$ballsport,levels = c(0,1),labels = c("No Ball", "Ball"))
table(ais$ballsport)




##### model assessment OUTER CV (with model selection INNER CV as part of model-fitting) #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the cross-validation splits 
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = bodyfat[!groupj.out,]
  trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
  trainy.out = traindata.out[,3]
  testdata.out = bodyfat[groupj.out,]
  testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
  testy.out = testdata.out[,3]
  ### entire model-fitting process ###
  fulldata.in = traindata.out  # only input the data used to fit the model
  x.in = model.matrix(BodyFatSiri~.,data=fulldata.in)[,-(1:4)]
  y.in = fulldata.in[,3]
  k.in = 10 
  n.in = dim(fulldata.in)[1]
  groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
  #    set.seed(8)   # do not reset seed for each internal loop
  cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
  #LASSO cross-validation
  cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = 1, nfolds=k.in, foldid=cvgroups.in)
  plot(cvLASSOglm.in$lambda,cvLASSOglm.in$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
       xlim=c(0,3),ylim = c(15,25))
  whichlowestcvLASSO.in = order(cvLASSOglm.in$cvm)[1]; min(cvLASSOglm.in$cvm)
  bestlambdaLASSO = (cvLASSOglm.in$lambda)[whichlowestcvLASSO.in]; bestlambdaLASSO
  abline(v=bestlambdaLASSO)
  ### resulting in bestlambdaLASSO ###
  LASSOtrainfit.out = glmnet(trainx.out, trainy.out, alpha = 1,lambda=lambdalist)
  allpredictedCV.out[groupj.out] = predict(LASSOtrainfit.out,newx=testx.out,s=bestlambdaLASSO)
}
#assessment
y.out = fulldata.out$BodyFatSiri
CV.out = sum((allpredictedCV.out-y.out)^2)/n.out
R2.out = 1-sum((allpredictedCV.out-y.out)^2)/sum((y.out-mean(y.out))^2)
CV.out; R2.out



