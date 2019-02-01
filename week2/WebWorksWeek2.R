#install.packages("MASS")
library(MASS)

data("Boston")
head(Boston)
?Boston
summary(Boston)
dim(Boston)


#set up training and test sets
#install.packages("FNN")
library(FNN)
set.seed(100)
train = sample(1:506, 350, replace=F)

train.x = Boston[train,c("age","rad")]
train.y = Boston[train,"crim"]
valid.x = Boston[-train,c("age","rad")]
plot(Boston$age, Boston$crim)
plot(Boston$rad, Boston$crim)
hist(Boston$age)
hist(Boston$rad)
hist(Boston$crim)


train.x.std = scale(train.x)

attr(train.x.std, "scaled:center")
attr(train.x.std, "scaled:scale")

#standardize validation set with mean and standard deviation of the training set
valid.x.std = scale(valid.x, center = attr(train.x.std, "scaled:center"), scale = attr(train.x.std, "scaled:scale"))

#do the analysis
predictions = knn.reg(train.x.std, valid.x.std, train.y, k=25)
predictions$pred[1:10] #look at first 10

#mean squared error
mean((Boston[-train,"crim"] - predictions$pred)^2)

length(predictions$pred) 
length(Boston[-train,"crim"])

#finish web works week 1
#prob 1 week 2
attach(Boston)
age.std = scale(age)
rad.std = scale(rad)
BostonStd = data.frame(age,age.std,rad,rad.std,crim)
dim(BostonStd)
x.std = cbind(BostonStd$age.std,BostonStd$rad.std)

#do the analysis
predictions = knn.reg(x.std, x.std, crim, k=25)
predictions$pred[1:10] #look at first 10

#mean squared error
mean((BostonStd[,"crim"] - predictions$pred)^2)


#prob 2 week 2
num_examples = dim(Boston)[1]

#mse = sample(groups,n)  #orders randomly, with seed (2) to determine starting point
#cvgroups
#prediction via cross-validation
allpredictedCV = rep(0,num_examples)
for (i in 1:num_examples)  {
  train.x = Boston[-i,c("age","rad")]
  train.y = Boston[-i,"crim"]
  valid.x = Boston[i,c("age","rad")]
  
  train.x.std = scale(train.x)
  attr(train.x.std, "scaled:center")
  attr(train.x.std, "scaled:scale")
  
  #standardize validation set with mean and standard deviation of the training set
  valid.x.std = scale(valid.x, center = attr(train.x.std, "scaled:center"), scale = attr(train.x.std, "scaled:scale"))
  
  #do the analysis
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k=25)
  #print(class(predictions$pred))
  print(predictions$pred)
  allpredictedCV[i] = predictions$pred
  
  
  #mean squared error
  #mean((Boston[-train,"crim"] - predictions$pred)^2)
  
  #mean squared error
  #mean((BostonStd[,"crim"] - predictions$pred)^2)
}

#plot(Fullfit$fitted.values,BodyFatSiri)
#abline(0,1)
#points(allpredictedCV,BodyFatSiri,pch=20,col="red")

#the cross-validation assessment part
LeaveOneOutCV = sum((Boston[,"crim"]-allpredictedCV)^2)/num_examples
LeaveOneOutCV


#problem 3 web works
k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(num_examples/k)),1:(num_examples-floor(num_examples/k)*k))  #produces list of group labels
#groups

#floor(num_examples/k)
#num_examples-floor(num_examples/k)*k

set.seed(100)
cvgroups = sample(groups,num_examples)  #orders randomly, with seed (2) to determine starting point
cvgroups
#prediction via cross-validation
allpredictedCV = rep(0,num_examples)

for (i in 1:k)  {
  groupi = (cvgroups == i)
  #lmfitCV = lm(formula = Fullmodel,data=bodyfat,subset=!groupi)
  #allpredictedCV[groupi] = predict.lm(lmfitCV,bodyfat[groupi,])
  
  train.x = Boston[!groupi,c("age","rad")]
  train.y = Boston[!groupi,"crim"]
  valid.x = Boston[groupi,c("age","rad")]
  
  train.x.std = scale(train.x)
  attr(train.x.std, "scaled:center")
  attr(train.x.std, "scaled:scale")
  
  #standardize validation set with mean and standard deviation of the training set
  valid.x.std = scale(valid.x, center = attr(train.x.std, "scaled:center"), scale = attr(train.x.std, "scaled:scale"))
  
  #do the analysis
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k=25)
  #print(class(predictions$pred))
  #print(predictions$pred)
  #print(length(predictions$pred))
  #print(length(Boston[groupi,"crim"]))
  #allpredictedCV[i] = ((Boston[groupi,"crim"]-predictions$pred)^2)/length(predictions$pred)
  #allpredictedCV[i] = mean((Boston[groupi,"crim"]-predictions$pred)^2)
  allpredictedCV[groupi] = predictions$pred
}

#mean(allpredictedCV)
mean( (Boston[,"crim"] - allpredictedCV)^2 )

#problem 3 hint
mfoldCVpredictions = rep(NA,n)
for (fold in 1:m) {
  train.x = x.std[cvgroups != fold,]
  train.x.std = scale(train.x)
  train.y = y[cvgroups != fold]
  valid.x = x.std[cvgroups == fold,]
  valid.x.std = scale(valid.x, 
                      center = attr(train.x.std, "scaled:center"), 
                      scale = attr(train.x.std, "scaled:scale"))
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k = 25)
  mfoldCVpredictions[cvgroups == fold] = predictions$pred
}
mean( (y - mfoldCVpredictions)^2 )

