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
