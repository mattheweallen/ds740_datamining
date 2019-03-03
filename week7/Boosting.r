##Boosting
#install.packages("gbm")
library(gbm)
sonar = read.csv("Sonar_rock_metal.csv", header = F)

#redefine V61 as a numerical value
sonar$object = rep(0,length=208)
sonar$object[which(sonar$V61=="M")]=1 

#use "gaussian" for a regression problem 
#bernoulli for classification problems
boost = gbm(object~.-V61, data = sonar, distribution = "bernoulli", n.trees = 1000, shrinkage = .001, interaction.depth = 3)
boost
summary(boost)

#marginal effects
plot(boost, i="V11") #higher values of V11 are predicted of metal
plot(boost, i="V36")


#use 10 fold cross validation to estimate the error rate
n=208
k=10 #using 10-fold cross-validation
groups=c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k)) #produces list of group labels
groups

set.seed(729)
cvgroups = sample(groups,n)
boost.predict=rep(-1,n)

for(i in 1:k) {
  groupi = (cvgroups == i)
  #perform boosting and predict values of groupi
  boost = gbm(object~.-V61, data = sonar[!groupi,], distribution = "bernoulli", n.trees = 1000, shrinkage = .001, interaction.depth = 3)
  boost.predict[groupi] = predict(boost, newdata = sonar[groupi,], n.trees = 1000, type="response")
}

boost.predict[1:5] #gbm predicts probabilities not classes
table(boost.predict>.5,sonar$object)
a = table(boost.predict>.5,sonar$object)
(a[1,2] + a[2,1])/n #classification error rate
