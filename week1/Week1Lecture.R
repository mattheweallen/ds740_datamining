#install.packages("ISLR")
#install.packages("class")

library(ISLR)
library(class)

head(Default)
dim(Default)
summary((Default))

student01 = rep(0, length(Default$student))
student01[which(Default$student == "Yes")] = 1

#balance and income have different standard deviations
sd(Default$balance)
sd(Default$income)

#standardize variables.
balance.std = scale(Default$balance)
income.std = scale(Default$income)

#put three new variables in data frame
xvals = data.frame(student01, balance.std, income.std)

#subset data into training and validate (test) sets
set.seed(123)

#standard to split 2/3 and 1/3
train = sample(1:10000, 6666, replace = F)
train.x = xvals[train,]
valid.x = xvals[-train,]
predictions = knn(train.x, valid.x, Default$default[train], k = 1)

head(predictions)

table(predictions, Default$default[-train])

#overall error rate
#predictions   No  Yes
#No            3163   62
#Yes             72   37

#overall error rate:
# (62+72)/3334 = 0.4

#proportion of people who defaulted who were misclassified
# 62/99 = .626

#play around with k
#increasing k (the number of neigbors to classify each point)
#flexibility of model decreases, variance decreases, bias increases

K = seq(1, 150, by = 2)
overall = numeric(length(K))
for(i in 1:length(K)) {
  predictions = knn(train.x, valid.x, Default$default[train], k = K[i])
  mytable = table(predictions, Default$default[-train])
  overall[i] = (mytable[1,2] + mytable[2,1])/3334
}
plot(K, overall, type="l", lwd=2)


#k nearest neighbors to solve a regression problem
#install.packages("FNN")
library(FNN)
ames = read.csv("C:/Users/matt/source/repos/ds740data/ameshousing.csv")
head(ames)
#standardize
lot.std = scale(ames$Lot.Area)
area.std = scale(ames$Gr.Liv.Area)
xvals = cbind(lot.std, area.std)

#set up training and test sets
set.seed(111)
train = sample(1:2930, 2000, replace=F)

train.x = xvals[train,]
train.y = ames$SalePrice[train]
valid.x = xvals[-train,]

#do the analysis
predictions = knn.reg(train.x, valid.x, train.y, k=10)
predictions$pred[1:10] #look at first 10

#mean squared error
mean((ames$SalePrice[-train] - predictions$pred)^2)
#do for loop here and find K that produces lowest MSE