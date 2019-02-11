library(ISLR)
myColleges = College[-96,] # Remove an observation with an unrealistic graduation rate (> 100%)
myColleges = myColleges[,c(4, 7,11, 15,18)] # Examine a subset of variables so the scatterplots aren't too small to read
pairs(myColleges)

#variance inflation factor
#VIF_i = 1 / (1-R_i^2)
#R_i^2 is the R^2 from regression on all other predictor variables
fit = lm(Grad.Rate ~ ., data=myColleges)
#install.packages("car")
#install.packages("Rcpp")
library(car)
vif(fit)
#VIF >= 10 indicates 90% or more of variation explained by other predictors
#may want to remove or replace it by its residuals

#sample problem
cafe = read.csv("cafedata_subset.csv")
colnames(cafe)

pairs(cafe[,1:5])

summary(cafe[,1:2])
table(cafe[,3:4])

fit = lm(Sales ~ .-Date-Day.Code,data=cafe) #still have singularities and high standard errors
summary(fit)

#redundant relationships, perfect linear relationship, variable do not add any more information
plot(cafe$Total.Items.Wasted, cafe$Bread.Sand.Waste+cafe$Wraps.Waste+cafe$Muffins.Waste+cafe$Cookies.Waste+cafe$Fruit.Cup.Waste)

plot(cafe$Sodas+cafe$Coffees,cafe$Total.Soda.and.Coffee)

fit = lm(Sales ~ t + Day.of.Week + Bread.Sand.Sold + Wraps.Sold + Muffins.Sold + Cookies.Sold + Fruit.Cup.Sold + Chips + Juices + Sodas + Coffees + Max.Temp + Total.Items.Wasted, data=cafe)
summary(fit) #high standard errors

library(car)
vif(fit)

#cross validation



#define a predict() function for regsubset objects, try not running and see if predict function below works
predict.regsubsets <- function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id=id)
    xvars = names(coefi)
    mat[,xvars] %*% coefi
} #end function predict.regsubsets

#reg
regfit = regsubsets(Sales ~ t + Day.of.Week + Bread.Sand.Sold + Wraps.Sold + Muffins.Sold + Cookies.Sold + Fruit.Cup.Sold + Chips + Juices + Sodas + Coffees + Max.Temp + Total.Items.Wasted, data=cafe, nvmax=16)

#cross validation of logistic regression
#n = dim(Default)[1] #number of observations
library(leaps)
n=42
k = 10 #10 fold cross validation
groups = c(rep(1:k,floor(n/k)), 1:(n-floor(n/k)*k))

set.seed(246) # ensures replicable results
cvgroups = sample(groups, n) #places observations in random groups
group.error = matrix(,nr=16, nc=k) #row = number of variables, column = which fold

for(i in 1:k) {
  groupi = (cvgroups == i) #all observations in group i
  cv.fit = regsubsets(Sales ~ t + Day.of.Week + Bread.Sand.Sold + Wraps.Sold + Muffins.Sold + Cookies.Sold + Fruit.Cup.Sold + Chips + Juices + Sodas + Coffees + Max.Temp + Total.Items.Wasted, data=cafe[!groupi,], nvmax=16)
  
  for(j in 1:16) {
    y.pred = predict(cv.fit, newdata = cafe[groupi,], id=j)
    group.error[j,i] = mean((cafe$Sales[groupi] - y.pred)^2) 
  } #end iter over model size
} #end iter over folds

MSE = apply(group.error, 1, mean)
plot(MSE)
which.min(MSE)

se = apply(group.error,1,sd)/sqrt(k)
se[14]
which(MSE <= MSE[14] + se[14])
#based on one standard error rule choose 2, most parsimonious,
coef(regfit,2)

class(regfit)
#i think when predict in the loop is called it looks for predict. + class(regfit) or predict.regsubsets