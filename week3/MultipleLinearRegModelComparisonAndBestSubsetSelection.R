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
