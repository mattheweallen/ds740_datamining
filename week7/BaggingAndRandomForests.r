#bagging stands for bootstrap aggregation
#install.packages("randomForest")
library(randomForest)
sonar = read.csv("Sonar_rock_metal.csv", header = F)

sonar.bag = randomForest(V61~., data=sonar,mtry=60,importance=T)
#could use subset = train
#mtry is number of predictor variables

sonar.bag
plot(sonar.bag)
legend("topright",colnames(sonar.bag$err.rate),col=1:3,lty=1:3)
#oob = out of bag
#r = rocks
#m = minerals

#variable importance
importance(sonar.bag)
varImpPlot(sonar.bag)

#marginal effect of each variable
partialPlot(sonar.bag, pred.data = sonar, x.var = V11, which.class = "M")


sonar.rf = randomForest(V61~.,data=sonar,mtry=8,importance=T)
plot(sonar.rf)
