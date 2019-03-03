cancer = read.csv("CancerSurvival01.csv")
library(tree)
mytree=tree(as.factor(Survival01)~., data=cancer)

plot(mytree)
text(mytree, pretty = 0)

summary(mytree)

mytree

#problem 5
set.seed(400)
cancer.cv = cv.tree(mytree, FUN = prune.misclass)
cancer.cv

prune.cancer = prune.misclass(mytree, best=3)
plot(prune.cancer)
text(prune.cancer, pretty=0)

#problem 6
library(gbm)
set.seed(400)
boost = gbm(Survival01~., data=cancer, distribution = "bernoulli",n.trees=5000, shrinkage = .001, interaction.depth=2)
summary(boost)

plot(boost, i="Nodes")
  

#problem 7
library(randomForest)
set.seed(400)
cancer.bag = randomForest(as.factor(Survival01)~., data=cancer, mtry = 3, importance = T) 
cancer.bag

plot(cancer.bag)
importance(cancer.bag)
varImpPlot(cancer.bag)
