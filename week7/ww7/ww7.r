cancer = read.csv("CancerSurvival01.csv")
library(tree)
mytree=tree(as.factor(Survival01)~., data=cancer)