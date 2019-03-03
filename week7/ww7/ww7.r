cancer = read.csv("CancerSurvival01.csv")
library(tree)
mytree=tree(as.factor(Survival01)~., data=cancer)

plot(mytree)
text(mytree, pretty = 0)

summary(mytree)
