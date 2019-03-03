sonar = read.csv("Sonar_rock_metal.csv", header = F)
summary(sonar_rock_metal)
dim(sonar)

set.seed(728)
train = sample(1:dim(sonar)[1],140,replace=F)
train

#install.packages("tree")
library(tree)
mytree = tree(V61~., data = sonar[train,])
summary(mytree)

plot(mytree)
text(mytree, pretty=0)


item.pred = predict(mytree, sonar[-train,], type="class")
#type = "class", returns predicted response classes
#type = "vector", returns predicted probabilities

#confusion matrix
table(item.pred, sonar$V61[-train])
15/(15+34+19)

#use 10-fold CV to choose the optimal number of leaves
sonar.cv = cv.tree(mytree, FUN = prune.misclass)
#for regression use prune.tree

sonar.cv

plot(sonar.cv)

#extracting the optimal number of leaves
min(sonar.cv$dev)
which(sonar.cv$dev == min(sonar.cv$dev))
sonar.cv$size[which(sonar.cv$dev == min(sonar.cv$dev))]

prune.sonar = prune.misclass(mytree, best = 6)
plot(prune.sonar)
?prune.misclass
text(prune.sonar, pretty = 0)


#changing parameters about when to split the predictor space
mytree = tree(V61~., data = sonar, control = tree.control(nobs = 208, mindev = 0, minsize = 2))
summary(mytree)

plot(mytree)
text(mytree, pretty=0)
