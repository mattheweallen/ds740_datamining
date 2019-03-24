#create hyperplane
n = 20
set.seed(524) 
x = matrix(runif(n*2),nc=2)
y = rep(-1, n)
y[which(-.8-2*x[,1]+3*x[,2]>0)] = 1

mycol = rep("red", 20)
mycol[which(y==1)] = "blue"
plot(x, pch = 3-y, col = mycol, las=1, cex.axis=1.2, xlab="x1", ylab = "x2")
abline(coef=c(.8/3,2/3)) #hyperplane, -.8 - 2*x1 + 3*x2 = 0, under line is less than zero, above line is greater than zero

#support vector classifier for iris data
summary(iris)
my.iris = iris[which(iris$Species != "setosa"),]
attach(my.iris)

my.col = rep("red",100)
my.col[which(Species=="virginica")] = "blue" 
my.pch = rep(21,100)
my.pch[which(Species=="virginica")] = 4

plot(Sepal.Length,Petal.Length,col = my.col, pch = my.pch)
legend("topleft",legend=c("virginica","versicolor"),col=c("blue","red"),pch=c(4,21))

library(e1071)
svmfit = svm(Species~Sepal.Length+Petal.Length, kernel="linear", cost=1, type="C-classification")
summary(svmfit)
svmfit$SV
dim(svmfit$SV)

my.iris = data.frame(my.iris)
#built in plot function
plot(svmfit,data=my.iris,Petal.Length~Sepal.Length)
#perhaps better plot
plot(scale(Sepal.Length),scale(Petal.Length),col=my.col,pch=my.pch)
b = svmfit$rho #beta_0
w = colSums(svmfit$coefs[,1]*svmfit$SV) # beta_1, ... beta_p

#draw the lines
abline(b/w[2],-w[1]/w[2]) # support vector classifier
                                # y intercept, slope
abline((b+1)/w[2],-w[1]/w[2],lty=2) #margins
abline((b-1)/w[2],-w[1]/w[2],lty=2)

#hightlight the support vectors
points(svmfit$SV,cex=2)


#predicted classifications of training data
svmfit$fitted

#confusion matrix
table(svmfit$fitted,Species)
#fitted values are rows, true species are listed in columns

#see if can do better that 4+3 errors, by trying different values of cost parameter
#10-fold cross validation
tune.out = tune(svm,Species~Sepal.Length+Petal.Length, kernel="linear",type="C-classification",ranges=list(cost=c(.001,.01,.1,1,5,10,100)))
summary(tune.out)

my.best = tune.out$best.model
summary(my.best)

table(my.best$fitted, Species)

#in lecture "Technical details of the maximal margin classifier"
#For more information about solving this optimization problem using Lagrange multipliers, 
#see p. 420 of The Elements of Statistical Learning by Hastie, Tibshirani, and Friedman.

