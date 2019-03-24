#curved boundaries between categories 
n = 100
set.seed(516) 
x = matrix(runif(n*2, -1,1),nc=2)
y = rep("red", n)
y[which(.5-3*(x[,1])^2+x[,2]>0)] = "blue"

mypch = rep(4,n)
mypch[y=="blue"] = 2
plot(x, pch = mypch, col = y, las=1, cex.axis=1.2, xlab="x1", ylab = "x2")
curve(-.5+3*x^2, add=T)

#Radial Kernel
n = 100
set.seed(515) 
x = matrix(runif(n*2, -1,1),nc=2)
y = rep("red", n)
y[which(-.75+2*(x[,1])^2+(x[,2])^2>0)] = "blue"

mypch = rep(4,n)
mypch[y=="blue"] = 2
plot(x, pch = mypch, col = y, las=1, cex.axis=1.2, xlab="x1", ylab = "x2")

#support vector machines in R
mydat = data.frame(x=x,y=y)
svmfit = svm(y~.,data=mydat,kernel="radial",cost=10,gamma=0.5,type="C-classification")
svmfit

#cross validation, for tuning parameter
y=as.factor(y)
tune.out=tune(svm,y~x,kernel="radial",ranges=list(cost=c(.001,.01,.1,1,5,10,100),gamma=c(.5,1,2,3,4)), type="C-classification")
summary(tune.out$best.model)

#decision boundary is a curve
xgrid=cbind(rep(seq(-1,1,.05),41), rep(seq(-1,1,.05),each=41))
func=predict(svmfit,xgrid,decision.values = TRUE)
func=attributes(func)$decision
#draw the curve f(x)=0
contour(seq(-1,1,.05),seq(-1,1,.05),matrix(func,41,41),level=0,add=T,lwd=2)

#For a summary of the advantages of the radial kernel, see A Practical Guide to Support Vector Classification by Chih-Wei Hsu, Chih-Chung Chang, and Chih-Jen Lin (PDF)
#https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf

#For more about kernels in logistic regression, see The Elements of Statistical Learning by Hastie, Tibshirani, and Friedman.
#https://web.stanford.edu/~hastie/ElemStatLearn//
#For a summary of BRUTO, see "Flexible Discriminant Analysis by Optimal Scoring" by Trevor Hastie, Robert Tibshirani, and Andreas Buja (PDF).
#https://web.stanford.edu/~hastie/Papers/fda.pdf

