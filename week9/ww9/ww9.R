#problem 1

#Obs. #	x1	x2	Y
#1	3	4	Red Circle
#2	2	2	Red Circle
#3	4	4	Red Circle
#4	1	4	Red Circle
#5	2	1	Blue Triangle
#6	4	3	Blue Triangle
#7	4	1	Blue Triangle

prob1 = data.frame(c(3,2,4,1,2,4,4),c(4,2,4,4,1,3,1),c("Red Circle","Red Circle","Red Circle","Red Circle","Blue Triangle","Blue Triangle","Blue Triangle"))
colnames(prob1) = c("x1","x2","Y")
summary(prob1)
class(prob1$Y)# = as.factor(prob1$Y)
attach(prob1)

library(e1071)
model = Y~x1+x2
svmfit = svm(model, kernel="linear", cost=1, type="C-classification")
summary(svmfit)
svmfit$SV
dim(svmfit$SV)


#built in plot function
plot(svmfit,data=prob1,x2~x1)
#perhaps better plot

my.col = rep("red",7)
my.col[which(Y=="Red Circle")] = "blue" 
my.pch = rep(21,7)
my.pch[which(Y=="Red Circle")] = 4

plot(scale(x1),scale(x2),col=my.col,pch=my.pch)
b = svmfit$rho #beta_0
w = colSums(svmfit$coefs[,1]*svmfit$SV) # beta_1, ... beta_p

#draw the lines
abline(b/w[2],-w[1]/w[2]) # support vector classifier
# y intercept, slope
abline((b+1)/w[2],-w[1]/w[2],lty=2) #margins
abline((b-1)/w[2],-w[1]/w[2],lty=2)
