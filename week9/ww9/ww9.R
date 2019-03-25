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

#install.packages("e1071")
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
b/w[2]
-w[1]/w[2]

#
plot(x1,x2,col=my.col,pch=my.pch)
b = svmfit$rho #beta_0
w = colSums(svmfit$coefs[,1]*svmfit$SV) # beta_1, ... beta_p
abline(-.5,1)

1*0.4444444
.5*0.4444444
1/2.25

0.4444444 + 0.4444444 + 0.2222222
sqrt(1/2.25)
(.5*0.6666667)^2 + 2*(1*0.6666667)^2 
.5*0.6666667

.5/sqrt(2)
-0.3535534+2*0.7071068 -0.7071068
#0.3535534

#web work 9 Prob 2
bank = read.csv("bank-additional.csv", sep = ";")
attach(bank)
my.col = rep("red",4119)
my.col[which(y=="yes")] = "blue" 
my.pch = rep(21,4119)
my.pch[which(y=="yes")] = 4

plot(scale(emp.var.rate),scale(duration),col=my.col,pch=my.pch)
legend("topleft", legend = c("No", "Yes"), pch = c(21, 4), col = c("red","blue"))

library(e1071)
svmfit = svm(y~emp.var.rate+duration, data = bank, kernel = "linear", cost = 1)

summary(svmfit)
#confusion matrix
table(svmfit$fitted,y)
16/(16+3652)
417/(34+417)
table(bank$y)


library(e1071)
svmfit = svm(y~emp.var.rate+duration, data = bank, kernel = "linear", cost = 1)

w = colSums(svmfit$coefs[,1] * svmfit$SV)
b = svmfit$rho
# Optimal line
abline(b/w[2],-w[1]/w[2]) # y-int, slope
# Margin lines
abline((b-1)/w[2],-w[1]/w[2], lty=2) # y-int, slope; 
# as cost decreases, w[2] tends to decrease, resulting in a larger margin
abline((b+1)/w[2],-w[1]/w[2], lty=2) 
-w[1]/w[2]


#problem 5
set.seed(999)
tune.out = tune(svm, y~emp.var.rate + duration, data = bank, kernel = "radial", 
                ranges = list( cost = c(.001, .01, .1, 1, 5, 10, 100), 
                               gamma = c(0.5, 1, 2, 3, 4) ))

best.mod = tune.out$best.model
summary(best.mod)

summary(tune.out)
summary(tune.out)$best.performance

newClient = data.frame(emp.var.rate = 1, duration = 250)
predict(best.mod, newdata = newClient)


#problem 6
xgrid = cbind(rep(seq(-4, 2, length=1000), 1000), 
              rep(seq(0, 4000, length=1000), each=1000) )

#problem 7
func = predict(best.mod, xgrid, decision.values = TRUE)

func = attributes(func)$decision
#draw the curve f(x)=0
contour(seq(-4,2,length=1000), seq(0,4000,length=1000),
        matrix(func,1000,1000), level = 0, add=T, lwd=2)
