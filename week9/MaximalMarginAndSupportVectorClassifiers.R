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

