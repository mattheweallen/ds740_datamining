set.seed(20)
x = seq(from = 0, to = 2*pi, by=.01)
y = sin(x)
library(nnet)
fit0 = nnet(y~x, size = 0, linout = T, skip = T)


matplot(cbind(x,x), cbind(y, fit0$fitted.values), type = "l")
legend("topright", legend = c("y = sin(x)", "Estimated"), lty=c(1,2), col=c("black","red"))


# do NOT reset the seed after running Problem 1
fit1 = nnet(y~x, size = 1, linout=T)
fit2 = nnet(y~x, size = 2, linout=T)
matplot(cbind(x,x,x), cbind(y, fit1$fitted.values, fit2$fitted.values), type="l", lwd=2, col=c("black", "blue", "red"))
legend("topright", legend = c("y = sin(x)", "1 Hidden Node", "2 Hidden Nodes"), lty=c(1,2,3), col=c("black", "blue", "red"))

fit0
fit1$convergence
fit2$convergence

fit2 = nnet(y~x, size = 2, linout=T, maxit = 500)
set.seed(16)
fit2 = nnet(y~x, size = 2, linout=T, maxit = 500)
