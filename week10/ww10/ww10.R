x = seq(from = 0, to = 2*pi, by=.01)
y = sin(x)
library(nnet)
fit0 = nnet(y~x, size = 0, linout = T, skip = T)


matplot(cbind(x,x), cbind(y, fit0$fitted.values), type = "l")
legend("topright", legend = c("y = sin(x)", "Estimated"), lty=c(1,2), col=c("black","red"))
