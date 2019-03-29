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

#p4
library(ISLR)
data("Default")
summary(Default)
Default$student01=0
Default$student01[which(Default$student=="Yes")] = 1
Default=Default[,-2]

library(nnet)
set.seed(4)
fit = nnet(default~., data=Default, size = 1)
summary(fit$fitted.values)

#p5
defaultcalc <- function(b,w,x) {
  1/(1+exp(-(b+w*x)))
}
defaultcalc(.01,.02,10000)
defaultcalc(.01,.02,100000)

Default.std = data.frame(default = Default[ ,1],scale(Default[ ,2:4]))

fit = nnet(default~., data=Default.std, size = 1, maxit = 200)

#p6
#install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(fit)
summary(fit)
fit$wts

text(-.2, .9, round(fit$wts[1], 2))
text(-.6, .8, round(fit$wts[2], 2))
text(-.6, .55, round(fit$wts[3], 2))
text(-.6, .33, round(fit$wts[4], 2))
text(.6, .9, round(fit$wts[5], 2))
text(.15, .55, round(fit$wts[6], 2))

axis(1)
axis(2)

#p7
fit$fitted.values[28]

zH1 = fit$wts[1] + sum(fit$wts[2:4] * Default.std[28, 2:4])
zH1 = 0.007841263
sigmaH1 = 1/(1+exp(-zH1))
sigmaH1
zOut = fit$wts[5] + sigmaH1 * fit$wts[6]
zOut = 13.68399 + sigmaH1 * -32.10247
zOut
1/(1+exp(-zOut))
