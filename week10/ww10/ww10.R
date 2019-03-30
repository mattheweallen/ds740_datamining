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

#p8
DefaultClass = predict(fit, Default.std, type = "class")
confusion = table(predicted=DefaultClass, actual=Default.std$default)
(confusion[1,2] + confusion[2,1])/sum(confusion)
confusion


DefaultClass.8 = rep(NA, length(Default.std$default))
DefaultClass.8[which(fit$fitted.values > .8)] = "Yes"
DefaultClass.8[which(fit$fitted.values < .2)] = "No"

confusion = table(predvals = DefaultClass.8, truth = Default.std$default)
(confusion[1,2] + confusion[2,1])/sum(confusion)

length(which(is.na(DefaultClass.8)))

#problem 9
garson(fit)

lekprofile(fit)

#p10
# CV to choose # of hidden nodes
n = dim(Default)[1]
k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)))
sizes = 1:8
misclassError = matrix( , nr = k, nc = length(sizes) )
conv = matrix(, nr = k, nc = length(sizes) ) 
set.seed(4)
cvgroups = sample(groups,n) 
for(i in 1:k){
  groupi = (cvgroups == i)
  myDefault.train = scale(Default[!groupi, 2:4])
  myDefault.valid = scale(Default[groupi, 2:4], center = attr(myDefault.train, "scaled:center"), 
                          scale = attr(myDefault.train, "scaled:scale"))
  myDefault.train = data.frame(default=Default[!groupi, 1], myDefault.train)
  myDefault.valid = data.frame(default=Default[groupi, 1], myDefault.valid)
  for(j in 1:length(sizes)){
    fit = nnet(default ~ ., data=myDefault.train, size = sizes[j], trace = F, maxit=1000) 
    predictions = predict(fit, myDefault.valid, type = "class")
    misclassError[i, j] = length(which(predictions != myDefault.valid[ , 1])) / length(predictions)
    conv[i, j] = fit$convergence
  } # end iteration over j
} # end iteration over i

#p12
colSums(conv)
misclassError
error = apply(misclassError, 2, mean)
plot(sizes, error, type = "l", lwd = 2, las = 1)
min(error)
which(error == min(error))
error

#p13
set.seed(4)
train = sample(1:10000, 8000, replace = F)
myDefault.train = scale(Default[train, 2:4])
myDefault.valid = scale(Default[-train, 2:4], center = attr(myDefault.train, "scaled:center"), 
                        scale = attr(myDefault.train, "scaled:scale"))
myDefault.train = data.frame(default=Default[train, 1], myDefault.train)
myDefault.valid = data.frame(default=Default[-train, 1], myDefault.valid)

fit = nnet(default ~ ., data=myDefault.train, size = 4, maxit=1000)
train.predict = predict(fit, myDefault.train, type = "class")
length(which(train.predict != myDefault.train[ , 1]))/length(train.predict)

valid.predict = predict(fit, myDefault.valid, type = "class")
length(which(valid.predict != myDefault.valid[ , 1]))/length(valid.predict)

#p14
fit2 = nnet(default ~ ., data=myDefault.train, size = 4, maxit=1000, decay = .5)
valid.predict = predict(fit2, myDefault.valid, type = "class")
length(which(valid.predict != myDefault.valid[ , 1]))/length(valid.predict)

max(abs(fit$wts))
max(abs(fit2$wts))
