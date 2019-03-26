#For more about backpropagation, see University of Cambridge Engineering Part IIB & EIST Part II.
#http://mi.eng.cam.ac.uk/~mjfg/local/I10/i10_hand4.pdf
#R code: (Not that useful for neural networks, but handy for making pretty plots)
install.packages("plot3D")
library(plot3D)
M <- mesh(seq(-2, 2, length.out = 80),
          seq(-2, 2, length.out = 80))
z <- -(M$x^2 + M$y^2)
surf3D(M$x, M$y, z, colvar = z, colkey = FALSE, box = FALSE)

#neural networks in R
library(nnet)
set.seed(100)
fit = nnet(Species ~ Petal.Length + Sepal.Length, 
           data = iris, size = 1, maxit = 200)
summary(fit)
install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(fit)

fit$fitted.values

maxSpecies = apply(fit$fitted.values, 1, which.max)
maxProb = apply(fit$fitted.values, 1, max)

highProb = which(maxProb > .9)

predSpecies = rep(NA, 150)
predSpecies[highProb] = maxSpecies[highProb]
table(predSpecies, iris$Species)

length(which(is.na(predSpecies)))

