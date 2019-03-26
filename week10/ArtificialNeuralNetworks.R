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
#install.packages("NeuralNetTools")
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

#example regression problem
# CV to tune number of hidden nodes
n = dim(iris)[1]
k = 10 #using 10-fold cross-validation
sizes = 1:8 # number of hidden nodes
groups = rep(1:k,floor(n/k))

set.seed(100)
cvgroups = sample(groups,n) 
squaredError = matrix( , nr = n, nc = length(sizes) ) 
# squaredError[i, j] contains (y - yhat)^2 
# for observation i, size j

#example: cross-validation
for(i in 1:k){
  groupi = (cvgroups == i)
  for(j in 1:length(sizes)){
    fit = nnet(Sepal.Width ~ Petal.Length + Sepal.Length, 
               data = iris[!groupi,], size = sizes[j], maxit = 200,
               linout = T, trace = F)
    
    squaredError[groupi, j] = (iris$Sepal.Width[groupi] - predict(fit, iris[groupi,]) )^2
  } # end iteration over j
} # end iteration over i

MSE = apply(squaredError, 2, mean)
plot(sizes,MSE,type="l",lwd=2,las=1)

#variable importance
set.seed(100)
iris2 = data.frame(scale(iris[,1:4]))
fit = nnet(Sepal.Width~Petal.Length+Sepal.Length+Petal.Width,data=iris2,size=2,maxit=400,linout=T)
library(NeuralNetTools)
garson(fit)

#For more about Lek profiles, see Sensitivity analysis for neural networks - R is my friend
#https://beckmw.wordpress.com/2013/10/07/sensitivity-analysis-for-neural-networks/
#R code:
  
iris2 = data.frame( scale(iris[,1:4]) )
set.seed(100)
fit = nnet(Sepal.Width ~ Petal.Length + Sepal.Length + Petal.Width, 
           data = iris2, size = 2, maxit = 400, linout = T)
library(NeuralNetTools)
lekprofile(fit)
