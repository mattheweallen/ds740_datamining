#ds740 spring 2019 final project by Matt Allen

#data used for this project can be downloaded from google drive at the link below.
#https://drive.google.com/open?id=1T5SGdP_quLwlcIQQ8yKmhwkFLBsfYl3L

#data file was created using google bigquery, then exported to google drive.
#data includes all citi bike trips from June 2016.
#one month alone is 189 MB. an entire year's worth was over a gigabyte.
#processing that much data is beyond the scope of the current study, so just one month was used.

#read in the csv file to a data frame
biketripsJune2016 = read.csv("C:/Users/matt/Documents/201606NYCCitiBike/bq-results-20190429-085840-1bny7q8gd48j.csv")

#remove any rows that have na values
biketripsJune2016 = na.omit(biketripsJune2016)

#check the summary to see if the data makes sense
summary(biketripsJune2016)

#tripduration has a max of 2167999.0 seconds. that is not correct and may indicate a stolen bike.
#citi bikes are typically used for shorter trips. we will limit the data to trips under a half hour or 1800 seconds.
max_bike_trip_duration = 1800
biketripsJune2016 = biketripsJune2016[which(biketripsJune2016$tripduration<max_bike_trip_duration),]

#there is a birth year of 1885 in the data that does not make sense either.
#remove all rows where birth year is less that 1916, which would make the rider over 100 years of age.
min_birth_year = 1916
biketripsJune2016 = biketripsJune2016[which(biketripsJune2016$birth_year > min_birth_year),]

#make a column based on birth year to be age, where age is data_year - birth_year.
data_year = 2016
biketripsJune2016$age = data_year - biketripsJune2016$birth_year 

#make a column based on startime that is the time hour of the day that the trip started.
biketripsJune2016$start_hour = as.numeric(substr(biketripsJune2016$starttime, 12, 13))

#make column for trip duration in minutes to make it easier to interpret.
biketripsJune2016$tripduration_minutes = biketripsJune2016$tripduration/60 


#males are represented about 3 times more than females in the data, so we will down sample males
#https://www.rdocumentation.org/packages/caret/versions/6.0-83/topics/downSample
library("caret")
set.seed(3)
biketripsJune2016 = downSample(biketripsJune2016,biketripsJune2016$gender)

#the response in this study is gender.
#the features that will be used to predict gender are tripduration_minutes, age, and start_hour
columns_to_keep = c("tripduration_minutes","age","start_hour","gender")
biketripsJune2016 = biketripsJune2016[,columns_to_keep]

#check for any skewness in the remaining features.
hist(biketripsJune2016$tripduration_minutes)
hist(biketripsJune2016$age)
hist(biketripsJune2016$start_hour)

#they tripduration_minutes exihibits a slight right skewness, but I think it is ok.
#do initial logistic regression fit on all the data
model = gender~tripduration_minutes+age+start_hour
fit = glm(model, data=biketripsJune2016, family = "binomial")
summary(fit)
#all variables are significant
#check for multicollinearity
library(car)
vif(fit)
#all variables are less than 10, so we can conclude multicollinearity does not exist

#cross validation of logistic regression
n = dim(biketripsJune2016)[1] #number of observations
k = 10 #10 fold cross validation
groups = c(rep(1:k,floor(n/k)), 1:(n-floor(n/k)*k))
set.seed(3) # ensures replicable results
cvgroups = sample(groups, n) #places observations in random groups
predictvals = rep(-1,n) #makes trouble shooting easier

for(i in 1:k) {
  groupi = (cvgroups == i) #all observations in group i
  fit = glm(model, data=biketripsJune2016[!groupi,], family = "binomial")
  predictvals[groupi] = predict(fit, biketripsJune2016[groupi,], type = "response") #response indicates to give predicted probability vs logit or log odds
}
confusion.matrix = table(predictvals > .5, biketripsJune2016$gender)
sensitivity = confusion.matrix[2,2]/(confusion.matrix[2,2] + confusion.matrix[1,2])
sensitivity #true positive rate
#0.5844636

specificity = confusion.matrix[1,1]/(confusion.matrix[1,1] + confusion.matrix[2,1])
specificity #true negative rate
#0.5237246


#create an ROC curve
#install.packages("pROC")
library(pROC)
myroc = roc(response=biketripsJune2016$gender, predictor = predictvals)
plot.roc(myroc)
myroc$auc
#the Area under the curve is 0.5717, which is not much better that random guessing.

#let's try a simplified model that just uses tripduration_minutes and lda
require(MASS)
ldafit = lda(biketripsJune2016$gender~biketripsJune2016$tripduration_minutes)
ldafit

#analyze the error rate of lda with confusion matrix
fittedclass = predict(ldafit,data=biketripsJune2016)$class
confusion.matrix = table(biketripsJune2016$gender,fittedclass)
confusion.matrix

sensitivity = confusion.matrix[2,2]/(confusion.matrix[2,2] + confusion.matrix[1,2])
sensitivity
#0.5348455

specificity = confusion.matrix[1,1]/(confusion.matrix[1,1] + confusion.matrix[2,1])
specificity
#0.5488671

overallerror_lda = (confusion.matrix[1,2] + confusion.matrix[2,1])/sum(confusion.matrix)
overallerror_lda
#0.459318

myroc = roc(response=biketripsJune2016$gender, predictor = biketripsJune2016$tripduration_minutes)
myroc$auc
#Area under the curve: 0.5594
plot(myroc)

#multiple predictors in lda
ldafit = lda(model, data=biketripsJune2016)
ldafit

fittedclass = predict(ldafit,data=biketripsJune2016)$class
table(biketripsJune2016$gender,fittedclass)
diag(table(biketripsJune2016$gender,fittedclass))

n = dim(biketripsJune2016)[1]
ErrorLDA = sum(biketripsJune2016$gender != fittedclass)/n
ErrorLDA
#0.4459437

#do 10-fold cross validation on two lda models
allpredictedCV1 = allpredictedCV2 = factor(rep(NA,n),levels=c("female","male"))
cvk = 10

for (i in 1:cvk)  {
  ldafit1 = lda(gender~biketripsJune2016$tripduration_minutes, data=biketripsJune2016, subset=(cvgroups!=i))
  newdata1 = data.frame(biketripsJune2016[cvgroups==i,c(1)])
  allpredictedCV1[cvgroups==i] = predict(ldafit1,newdata1)$class
  
  ldafit2 = lda(model, data=biketripsJune2016, subset=(cvgroups!=i))
  newdata2 = data.frame(biketripsJune2016[cvgroups==i,c(1,2,3)])
  allpredictedCV2[cvgroups==i] = predict(ldafit2,newdata2)$class
}
CVmodel1 = sum(allpredictedCV1!= biketripsJune2016$gender)/n; CVmodel1 #0.5059761
CVmodel2 = sum(allpredictedCV2!= biketripsJune2016$gender)/n; CVmodel2 #0.4459239
