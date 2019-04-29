#https://cloud.google.com/blog/products/gcp/google-cloud-platform-for-data-scientists-using-r-with-google-bigquery
#use google bigquery to get data
#install.packages("bigrquery")
#install.packages("readr")
#library("bigrquery")
#library("readr")
#https://db.rstudio.com/databases/big-query/
#https://console.cloud.google.com/home/dashboard?project=nyc-citibike-238810
#auth code: 4/OAFrOq6St9wo4ZSiYCL8C2m541Z9uRvza_JdXsS57OqJiezbSQCmyLc
#project <- "nyc-citibike-238810" # put your project ID here
#sql <- "select * FROM [bigquery-public-data.new_york.citibike_trips] LIMIT 5"
#df <- query_exec(sql, project = project)

#write out example csv file for data
#https://stackoverflow.com/questions/6299220/access-a-url-and-read-data-with-r

#data is stored here: https://drive.google.com/open?id=1vUKo8OyTMkaEH1hyrHtXKD81doXo1sVD


#biketrips2016 <- read.csv(url("https://drive.google.com/open?id=1vUKo8OyTMkaEH1hyrHtXKD81doXo1sVD"))
biketrips2016 <- read.csv("C:/Users/matt/Documents/201606NYCCitiBike/bq-results-20190429-085840-1bny7q8gd48j.csv")
hist(biketrips2016$birth_year)
#shapiro.test(biketrips2016$birth_year)
hist(biketrips2016$tripduration)

#biketrips2016$starttime =  as.Date(biketrips2016$starttime)
#summary(biketrips2016)
#https://stackoverflow.com/questions/19460608/want-only-the-time-portion-of-a-date-time-object-in-r

timestr = substr(biketrips2016$starttime, 12, 16)

#install.packages("lubridate")
library(lubridate)
attach(biketrips2016)
timefrommidnight = as.numeric(hm(timestr))
summary(timefrommidnight)
boxplot(biketrips2016$birth_year~biketrips2016$gender)
boxplot(timefrommidnight~biketrips2016$gender)
summary(biketrips2016)
boxplot(biketrips2016$tripduration~biketrips2016$gender)
hist(biketrips2016$tripduration)
length(which(tripduration>10000))
tripduration[which(tripduration<10000)]
boxplot(tripduration[which(tripduration<1800)]~gender[which(tripduration<1800)])
hist(log(tripduration[which(tripduration<1800)]))
boxplot(log(tripduration[which(tripduration<1800)])~gender[which(tripduration<1800)])

y=gender[which(tripduration<1800)]
x1=log(tripduration[which(tripduration<1800)])
x2=birth_year[which(tripduration<1800)]
x3=timefrommidnight[which(tripduration<1800)]
fit = glm(y~x1+x2,family = "binomial")
summary(fit)

boxplot(birth_year[which(tripduration<1800)]~gender[which(tripduration<1800)])

hist(log(x2))
#remove birth years that do not make sense



require(MASS)
ldafit = lda(y~x1)
#Question 4
ldafit

#lda was not used because there is not enough separation of the distribution.

#throw out values above threshold for trip time, and below threshold for year. May be questionable ages of individuals.
#do study around trips for shorter times.


#install.packages("randomForest")
#library(randomForest)
#hitters.bag = randomForest(y~x1, mtry=19,importance=T)

#install.packages("gbm")
#library(gbm)
#boost = gbm(y~x1, distribution = "gaussian", n.trees = 100, shrinkage = .001, interaction.depth = 4)
#summary(boost)
#boost

#plot(boost)






df = data.frame(y,x1,x2,x3)
df = na.omit(df)
library(tree)
summary( df)
mytree = tree(y~.,data=df) #, data = OJ[train,])
summary(mytree)


plot(mytree)
text(mytree, pretty=0)

females = which(y=="female")
males = which(y=="male")

library(nnet)
nextdf = df[c(males[1:100000],females[1:100000]),]
fit = nnet(y~., data = nextdf, size = 1)

test = df[c(males[100001:200000],females[100001:200000]),]

MMClass = predict(fit, data=test, type = "class")
confusion = table(predicted=MMClass, actual=test$y)
(confusion[1,2] + confusion[2,1])/sum(confusion)


#install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(fit)