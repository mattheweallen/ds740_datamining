#prob 1
myexp = exp(-60/72)*4
1/(1+myexp)
(1/(1+myexp))*myexp

#prob 2
10/36
log(.8)-(50/36)
x=-.009
x*0.2777778 - 1.612032
log(.2)
-1.609438

#prob 4
dividends = read.csv("Dividends.csv")
library(pROC)
myroc = roc(response=dividends$y, predictor = dividends$x)
plot.roc(myroc)
myroc$auc

library(MASS)  #help(lda)

ldafit1 = lda(dividends$y~dividends$x)

fittedclass1 = predict(ldafit1,data=dividends)$class
table(dividends$y,fittedclass1)
sensitivity = 762/(762+38)
sensitivity
specificity = 107/(93+107)
specificity

#prob 6
heart = read.csv("Heart_disease_Cleveland.csv")
ldafit1 = lda(heart$DiseaseStatus~heart$MaxHeartRate + heart$STdepress)
ldafit1
fittedclass0 = predict(ldafit1,data=heart)$class
table(heart$DiseaseStatus,fittedclass0)


ldafit2 = lda(heart$DiseaseStatus~heart$MaxHeartRate + heart$STdepress+heart$ChestPain)
ldafit2
#mydf = data.frame(colnames("MaxHeartRate", "STdepress", "ChestPain"))
MaxHeartRate = 130
STdepress = 2
ChestPain = 4
mydf = data.frame(MaxHeartRate, STdepress, ChestPain)
predict(ldafit2,data = mydf)$class
mydf

newdata = data.frame(ChestPain =factor(4), MaxHeartRate =130, STdepress=2)
predict(ldafit2,newdata)$class

#prob 6
input = read.csv("Heart_Disease_Cleveland.csv")  # Change Dir... to appropriate folder
names(input)
heart = input[,c(1,4,5,8,10,14)]


library(MASS)
ldafit1 = lda(DiseaseStatus ~ MaxHeartRate + STdepress, data=heart)
ldafit1

fittedclasslda1 = predict(ldafit1,data=heart)$class
table(heart$DiseaseStatus,fittedclasslda1)
diag(table(heart$DiseaseStatus,fittedclasslda1))


newdata = data.frame(ChestPain =factor(4), MaxHeartRate =130, STdepress=2)
predict(ldafit1,newdata)$class


ldafit2 = lda(DiseaseStatus~., data=heart)
ldafit2
fittedclasslda2 = predict(ldafit2,data=heart)$class
table(heart$DiseaseStatus,fittedclasslda2)
sum(diag(table(heart$DiseaseStatus,fittedclasslda1)))
sum(diag(table(heart$DiseaseStatus,fittedclasslda2)))


