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

#islr lecture

#problem 7
K=5; p=2; K+K*p+p*(p+1)/2

K=5; p=5; K+K*p+p*(p+1)/2


n=303
m = 10
groups = c(rep(1:m,floor(n/m)),1:(n%%m))
set.seed(4)
cvgroups = sample(groups,n)

allpredictedCV1 = allpredictedCV2 = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:m)  {
  ldafit1 = lda(DiseaseStatus ~ MaxHeartRate + STdepress, data=heart, subset=(cvgroups!=i))
  newdata1 = heart[cvgroups==i,c(4,5)]
  allpredictedCV1[cvgroups==i] = predict(ldafit1,newdata1)$class
  ldafit2 = lda(DiseaseStatus~., data=heart, subset=(cvgroups!=i))
  newdata2 = data.frame(heart[cvgroups==i,-6])
  allpredictedCV2[cvgroups==i] = predict(ldafit2,newdata2)$class
}
CVmodel1 = sum(allpredictedCV1!= heart$DiseaseStatus)/n; CVmodel1
CVmodel2 = sum(allpredictedCV2!= heart$DiseaseStatus)/n; CVmodel2


#problem 8
qdafit3 = qda(DiseaseStatus ~ MaxHeartRate + STdepress, data=heart)
qdafit3
for (i in 0:4) print(sd(heart$STdepress[heart$DiseaseStatus==i]))


fittedclassqda3 = predict(qdafit3,data=heart)$class
table(heart$DiseaseStatus,fittedclassqda3)
diag(table(heart$DiseaseStatus,fittedclassqda3))

newdata = data.frame(ChestPain =factor(4), MaxHeartRate =130, STdepress=2)
predict(qdafit3,newdata)$class

qdafit4 = qda(DiseaseStatus~., data=heart); qdafit4
fittedclassqda4 = predict(qdafit4,data=heart)$class
sum(diag(table(heart$DiseaseStatus,fittedclassqda3)))
sum(diag(table(heart$DiseaseStatus,fittedclassqda4)))

#problem 9
K=5; p=2; K+K*p+K*p*(p+1)/2
K=5; p=5; K+K*p+K*p*(p+1)/2


allpredictedCV3 = allpredictedCV4 = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:m)  {
  qdafit3 = qda(DiseaseStatus ~ MaxHeartRate + STdepress, data=heart, subset=(cvgroups!=i))
  newdata3 = heart[cvgroups==i,c(4,5)]
  allpredictedCV3[cvgroups==i] = predict(qdafit3,newdata3)$class
  qdafit4 = qda(DiseaseStatus~., data=heart, subset=(cvgroups!=i))
  newdata4 = data.frame(heart[cvgroups==i,-6])
  allpredictedCV4[cvgroups==i] = predict(qdafit4,newdata4)$class
}
CVmodel3 = sum(allpredictedCV3!= heart$DiseaseStatus)/n; CVmodel3
CVmodel4 = sum(allpredictedCV4!= heart$DiseaseStatus)/n; CVmodel4
