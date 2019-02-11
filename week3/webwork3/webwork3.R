badRegression = read.csv("BadRegression.csv")
#fit = glm(default ~ student + balance, data=Default, family = "binomial")

#ROC Curve
#install.packages("pROC")
library(pROC)
myroc = roc(response=badRegression$y, predictor = badRegression$predictvals)
plot.roc(myroc)
summary(myroc)
myroc$auc
boxplot(badRegression$predictvals~badRegression$y)


myroc = roc(response=badRegression$y, predictor=badRegression$predictvals, direction="<")
plot.roc(myroc)
summary(myroc)
myroc$auc


heart = read.csv("Heart_disease_Cleveland.csv")
summary(heart)

heart$Sex = as.factor(heart$Sex)
heart$ChestPain = as.factor(heart$ChestPain)
heart$HighBloodSugar = as.factor(heart$HighBloodSugar)
heart$ECG = as.factor(heart$ECG)
heart$ExerAngina = as.factor(heart$ExerAngina)
heart$Slope = as.factor(heart$Slope)
heart$Thal = as.factor(heart$Thal)

#new variable
heart$HD  <- ifelse(heart$DiseaseStatus >0, 1, 0)

fit = glm(HD~., data = heart, family = "binomial")

fit = glm(HD~ . - DiseaseStatus, data = heart, family = "binomial")

fit = glm(STdepress~ . -HD, data = heart)
par(mfrow = c(2, 2))
plot(fit) # where fit is what you called the regression model

hist(heart$STdepress)
boxplot(heart$STdepress)

fit2 = lm(log(STdepress+1) ~ .-HD, data = heart)
par(mfrow = c(2, 2))
plot(fit2) # where fit is what you called the regression model

AIC(fit)
AIC(fit2)
