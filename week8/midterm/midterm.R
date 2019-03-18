#https://www.mayoclinic.org/tests-procedures/hematocrit/about/pac-20384728
#https://www.rdocumentation.org/packages/alr4/versions/1.0.5/topics/ais
ais = read.csv("ais.csv")
summary(ais)

sum(ais$Sex[which(ais$Sex == 1)])
#class(ais$Sport)
#class(ais$Sex)
#ais$Sex = as.factor(ais$Sex)
ais$Sex <- factor(ais$Sex,levels = c(0,1),labels = c("Male", "Female"))


summary(ais$Sex)
#males = which(ais$Sex == 0)
#ais$Sex = "Female"  
#ais$Sex[males] = "Male"
summary(ais$Sport)
names(ais)
#k nearest neighbors sport

table(ais$Sport) #response
table(ais$Sex) 

#plot histograms of variable to check for normality
hist(ais$Bfat) #possible response variable looks skewed

hist(ais$Ht)
hist(ais$Wt)
hist(ais$LBM)
hist(ais$RCC)
hist(ais$WCC)
hist(ais$Hc)
hist(ais$Hg)
hist(ais$Ferr)
hist(ais$BMI)
hist(ais$SSF)


