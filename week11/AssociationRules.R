#measuring the quality of a rule A -> rule B
#support: P(A and B)
#confidence: P(A and B)/P(A) = P(B|A)
#lift: P(B|A)/P(B)

#     READ?
# TV?       YES   NO
#     YES   527   570
#     NO    113   336

#P(TV and READ) = 527/(527+570+113+336)
#P(TV and READ)/P(TV) = P(READ | TV)  = 0.3408797/ ((527+570)/(527+570+113+336)) = 0.4804011
#P(READ | TV)/P(READ) =  0.4804011/((527+113)/(527+570+113+336)) 1.16



rules = apriori(Groceries, parameter = list(support = .001, confidence = 0.5))
subrules = head(rules, n = 10, by = "lift")
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="grouped")


#start web work
#p1
ames = read.csv("AmesSimple.csv")
attach(ames)

#There are many possibilities; here is one:
summary(Lot.Shape)
#Because there are no NA’s, we can start by setting all of the values equal to “Reg”, and then change the values that should be irregular:
Lot.Shape.bin = rep( "Reg", length(Lot.Shape) )
Lot.Shape.bin[ which(Lot.Shape %in% c("IR1", "IR2", "IR3")) ] = "Ir"

#p2
#install.packages("arules")
library(arules)
Bedroom.AbvGr.disc=discretize(Bedroom.AbvGr, method="interval",breaks=2, ordered=T)
summary(Bedroom.AbvGr.disc)
Full.Bath.disc = discretize(Full.Bath, method = "interval", breaks=2, ordered=T)
summary(Full.Bath.disc)

#p3
Gr.Liv.Area.disc = discretize(Gr.Liv.Area, method = "interval", breaks=3, ordered=T)
summary(Gr.Liv.Area.disc)

#p4
itemFrequency(Gr.Liv.Area.disc)
5/(315+2610+5)
hist(Gr.Liv.Area)

summary(Gr.Liv.Area)
Gr.Liv.Area.disc = discretize(Gr.Liv.Area, method = "fixed", breaks=c(334, 1126, 1743, 5642), ordered=T)
summary(Gr.Liv.Area.disc)
hist(SalePrice)
summary(SalePrice)
SalePrice.disc = discretize(SalePrice, "fixed", breaks=c(12789, 129500, 213500, 755000), ordered=T)
summary(SalePrice.disc)



#p5
colnames(ames)
Lot.Area = as.factor(Lot.Area)
Lot.Shape.bin = as.factor(Lot.Shape.bin)
Total.Bsmt.SF = as.factor(Total.Bsmt.SF)
has.Fireplace = as.factor(has.Fireplace)
Bldg.Type.simple = as.factor(Bldg.Type.simple)
Year.Built = as.factor(Year.Built)
Full.Bath = as.factor(Full.Bath)
Bedroom.AbvGr = as.factor(Bedroom.AbvGr)
Garage.Area = as.factor(Garage.Area)
has.Pool = as.factor(has.Pool)
ames.df = data.frame(Lot.Area, Lot.Shape.bin, Total.Bsmt.SF, Gr.Liv.Area.disc, has.Fireplace, SalePrice.disc, Bldg.Type.simple, Year.Built, Full.Bath, Bedroom.AbvGr, Garage.Area, has.Pool)

ames.trans = as(ames.df, "transactions")

#p6
rules = apriori(ames.trans, parameter = list(support = .05, confidence = 0.5))
summary(rules)
inspect(head(rules, n = 5, by = "lift"))

#p7
rules2 = apriori(ames.trans, parameter = list(support = .05, confidence = 0.5), 
                 appearance = list(rhs = c("SalePrice.disc=[2.14e+05,7.55e+05]"), default = "lhs") )
nonRedundant = which(interestMeasure(rules2, measure = "improvement", transactions = NULL, reuse = TRUE, quality_measure = "confidence") >= 0)
rules3 = rules2[nonRedundant]
summary(rules3)

#p8
rules4 = subset( rules3, subset = lhs %in% c("Bedroom.AbvGr=3", "Bedroom.AbvGr=4") )
summary(rules4)
inspect(head(rules4, n = 5, by = "lift"))

highLift = subset(rules3, subset = lift > 3.5 & confidence > .95)
summary(highLift)

#p9
mylhs = lhs(rules3)
singleAnt = which( size(mylhs) == 1 )
inspect( rules3[singleAnt] )


RegLot = which(mylhs %in% c("Lot.Shape.bin=Reg"))
inspect( rules3[ RegLot ] )
mylhs.mat = as(mylhs, Class = "matrix")
hist( colSums(mylhs.mat) )
