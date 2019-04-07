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
