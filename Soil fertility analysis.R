library(sjPlot)

setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-Sequencing")

#Import data
Soildata = read.csv(file = "C:/Users/kentp/Documents/Github/Rhizosphere-Sequencing/Soil Data.csv", fileEncoding="UTF-8-BOM")

Soilsummary = aggregate(Soildata[, 5:12], list(Soildata$Site), mean)
Soilsummary

tab_df(Soilsummary, file="Soilsummary.doc")



one.waysoil.pH = aov(soil.pH ~ Site, data = Soildata)
summary(one.waysoil.pH)
one.waysoil.water.pH = aov(soil.water.pH ~ Site, data = Soildata)
summary(one.waysoil.water.pH)
one.wayBuffer.pH = aov(Buffer.pH ~ Site, data = Soildata)
summary(one.wayBuffer.pH)


#Anova to compare P between sites
one.wayP = aov(P..kg.ha. ~ Site, data = Soildata)
summary(one.wayP)
#Anova to compare K between sites
one.wayK = aov(K..kg.ha. ~ Site, data = Soildata)
summary(one.wayK)

one.wayCa = aov(Ca..kg.ha. ~ Site, data = Soildata)
summary(one.wayCa)

one.wayMg = aov(Mg..kg.ha. ~ Site, data = Soildata)
summary(one.wayMg)

one.wayZn = aov(Zn..kg.ha. ~ Site, data = Soildata)
summary(one.wayZn)




