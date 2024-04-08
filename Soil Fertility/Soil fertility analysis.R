library(sjPlot)
library(readxl)
library(emmeans)
library(multcomp)
library(ggpubr)
library(multcompView)

#Import data
Soildata = read_xlsx("SoilDataComplete.xlsx")
UK = subset(Soildata, Site == "UK")
KS = subset(Soildata, Site == "KS")
Year0 = subset(Soildata, Year == 0)
Year3 = subset(Soildata, Year == 3)

UKYear3 = subset(Soildata, Year == 3 & Site == "UK")
KSYear3 = subset(Soildata, Year == 3 & Site == "KS")
Soilsummary0 = aggregate(Year0[, 5:12], list(Year0$Site), mean)
Soilsummary3 = aggregate(Year3[, 5:12], list(Year3$Site), mean)
Soilsummary0
Soilsummary3



SoilsummaryUK = aggregate(UK[, 5:12], list(UK$Year), mean)
SoilsummaryUK

SoilsummaryKS = aggregate(KS[, 5:12], list(KS$Year), mean)
SoilsummaryKS



one.waysoil.pH = aov(soil.pH ~ Site, data = Soildata)
summary(one.waysoil.pH)
one.waysoil.water.pH = aov(soil.water.pH ~ Site, data = Soildata)
summary(one.waysoil.water.pH)
summary(one.wayP)
#Anova to compare K between sites
one.wayK = aov(K..kg.ha. ~ Site, data = Soildata)
summary(one.wayK)


one.wayCa = aov(Ca..kg.ha. ~ Site, data = Soildata)
summary(one.wayCa)

one.wayBuffer.pH = aov(Buffer.pH ~ Site, data = Soildata)
summary(one.wayBuffer.pH)


#Anova to compare P between sites
one.wayP = aov(` (kg/ha)` ~ `Cash Crop`, data = UKYear3)
summary(one.wayP)
one.wayMg = aov(Mg..kg.ha. ~ Site, data = Soildata)
summary(one.wayMg)

one.wayZn = aov(Zn..kg.ha. ~ Site, data = Soildata)
summary(one.wayZn)


subset0to10 = subset(Soildata, `Soil Depth (cm)` == "0 to 10")
subset10to20 = subset(Soildata, `Soil Depth (cm)` == "10to20")

one.wayP = aov(`Zn (kg/ha)` ~ `Site`, data = subset0to10)
summary(one.wayP)


