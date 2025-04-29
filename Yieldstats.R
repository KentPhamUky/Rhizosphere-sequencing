library(readxl)
library(ggplot2)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(carData)
library(plyr)
library(dplyr)
library(ggpubr)
library(car)
library(ggpattern)

data<-read_excel("Compiled Yields.xlsx")

data$Plot<- as.factor(data$Plot)
data$Rotation<- as.factor(data$Rotation)
data$Cash_Crop<- as.factor(data$Cash_Crop)
data$Year<- as.factor(data$Year)
data$Sample_ID<- as.factor(data$Sample_ID)

data$Vegetative_transform<-log(data$Vegetative)
shapiro.test(data$Marketable)
data$Marketable_transform<-sqrt(data$Marketable)
shapiro.test(data$Marketable_transform)

Year1 <- subset(data, data$Year == "2021")
Year2 <- subset(data, data$Year == "2022")
Year3 <- subset(data, data$Year == "2023")
####Vegetative####

Vegetative <- lme(fixed=Vegetative_transform ~ Year*Rotation,
                  data = data,
                  random = ~1|Plot,
                  na.action = na.omit)

anova(Vegetative)
Vegetativemeans <-emmeans(Vegetative, ~ (Year*Rotation))
multcomp::cld(Vegetativemeans, Letters=LETTERS)

Vegetative21 <- lme(fixed=Vegetative_transform ~ Rotation,
                    data = Year1,
                    random = ~1|Plot,
                    na.action = na.omit)
Vegetativemeans21 <-emmeans(Vegetative21, ~ (Rotation))
Veg21=multcomp::cld(Vegetativemeans21, Letters=LETTERS)
Veg21$Crop= c("Soybean", "Soybean", "Fiber", "Grain", "Corn", "Corn")

ggplot(Veg21, aes(fill = Crop, x = Rotation, y = emmean))+
  theme_classic()+
  geom_bar(stat="identity", width = 0.6, position = "dodge", col = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + SE), width = 0.3, position = position_dodge(0.6))+
  guides(fill = guide_legend("Crop"))+
  xlab("Treatment")+ylab("Vegetative Biomass (ln adjusted)")+
  ggtitle("2021 Vegetative Biomass")+
  geom_text(aes(label=.group), vjust = -0.9, hjust= .65, size = 5)


Vegetative22 <- lme(fixed=Vegetative_transform ~ Rotation,
                    data = Year2,
                    random = ~1|Plot,
                    na.action = na.omit)

anova(Vegetative22)
Vegetativemeans22 <-emmeans(Vegetative22, ~ (Rotation))
Veg22=multcomp::cld(Vegetativemeans22, Letters=LETTERS)
Veg22$Crop= c("Grain", "Soybean", "Soybean", "Fiber", "Corn", "Corn")
ggplot(Veg22, aes(fill = Crop, x = Rotation, y = emmean))+
  theme_classic()+
  geom_bar(stat="identity", width = 0.6, position = "dodge", col = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + SE), width = 0.3, position = position_dodge(0.6))+
  guides(fill = guide_legend("Crop"))+
  xlab("Treatment")+ylab("Vegetative Biomass (ln adjusted)")+
  ggtitle("2022 Vegetative Biomass")+
  geom_text(aes(label=.group), vjust = -0.6, hjust= "center", size = 5)

Vegetative23 <- lme(fixed=Vegetative_transform ~ Rotation,
                    data = Year3,
                    random = ~1|Plot,
                    na.action = na.omit)

anova(Vegetative23)
Vegetativemeans23 <-emmeans(Vegetative23, ~ (Rotation))
Veg23 = multcomp::cld(Vegetativemeans23, Letters=LETTERS)
Veg23$Crop= c("Grain", "Soybean", "Soybean", "Fiber", "Corn", "Corn")
ggplot(Veg23, aes(fill = Crop, x = Rotation, y = emmean))+
  theme_classic()+
  geom_bar(stat="identity", width = 0.6, position = "dodge", col = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + SE), width = 0.3, position = position_dodge(0.6))+
  guides(fill = guide_legend("Crop"))+
  xlab("Treatment")+ylab("Vegetative Biomass (ln adjusted)")+
  ggtitle("2023 Vegetative Biomass")+
  geom_text(aes(label=.group), vjust = -0.6, hjust= "center", size = 5)


####end####

####Marketstats####
Market <- lm(Marketable_transform ~ Year*Cash_Crop,
                data = data,
                na.action = na.omit)
anova(Market)
Marketmeans <-emmeans(Market, ~ (Rotation))
Markettest=multcomp::cld(Marketmeans, Letters=LETTERS)
Markettest




Market21 <- lm(Marketable_transform ~ Treatment,
                    data = Year1,
                    na.action = na.omit)
anova(Market21)

Marketmeans21 <-emmeans(Market21, ~ (Rotation))
emm = emmeans(Market21, ~Treatment)
multcomp::cld(emm, Letters=LETTERS)
Market21=multcomp::cld(Marketmeans21, Letters=LETTERS)











Market22 <- lme(fixed=Marketable_transform ~ Rotation,
                data = Year2,
                random = ~1|Plot,
                na.action = na.omit)
Marketmeans22 <-emmeans(Market22, ~ (Rotation))
Market22=multcomp::cld(Marketmeans22, Letters=LETTERS)
Market22$Crop= c("Grain", "Soybean", "Soybean", "Fiber", "Corn", "Corn")

ggplot(Market22, aes(fill = Crop, x = Rotation, y = emmean))+
  theme_classic()+
  geom_bar(stat="identity", width = 0.6, position = "dodge", col = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + SE), width = 0.3, position = position_dodge(0.6))+
  guides(fill = guide_legend("Crop"))+
  xlab("Treatment")+ylab("Marketable Biomass (sqrt adjusted)")+
  ggtitle("2022 Marketable Biomass")+
  geom_text(aes(label=.group), vjust = -1.2, size = 5)

Market23 <- lme(fixed=Marketable_transform ~ Rotation,
                data = Year3,
                random = ~1|Plot,
                na.action = na.omit)
Marketmeans23 <-emmeans(Market23, ~ (Rotation))
Market23=multcomp::cld(Marketmeans23, Letters=LETTERS)

Market23$Crop= c("Grain", "Soybean", "Soybean", "Fiber", "Corn", "Corn")

ggplot(Market23, aes(fill = Crop, x = Rotation, y = emmean))+
  theme_classic()+
  geom_bar(stat="identity", width = 0.6, position = "dodge", col = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + SE), width = 0.3, position = position_dodge(0.6))+
  guides(fill = guide_legend("Crop"))+
  xlab("Treatment")+ylab("Marketable Biomass (sqrt adjusted)")+
  ggtitle("2023 Marketable Biomass")+
  geom_text(aes(label=.group), vjust = -1.2, size = 5)

####end####

####Graph####

sum2023 <- ddply(Year3, "Rotation", summarise,    #Make the summary data
              Market = mean(Marketable),
              Market_sd = sd(Marketable),
              Market_se = Market_sd/sqrt(5),
              Stover_sd = sd(Stover),
              Stover = mean(Stover),
              Stover_se = Stover_sd/sqrt(5),
              )


plotdata2021 = read_excel("Compyield2021.xlsx")
plotdata2021$Type <- factor(plotdata2021$Type, levels = c("Stover", "Marketable"))
ggplot(plotdata2021, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
  theme_classic()+
  geom_bar_pattern(
    stat = "identity",
    position = "stack",
    color = "black", 
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  scale_pattern_manual(values = c("Stover" = "none", "Marketable" = "stripe")) +
  geom_errorbar(aes(ymin = SE_Yield, ymax = SE_Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab(" Biomass Yield (kg/ha)")+
  ggtitle("2021 Biomass") +
  ylim(0, 30000)

plotdata2022 = read_excel("Compyield2022.xlsx")
plotdata2022$Type <- factor(plotdata2022$Type, levels = c("Stover", "Marketable"))
ggplot(plotdata2022, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
  theme_classic()+
  geom_bar_pattern(
    stat = "identity",
    position = "stack",
    color = "black", 
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  scale_pattern_manual(values = c("Stover" = "none", "Marketable" = "stripe")) +
  geom_errorbar(aes(ymin = SE_Yield, ymax = SE_Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  ggtitle("2022 Biomass") +
  ylim(0,30000)

plotdata2023 = read_excel("Compyield2023.xlsx")
plotdata2023$Type <- factor(plotdata2023$Type, levels = c("Stover", "Marketable"))
ggplot(plotdata2023, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
  theme_classic()+
  geom_bar_pattern(
    stat = "identity",
    position = "stack",
    color = "black", 
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B")) +
  scale_pattern_manual(values = c("Stover" = "none", "Marketable" = "stripe")) +
  geom_errorbar(aes(ymin = SE_Yield, ymax = SE_Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  ggtitle("2023 Biomass") +
  ylim(0,30000)
