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

marketable <- lme(fixed=Marketable_transform ~ Year*Rotation+Cash_Crop,
                  data = data,
                  random = ~1|Plot,
                  na.action = na.omit)

anova(marketable)
Vegetativemeans <-emmeans(marketable, ~ (Rotation))
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
plot2021=ggplot(plotdata2021, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
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
  annotate("text", x = 1, y = 20400, label = "C")+
  annotate("text", x = 2, y = 10500, label = "B")+
  annotate("text", x = 3, y = 9400, label = "BC")+
  annotate("text", x = 4, y = 10800, label = "B")+
  annotate("text", x = 5, y = 7400, label = "A")+
  annotate("text", x = 6, y = 20300, label = "C")+
  ggtitle("2021 Biomass") +
  ylim(0, 32000)
plot2021
plotdata2022 = read_excel("Compyield2022.xlsx")
plotdata2022$Type <- factor(plotdata2022$Type, levels = c("Stover", "Marketable"))
plot2022=ggplot(plotdata2022, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
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
  annotate("text", x = 1, y = 10000, label = "B")+
  annotate("text", x = 2, y = 18800, label = "D")+
  annotate("text", x = 3, y = 9400, label = "B")+
  annotate("text", x = 4, y = 7200, label = "C")+
  annotate("text", x = 5, y = 17000, label = "D")+
  annotate("text", x = 6, y = 7000, label = "A")+
  ggtitle("2022 Biomass") +
  ylim(0,32000)
plot2022
plotdata2023 = read_excel("Compyield2023.xlsx")
plotdata2023$Type <- factor(plotdata2023$Type, levels = c("Stover", "Marketable"))
plot2023=ggplot(plotdata2023, aes(x = Rotation, y = Yield, fill = Crop, pattern = Type))+
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
  annotate("text", x = 1, y = 30200, label = "D")+
  annotate("text", x = 2, y = 9500, label = "B")+
  annotate("text", x = 3, y = 13000, label = "C")+
  annotate("text", x = 4, y = 9500, label = "B")+
  annotate("text", x = 5, y = 3300, label = "A")+
  annotate("text", x = 6, y = 28700, label = "D")+
  ggtitle("2023 Biomass") +
  ylim(0,30200)
plot2023

combine = ggarrange(plot2021, plot2022, plot2023,
                    common.legend= TRUE,
                    legend="right",
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)

combine
Year3 = subset(Year3, Treatment!= "HF1")
Year3$Stovertrans<-(Year3$Stover)^1/3
shapiro.test(Year3$Stover)
stover <- lme(fixed=Stover ~ Treatment,
                  data = Year3,
                  random = ~1|Plot,
                  na.action = na.omit)

anova(stover)
stovermeans <-emmeans(stover, ~ (Treatment))
multcomp::cld(stovermeans, Letters=LETTERS)


stover2021data = subset(plotdata2021, Type == "Stover")
stover2021 = ggplot(stover2021data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 10000, label = "B")+
  annotate("text", x = 2, y = 5500, label = "A")+
  annotate("text", x = 4, y = 5900, label = "A")+
  annotate("text", x = 5, y = 6500, label = "A")+
  annotate("text", x = 6, y = 10000, label = "B")+
  ggtitle("2021 Stover") +
  ylim(0,16000)
stover2021

stover2022data = subset(plotdata2022, Type == "Stover")
stover2022 = ggplot(stover2022data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 5500, label = "A")+
  annotate("text", x = 2, y = 9000, label = "B")+
  annotate("text", x = 3, y = 5500, label = "A")+
  annotate("text", x = 5, y = 8400, label = "B")+
  annotate("text", x = 6, y = 5500, label = "A")+
  ggtitle("2022 Stover") +
  ylim(0,16000)
stover2022

stover2023data = subset(plotdata2023, Type == "Stover")
stover2023 = ggplot(stover2023data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 15800, label = "B")+
  annotate("text", x = 2, y = 5000, label = "A")+
  annotate("text", x = 4, y = 5000, label = "A")+
  annotate("text", x = 5, y = 3300, label = "A")+
  annotate("text", x = 6, y = 14000, label = "B")+
  ggtitle("2023 Stover") +
  ylim(0,16000)
stover2023
  
combine = ggarrange(stover2021, stover2022, stover2023,
                    common.legend= TRUE,
                    legend="right",
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)

combine

Marketable2021data = subset(plotdata2021, Type == "Marketable")
Marketable2021 = ggplot(Marketable2021data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 11000, label = "C")+
  annotate("text", x = 2, y = 5700, label = "B")+
  annotate("text", x = 3, y = 9000, label = "BC")+
  annotate("text", x = 4, y = 5500, label = "B")+
  annotate("text", x = 5, y = 2000, label = "A")+
  annotate("text", x = 6, y = 11000, label = "C")+
  ggtitle("2021 Marketable") +
  ylim(0,16000)
Marketable2021

Marketable2022data = subset(plotdata2022, Type == "Marketable")
Marketable2022 = ggplot(Marketable2022data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 4900, label = "B")+
  annotate("text", x = 2, y = 10500, label = "D")+
  annotate("text", x = 3, y = 4700, label = "B")+
  annotate("text", x = 4, y = 7000, label = "C")+
  annotate("text", x = 5, y = 9300, label = "D")+
  annotate("text", x = 6, y = 2000, label = "A")+
  ggtitle("2022 Marketable") +
  ylim(0,16000)
Marketable2022

Marketable2023data = subset(plotdata2023, Type == "Marketable")
Marketable2023 = ggplot(Marketable2023data, aes(x=Rotation, y=Yield, fill = Crop )) +
  theme_classic() +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("orange", "#508578","#5F7FC7", "#AD6F3B"))+
  geom_errorbar(aes(ymin = Yield-SE, ymax = Yield + SE), width = 0.3)+
  xlab("Treatment")+ylab("Biomass Yield (kg/ha)")+
  annotate("text", x = 1, y = 16000, label = "D")+
  annotate("text", x = 2, y = 5300, label = "B")+
  annotate("text", x = 3, y = 13000, label = "C")+
  annotate("text", x = 4, y = 5300, label = "B")+
  annotate("text", x = 5, y = 1000, label = "A")+
  annotate("text", x = 6, y = 16000, label = "D")+
  ggtitle("2023 Marketable") +
  ylim(0,16000)
Marketable2023

combine = ggarrange(Marketable2021, Marketable2022, Marketable2023,
                    common.legend= TRUE,
                    legend="right",
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)

combine

