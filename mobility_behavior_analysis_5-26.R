# Mobility Behavior Data Analysis
# Author: Leigh Ann Ganzar
# Last modified: 6-8-2021

# --------------------------------------------------------------------------

# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(oddsratio)
library(irr)
library(miceadds)
library(sandwich)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(regclass)

# set working directory
setwd("/Users/leighannganzar/Desktop/Post-Doc/Projects/Mobility Behavior")

# --------------------------------------------------------------------------

# INTER RATER RELIABILTY
# import dataset for IRR 
irr_df <- read_csv("IRR_Mobility_Behavior_052621.csv")

# create dataframes for each pair of variables
Sex_irr <- irr_df %>% 
  select(Sex_kl, Sex_kb)
Age_irr <- irr_df %>% 
  select(Age_kl, Age_kb)
Group_irr <- irr_df %>% 
  select(Group_kl, Group_kb)
Traveler_irr <- irr_df %>% 
  select(Traveler_kl, Traveler_kb)
Sidewalk_irr <- irr_df %>% 
  select(Sidewalk_kl, Sidewalk_kb)
Bike_Lane_With_irr <- irr_df %>% 
  select(Bike_Lane_With_kl, Bike_Lane_With_kb)
Bike_Lane_Against_irr <- irr_df %>% 
  select(Bike_Lane_Against_kl, Bike_Lane_Against_kb)
Street_With_Traffic_irr <- irr_df %>% 
  select(Street_With_Traffic_kl, Street_With_Traffic_kb)
Street_Against_Traffic_irr <- irr_df %>% 
  select(Street_Against_Traffic_kl, Street_Against_Traffic_kb)

# cohen's kappa calculations
kappa2(Sex_irr)
# kappa = 0.934
kappa2(Age_irr)
# kappa = 0.951
kappa2(Group_irr)
# kappa = 0.966
kappa2(Traveler_irr)
# kappa = 0.972
kappa2(Sidewalk_irr)
# kappa = 0.939
kappa2(Bike_Lane_With_irr)
# kappa = 0.931
kappa2(Bike_Lane_Against_irr)
# kappa = 0.892
kappa2(Street_With_Traffic_irr)
# kappa = 0.89
kappa2(Street_Against_Traffic_irr)
# kappa = 0.958

# range across 9 items = 0.89 - 0.966

# average across 9 items
irr_average <- c(0.934, 0.951, 0.966, 0.972, 0.939, 0.931, 0.892, 0.89, 0.958)
summary(irr_average)
sd(irr_average)
# mean = 0.937 , SD = 0.03

# --------------------------------------------------------------------------

# MAIN AIM ANALYSES
# import dataset for analyses
mobility_prelim <- read_csv("Data_Mobility_Behavior_052621.csv")
weekend <- read_csv("Data_Mobility_Behavior_Weekend.csv")
mobilitydf <- merge(weekend, mobility_prelim, by = "Row_ID")

# create factor variables
mobilitydf$Site <- as_factor(mobilitydf$Site)
mobilitydf$Site = relevel(mobilitydf$Site, ref="Rio Grande St")
mobilitydf$Time <- as_factor(mobilitydf$Time)
mobilitydf$Time = relevel(mobilitydf$Age, ref="Noon")
mobilitydf$Weekend <- as_factor(mobilitydf$Weekend)
mobilitydf$Weekend <- fct_recode(mobilitydf$Weekend,
                                 Weekend = "1",
                                 Weekday = "0")
mobilitydf$Site <- as_factor(mobilitydf$Site)
mobilitydf$Sex <- as_factor(mobilitydf$Sex)
mobilitydf$Sex <- fct_recode(mobilitydf$Sex, 
                             Male =  "m", 
                             Female = "f")
mobilitydf$Sex = relevel(mobilitydf$Sex, ref="Male")
mobilitydf$Age <- as_factor(mobilitydf$Age)
mobilitydf$Age <- fct_recode(mobilitydf$Age, 
                             Adult =  "a", 
                             Youth = "y", 
                             "Older Adult" = "oa")
mobilitydf$Age = relevel(mobilitydf$Age, ref="Adult")
mobilitydf$Age = factor(mobilitydf$Age, levels = c("Adult", "Youth", "Older Adult"))
mobilitydf$Group <- as_factor(mobilitydf$Group)
mobilitydf$Group <- fct_recode(mobilitydf$Group, 
                               "Traveling in Group" =  "y", 
                               "Traveling Alone" = "n")
mobilitydf$Group = relevel(mobilitydf$Group, ref="Traveling Alone")
mobilitydf$Traveler <- as_factor(mobilitydf$Traveler)
mobilitydf$Traveler <- fct_recode(mobilitydf$Traveler, 
                                  Walker = "w", 
                                  "Dog Walker" = "dw", 
                                  Runner = "r", 
                                  Cyclist = "c", 
                                  "E-scooter Rider" = "e", 
                                  "Other Traveler" = "o")
mobilitydf$Traveler <- factor(mobilitydf$Traveler, levels = c("Walker","Dog Walker","Runner", "Cyclist", "E-scooter Rider", "Other Traveler"))

mobilitydf$Not_Recommended_factor <- as_factor(mobilitydf$Not_Recommended)
mobilitydf$Infrastructure_Crossed_factor <- as_factor(mobilitydf$Infrastructure_Crossed)

# create variable for infrastructure location to include multiple infrastructure types
mobilitydf <- mobilitydf %>% 
  mutate(Location=
    case_when(Infrastructure_Crossed >= 1 ~ "Multiple Infrastructure Locations",
              Sidewalk == 1 ~ "Sidewalk",
              Bike_Lane_With == 1 ~ "Bike Lane - With Traffic",
              Bike_Lane_Against == 1 ~ "Bike Lane - Against Traffic",
              Street_With_Traffic == 1 ~ "Street - With Traffic",
              Street_Against_Traffic == 1 ~ "Street - Against Traffic"))
mobilitydf$Location_factor <- as_factor(mobilitydf$Location)

# create binary variable for infrastructure crossed - 0=one infrastructure type, 1=multiple infrastructure types
mobilitydf <- mobilitydf %>% 
  mutate(Location_binary=
    case_when(Infrastructure_Crossed == 0 ~ 0,
              Infrastructure_Crossed >= 1 ~ 1))
mobilitydf$Location_binaryfactor <- as_factor(mobilitydf$Location_binary)

# ---------------------------------------------------------------------------------------------

# desciptive characteristics table
# summary statistics for total sample
CrossTable(mobilitydf$Traveler)
CrossTable(mobilitydf$Site)
CrossTable(mobilitydf$Weekend)
CrossTable(mobilitydf$Time)
CrossTable(mobilitydf$Sex)
CrossTable(mobilitydf$Age)
CrossTable(mobilitydf$Group)
CrossTable(mobilitydf$Location)
CrossTable(mobilitydf$Not_Recommended_factor)
CrossTable(mobilitydf$Location_binaryfactor)

# summary statistics and chi square, stratified by traveler type
site_traveler <- table(mobilitydf$Site, mobilitydf$Traveler)
site_traveler 
prop.table(site_traveler, 2)
chisq.test(site_traveler)

day_traveler <- table(mobilitydf$Weekend, mobilitydf$Traveler)
day_traveler 
prop.table(day_traveler, 2)
chisq.test(day_traveler)

time_traveler <- table(mobilitydf$Time, mobilitydf$Traveler)
time_traveler
prop.table(time_traveler, 2)
chisq.test(time_traveler)

sex_traveler <- table(mobilitydf$Sex, mobilitydf$Traveler)
sex_traveler
prop.table(sex_traveler, 2)
chisq.test(sex_traveler)

age_traveler <- table(mobilitydf$Age, mobilitydf$Traveler)
age_traveler
prop.table(age_traveler, 2)
chisq.test(age_traveler)

group_traveler <- table(mobilitydf$Group, mobilitydf$Traveler)
group_traveler
prop.table(group_traveler, 2)
chisq.test(group_traveler)

location_traveler <- table(mobilitydf$Location, mobilitydf$Traveler)
location_traveler
prop.table(location_traveler, 2)
chisq.test(location_traveler)

recommend_traveler <- table(mobilitydf$Not_Recommended_factor, mobilitydf$Traveler)
recommend_traveler
prop.table(recommend_traveler, 2)
chisq.test(recommend_traveler)

multiple_traveler <- table(mobilitydf$Location_binaryfactor, mobilitydf$Traveler)
multiple_traveler
prop.table(multiple_traveler, 2)
chisq.test(multiple_traveler)

# -------------------------------------------------------------------------------------

# logistic regression models for preferred infrastructure
## unadjusted
model1 <- glm(Not_Recommended ~ Traveler, data = mobilitydf, family = binomial)
summary(model1)
or_model1 <- or_glm(data = mobilitydf, model = model1)

## adjusted (without time of day variable)
model2 <- glm(Not_Recommended ~ Traveler +  Sex + Age + Group, data = mobilitydf, family = binomial)
summary(model2)
or_model2 <- or_glm(data = mobilitydf, model = model2)

## adjusted (with time of day variable)
# logistic regression model with time and day as regular covariates
model3 <- glm(Not_Recommended ~ Traveler + Sex + Age + Group + Time + Weekend + Site, data = mobilitydf, family = binomial)
summary(model3)
or_glm(data = mobilitydf, model = model3)
VIF(model3)


# logistic regression models for multiple infrastructure types crossed
## unadjusted
model5 <- glm(Location_binary ~ Traveler, data = mobilitydf, family = binomial)
summary(model5)
or_model5 <- or_glm(data = mobilitydf, model = model5)

## adjusted (without time of day variable)
model6 <- glm(Location_binary ~ Traveler + Sex + Age + Group, data = mobilitydf, family = binomial)
summary(model6)
or_model6 <- or_glm(data = mobilitydf, model = model6)

## adjusted (with time of day variable)
# regression model
model7 <- glm(Location_binary ~ Traveler + Sex + Age + Group + Time + Weekend + Site, data = mobilitydf, family = binomial)
summary(model7)
or_glm(data = mobilitydf, model = model7)
model7_or <-exp(cbind(OR = coef(model7), confint(model7)))
round(model7_or, digits=4)
VIF(model7)

# --------------------------------------------------------------------------------

# tables and plots for regression models
## models with time variable as covariate
tab_model(model1, model3, model5, model7,
          file="mobility_behavior_logit_table.doc")
# OR tree for all models, both outomes
plot_models(model1, model3, model5, model7,
            m.labels = c("Not Recommended Infrastructure",
                         "Not Recommended Infrastructure - Adjusted",
                         "Multiple Infrastructure Used",
                         "Multiple Infrastructure Used - Adjusted"))

# separate plots for each outcome
p1 <- plot_model(model3,
           title = "",
           axis.title = "Odds Ratio",
           wrap.labels = 100,
           colors = "black",
           show.values = TRUE,
           value.offset = .3,
           value.size = 3,
           dot.size = 2,
           line.size = .3,
           width = .2, 
           axis.lim = c(0.1,10),
           vline.color = "grey",
           axis.labels = c("East Austin (vs. University)",
                           "Weekend (vs. Weekday)",
                           "Evening (vs. Noon)",
                           "Afternoon (vs. Noon)",
                           "Morning (vs. Noon)",
                           "Traveling in Group (vs. Traveling Alone)",
                           "Older Adult (vs. Adult)",
                           "Youth (vs. Adult)",
                           "Female (vs. Male)",
                           "Other Traveler (vs. Walker)",
                           "E-Scooter Rider (vs. Walker)",
                           "Cyclist (vs. Walker)",
                           "Runner (vs. Walker)",
                           "Dog Walker (vs. Walker)"))
p1 + theme_sjplot2(base_size = 14)

p2 <- plot_model(model7,
                 title = "",
                 axis.title = "Odds Ratio",
                 wrap.labels = 100,
                 colors = "black",
                 show.values = TRUE,
                 value.offset = .3,
                 value.size = 3,
                 dot.size = 2,
                 line.size = .3,
                 width = .2, 
                 axis.lim = c(0.1,10),
                 vline.color = "grey",
                 axis.labels = c("East Austin (vs. University)",
                                 "Weekend (vs. Weekday)",
                                 "Evening (vs. Noon)",
                                 "Afternoon (vs. Noon)",
                                 "Morning (vs. Noon)",
                                 "Traveling in Group (vs. Traveling Alone)",
                                 "Older Adult (vs. Adult)",
                                 "Youth (vs. Adult)",
                                 "Female (vs. Male)",
                                 "Other Traveler (vs. Walker)",
                                 "E-Scooter Rider (vs. Walker)",
                                 "Cyclist (vs. Walker)",
                                 "Runner (vs. Walker)",
                                 "Dog Walker (vs. Walker)"))
p2 + theme_sjplot2(base_size = 14)

# just fully adjusted models in one plot
df_or <- read_csv("/Users/leighannganzar/Desktop/Post-Doc/Projects/Mobility Behavior/ORtree_both_models.csv")
df_or$Variable <- factor(df_or$Variable, levels = c("East Austin (vs. University)",
                                                    "Weekend (vs. Weekday)",
                                                    "Evening (vs. Noon)",
                                                    "Afternoon (vs. Noon)",
                                                    "Morning (vs. Noon)",
                                                    "Traveling in Group (vs. Traveling Alone)",
                                                    "Older Adult (vs. Adult)",
                                                    "Youth (vs. Adult)",
                                                    "Female (vs. Male)",
                                                    "Other Traveler (vs. Walker)",
                                                    "E-Scooter Rider (vs. Walker)",
                                                    "Cyclist (vs. Walker)",
                                                    "Runner (vs. Walker)",
                                                    "Dog Walker (vs. Walker)"))
adj_up <- 0.08
adj_down <- -0.08
ggplot(df_or, 
       aes(x = Odds, y = Variable, color = Model, shape = Model))+
  geom_vline(aes(xintercept = 1),
             size = .25,
             linetype = "dashed",
             color = "grey60") +
  geom_errorbarh(data = filter(df_or, Model == "Crossed Infrastructure"),
                 aes(xmax = CIHigh, xmin = CILow),
                 size = .5,
                 height = .1,
                 color = "gray50",
                 position = position_nudge(y = adj_up)) +
  geom_point(data = filter(df_or, Model == "Crossed Infrastructure"),
             size = 4,
             position = position_nudge(y = adj_up))+
  geom_text(data=filter(df_or, Model=="Crossed Infrastructure"),
            aes(label=Odds),
            vjust=-2.0, size=6.0, color="black")+
  geom_errorbarh(data = filter(df_or, Model == "Not Recommended Infrastructure"),
                 aes(xmax = CIHigh, xmin = CILow),
                 size = .5,
                 height = .1,
                 color = "gray50",
                 position = position_nudge(y = adj_down)) +
  geom_point(data = filter(df_or, Model == "Not Recommended Infrastructure"),
             size = 4,
             position = position_nudge(y = adj_down))+
  geom_text(data=filter(df_or, Model=="Not Recommended Infrastructure"),
            aes(label=Odds),
            hjust=.5, vjust=3.0, size=6.0) +
  coord_trans(x = "log10") +
  scale_x_continuous(breaks=c(0.1, 0.2, 0.5, 1, 2, 5, 10)) +
  scale_y_discrete(labels=c("East Austin (vs. University)",
                            "Weekend (vs. Weekday)",
                            expression(Evening^c),
                            expression(Afternoon^c),
                            expression(Morning^c),
                            "Travel in Group (vs. Travel Alone)",
                            expression(Older~Adult^b),
                            expression(Youth^b),
                            "Female (vs. Male)",
                            expression(Other~Traveler^a),
                            expression(E-Scooter~Rider^a),
                            expression(Cyclist^a),
                            expression(Runner^a), 
                            expression(Dog~Walker^a)))+
  theme_bw() +
  scale_colour_manual(values = c("gray3", "gray50")) +
  ylab("") +
  xlab("Odds Ratio")+
  theme(legend.position="right") +
  theme(legend.title = element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(legend.key.height=unit(3,"line"))

ggsave("/Users/leighannganzar/Desktop/Post-Doc/Projects/Mobility Behavior/bothmodels_oneplot.png", height=20, width=18, dpi=500)

## models WITHOUT time variable 
tab_model(model1, model2, model5, model6,
          file="mobility_behavior_withouttime_logit_table.doc")
plot_models(model2, model6,
            m.labels = c("Not Recommended Infrastructure",
                         "Multiple Infrastructure Used"))

## models with time variable with robust standard error to account for clustering
tab_model(model1, model2, model5, model6, 
          robust = TRUE,
          vcov.fun = "CR", 
          vcov.type = "CR1",
          vcov.args = list(cluster = mobilitydf$Time),
          file="mobility_behavior_robustSE_logit_table.doc")

