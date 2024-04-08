
library(psych)
library(broom)
library(ggplot2)
library(tidyverse)
library(viridis) # colors
library(ggpubr) # for ggarrange()
library(ggridges) # for ridgeplots

rm(list=ls())
hexaco<-read.csv(file='hexaco.csv', header=TRUE, sep=',')
View(hexaco)
neo<-read.csv(file='neo.csv', header=TRUE, sep=',')

####################################

hexaco_excluded <- hexaco[, -c(1, 2)]

corr.test(hexaco_excluded, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5,normal=TRUE)

# set color palette
pal <- viridis(2) 

####################################


## H atomic

hexaco$target <- ifelse(hexaco$Trait=="ho", "Honesty", "Off-target")
ggplot(hexaco, aes(x=AH, y=EH)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Honesty-Humility loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Honesty", "Off-target")) + # set colors
  theme_minimal() -> reg.hon
reg.hon

## E atomic

hexaco$target <- ifelse(hexaco$Trait=="em", "Emotionality", "Off-target")
ggplot(hexaco, aes(x=AE, y=EE)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Emotionality loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Emotionality", "Off-target")) + # set colors
  theme_minimal() -> reg.emo
reg.emo

## X atomic

hexaco$target <- ifelse(hexaco$Trait=="ex", "Extraversion", "Off-target")
ggplot(hexaco, aes(x=EX, y=AX)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Extraversion loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Extraversion", "Off-target")) + # set colors
  theme_minimal() -> reg.ext
reg.ext


## A atomic

hexaco$target <- ifelse(hexaco$Trait=="ag", "Agreeableness", "Off-target")
ggplot(hexaco, aes(x=EA, y=AA)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Agreeableness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Agreeableness", "Off-target")) + # set colors
  theme_minimal() -> reg.agr
reg.agr

## C atomic

hexaco$target <- ifelse(hexaco$Trait=="co", "Conscientiousness", "Off-target")
ggplot(hexaco, aes(x=AC, y=EC)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Conscientiousness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Conscientiousness", "Off-target")) + # set colors
  theme_minimal() -> reg.con
reg.con


## O atomic

hexaco$target <- ifelse(hexaco$Trait=="op", "Openness", "Off-target")
ggplot(hexaco, aes(x=AO, y=EO)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="HEXACO Openness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = 1, breaks=c("Openness", "Off-target")) + # set colors
  theme_minimal() -> reg.ope
reg.ope


#############################################################################

neo_excluded <- neo[, -c(1, 2)]

corr.test(neo_excluded, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5,normal=TRUE)

## O atomic

neo$target <- ifelse(neo$Trait=="op", "Openness", "Off-target")
ggplot(neo, aes(x=AO, y=EO)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="NEO Openness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = 1, breaks=c("Openness", "Off-target")) + # set colors
  theme_minimal() -> reg.ope2
reg.ope2

## C atomic

neo$target <- ifelse(neo$Trait=="co", "Conscientiousness", "Off-target")
ggplot(neo, aes(x=AC, y=EC)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="NEO Conscientiousness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Conscientiousness", "Off-target")) + # set colors
  theme_minimal() -> reg.con2
reg.con2

## E atomic

neo$target <- ifelse(neo$Trait=="ex", "Extraversion", "Off-target")
ggplot(neo, aes(x=AE, y=EE)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="NEO Extraversion loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Extraversion", "Off-target")) + # set colors
  theme_minimal() -> reg.ext2
reg.ext2

## A atomic

neo$target <- ifelse(neo$Trait=="ag", "Agreeabelness", "Off-target")
ggplot(neo, aes(x=AA, y=EA)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="NEO Agreeableness loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Agreeableness", "Off-target")) + # set colors
  theme_minimal() -> reg.agr2
reg.agr2


## N atomic

neo$target <- ifelse(neo$Trait=="ne", "Neuroticism", "Off-target")
ggplot(neo, aes(x=AN, y=EN)) +
  geom_smooth(method = lm, color = "darkgrey") +
  geom_point(size = 5, alpha = .7, aes(color = target)) + # add target color after geom_smooth to preserve trendline
  labs(title="NEO Neuroticism loadings",
       y="Empirical loading",
       x="Pseudo loading",
       color="Trait") +
  scale_color_viridis_d(direction = -1, breaks=c("Neuroticisim", "Off-target")) + # set colors
  theme_minimal() -> reg.neu2
reg.neu2




