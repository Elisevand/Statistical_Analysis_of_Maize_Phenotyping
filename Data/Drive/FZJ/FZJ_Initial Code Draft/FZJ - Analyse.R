################################################################################
# FZJ #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/FZJ/FZJ_Initial Code Draft")
library(statgenHTP)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(readxl)

View(ISA_EPPN2020_FZJ)

################################################################################
# Import functions #
################################################################################
source("~/Mémoire/functions.R")

################################################################################
# DATA PREPARATION #
################################################################################

# Data importation
isaTAB <- read.csv("ISA_EPPN2020_FZJ - s_exp.csv", dec = ",")
data <-read.delim2("multisensor.txt", stringsAsFactors = TRUE, dec = ".")

unique(data$Variable.ID)

# Observer la distribution des échantillons pour chaque facteur
table(df[, c("Variable.ID", "Genotype")])

# Verifier la colinéarité
# MARCHE PAS ggplot(df, aes(x=df[df$Variable.ID == var[1], ]$Value, y=df[df$Variable.ID == var[2], ]$Value))+
#  geom_point()

data_imaging <- read.delim2("img_extracted.txt", header = TRUE)

# Extract the genotype from the isaTAB data
genotypes <- data.frame(isaTAB$Source.Name, isaTAB$Sample.Name)
names(genotypes) <- c("Genotype", "Unit.ID")

# Extract the position from the isaTAB data
position <- coordinates_isaTAB(isaTAB)

# Rajouter le génotype au data
df <- left_join(data, genotypes, by ="Unit.ID")

# Date au bon format
df$Date <- as.Date(df$Timestamp, format = "%Y_%m_%d")

# Graphiques boxplot

# Pour la premiere variable : Shoot_fresh_weight

ggplot(df[df$Variable.ID == "Shoot_fresh_weight", ], aes(y = Value))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = "Variance of the Shoot_fresh_weight")

# Pour la deuxième variable 
ggplot(df[df$Variable.ID == var[2], ], aes(y = Value))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = var[2])

# Pour toutes les variables
ggplot(df, aes(y = Value)) +
  geom_boxplot(aes(x = Genotype, fill = Genotype)) +
  facet_wrap(~Variable.ID, scales = "free") +
  labs(
    title = "Boxplots of Value by Genotype for Each Variable",
    x = "Genotype",
    y = "Value"
  ) +
  theme(
    axis.text.x = element_blank(),    
    axis.ticks.x = element_blank(),    
    plot.title = element_text(hjust = 0.5)
  )

# Graphiques de normalité pour la première variable

hist(df[df$Variable.ID == var[1], ]$Value, probability = TRUE, col = "lightblue")
curve(dnorm(x, mean = mean(na.omit(df[df$Variable.ID == var[1], ]$Value)), sd = sd(na.omit(df[df$Variable.ID == var[1], ]$Value))), 
            add = TRUE, col = "green", lwd = 2)
lines(density(na.omit(df[df$Variable.ID == var[1], ]$Value)), col = "blue", lwd = 2)


# Scatter plot pour les dernieres variables (time series)

ggplot(df[df$Variable.ID == var[7], ], aes(x = Date, y = Value))+
  geom_point(aes(colour = Genotype))+
  scale_x_date(labels = scales::label_date_short())+
  geom_smooth(aes(colour = Genotype))+
  labs(title = var[7])+
  facet_wrap(~Genotype)

ggplot(df[df$Variable.ID == var[8], ], aes(x = Date, y = Value))+
  geom_point(aes(colour = Genotype))+
  scale_x_date(labels = scales::label_date_short())+
  geom_smooth(aes(colour = Genotype))+
  labs(title = var[8])+
  facet_wrap(~Genotype)




###### PREMIER MODELE, plant height en fonction du nombre de feuilles

lm.test <- lm(df[df$Variable.ID == "Shoot_dry_weight", ]$Value ~ df[df$Variable.ID == "Shoot_fresh_weight", ]$Value, data = df)

lm.test.resid <- rstandard(lm.test)

plot(lm.test.resid ~ as.factor(df$Unit.ID), xlab = "Genotype",
     ylab = "Standardized residuals")



















































