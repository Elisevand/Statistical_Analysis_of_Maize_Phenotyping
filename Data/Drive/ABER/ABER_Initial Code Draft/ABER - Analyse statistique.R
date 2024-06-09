################################################################################
# ABER #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/ABER/ABER_Initial Code Draft")
library(statgenHTP)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggplot2)

################################################################################
# Import functions #
################################################################################
source("~/Mémoire/functions.R")

################################################################################
# DATA PREPARATION #
################################################################################

# Data importation

data <- read.delim("ISATab_a_phe_ZM009.txt")
names(data) <- c("barcode", "leaf_no", "shoot_fwt", "root_fwt", "shoot_dwt", "root_dwt", "shoot_dwt.fwt")
var <- names(data[2:7])

isaTAB <- read.csv("ISA_EPPN2020_ABER - s_exp.csv")

# Pas trop compris les root_pixels trucs...

# Extraction des coordonnées du isaTAB

position <- coordinates_isaTAB(isaTAB)

# Extraction du génotype du isaTAB

genotype <- isaTAB$Source.Name
# a faire plus tard : vérifier si ce sont les mêmes ordres de génotype et plantbarcode...

# Ajouter facteur génotype
data$genotype <- genotype


# EXPLORATION PLOTS

# Graphiques boxplot

# Pour les 7 variables

# Pour leaf_no

ggplot(data, aes(y = data[[var[1]]]))+
  geom_boxplot(aes(x = genotype, fill = genotype))+
  theme(legend.position = "none")+
  labs(title = (var[1]))

ggplot(data, aes(y = leaf_no))+
  geom_boxplot(aes(x = genotype, fill = genotype))+
  theme(legend.position = "none")+
  labs(title = "Variance of leaf number")

ggplot(data, aes(y = shoot_fwt))+
  geom_boxplot(aes(x = genotype, fill = genotype))+
  theme(legend.position = "none")+
  labs(title = "Distribution of Shoot Fresh Weight by Genotype")

ggplot(data, aes(y = root_fwt))+
  geom_boxplot(aes(x = genotype, fill = genotype))+
  theme(legend.position = "none")+
  labs(title = "Variance of root_fwt")

# etc pour les 2 dry weight

ggplot(data, aes(y = shoot_dwt.fwt))+
  geom_boxplot(aes(x = genotype, fill = genotype))+
  theme(legend.position = "none")+
  labs(title = "Variance of shoot_dwt.fwt")


# Histogramme de probabilité pour leaf_no

ggplot(data, aes(x = leaf_no)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(na.omit(data$leaf_no)), sd = sd(na.omit(data$leaf_no))), 
                color = "green", size = 1.5, linetype = "dashed") +
  geom_density(color = "blue", size = 1.5) +
  labs(title = "Histogram of Leaf Number with Density and Normal Distribution",
       x = "Leaf Number",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# etc pour les autres 

# Scatter plot pour les time series 









