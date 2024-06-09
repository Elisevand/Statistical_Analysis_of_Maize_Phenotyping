################################################################################
# SPPU #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/SPPU/SPPU_Initial Code Draft")
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

rgb1 <- read.csv("Rgb1_Morpho_Plant.xlsx - Content.csv", dec = ",", stringsAsFactors = TRUE)
rgb2 <- read.csv("Rgb2_Morpho_Plant.xlsx - Content.csv", dec = ",")
isaTAB <- read.csv("ISA_EPPN2020_SPPU - s_exp.csv")


# Extract the position from the isaTAB data

position <- coordinates_isaTAB(isaTAB)


# Select columns and rename columns

rgb1 <- subset(rgb1, select = -c(Experiment.ID, Round.Order, Tray.ID, Tray.Info, Position))
rgb2 <- subset(rgb2, select = -c(Experiment.ID, Round.Order, Tray.ID, Tray.Info, Position))


names(rgb1)[1:5] = c("Date", "Timestamp", "Plant.ID", "Genotype", "Soil")
names(rgb2)[1:5] = c("Date", "Timestamp", "Plant.ID", "Genotype", "Soil")

rgb1$Date <- as.Date(rgb1$Date)

# Rename the Plant.ID from 1 to 108

rgb1$Plant.ID <- sapply(strsplit(as.character(rgb1$Plant.ID), split ="_"), '[', 3)
rgb1$Plant.ID <- as.integer(rgb1$Plant.ID)
rgb1$Plant.ID <- rgb1$Plant.ID -4000

rgb2$Plant.ID <- sapply(strsplit(as.character(rgb2$Plant.ID), split ="_"), '[', 3)
rgb2$Plant.ID <- as.integer(rgb2$Plant.ID)
rgb2$Plant.ID <- rgb2$Plant.ID -4000

# EXPLORATION PLOT
rgb1$Angle <- as.factor(rgb1$Angle)
rgb1$Plant.ID <- as.factor(rgb1$Plant.ID)

ggplot(rgb1, aes(x = Date, y = AREA_PX))+
  geom_point(aes(colour = Soil))+
  facet_wrap(~ Genotype)

unique(rgb)




