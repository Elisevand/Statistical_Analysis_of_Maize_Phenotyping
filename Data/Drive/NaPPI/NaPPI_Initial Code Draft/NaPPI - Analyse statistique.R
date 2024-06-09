################################################################################
# NaPPI #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/NaPPI/NaPPI_Initial Code Draft")
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
isaTAB  <- read.csv("ISA_EPPN2020_NaPPI - s_exp.csv", dec = ",")
data_DW <-read.delim2("2020_NaPPI_DM.txt", stringsAsFactors = TRUE)
data_FW <- read.delim2("2020_NaPPI_FW.txt", stringsAsFactors = TRUE)
rgb <- read.delim("2020_NaPPI_RGB1.txt", stringsAsFactors = FALSE)


# Extract the position from the isaTAB data
position <- coordinates_isaTAB(isaTAB)


# Select columns and rename columns
rgb <- subset(rgb, select = -c(Round.Order, Position, Tray.ID, Device.ID))
names(rgb)[3:4] = c("Genotype", "Soil")

rgb$Plant.ID <- sapply(strsplit(as.character(rgb$Plant.ID), split ="_"), '[', 2)
rgb$Plant.ID <- sapply(strsplit(as.character(rgb$Plant.ID), split ="e"), '[', 2)
rgb$Plant.ID <- (as.numeric(rgb$Plant.ID))

rgb$Soil <- sapply(strsplit(as.character(rgb$Soil), split = "_"), '[', 1)
rgb$Soil <- as.factor(rgb$Soil)

# EXPLORATION PLOTS

ggplot(data_DW, aes(y = shoot_dry_biomass_scale_gram))+
  geom_boxplot(aes(x = Plant.Info))


rgb$test <- as.Date(rgb$Timestamp)

ggplot(rgb[rgb$Plant.ID==1, ], aes(x = test, y = shoot_side_width_mm))+
  geom_point(aes(colour=Genotype)) ##########+
  ###########scale_x_date(date_breaks = "2 days", date_labels = "%b %d")


  




