# Exemple M3P sur ALSIA

## Set working directory to the path of this file:
setwd("~/Documents/PostDoc/EPPN2020/Cross-platform_expe/Data/ALSIA/")

## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if (!("statgenHTP" %in% pkgs))    { install.packages("statgenHTP")}
if (!("dplyr" %in% pkgs))    { install.packages("dplyr")}
if (!("gridExtra" %in% pkgs))    { install.packages("gridExtra")}
#
suppressPackageStartupMessages(library(statgenHTP))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(dplyr))

library(lubridate)









data <- read.table("img_extracted.txt", header=TRUE)

# vecteur geno qui contient tous les noms des génotypes utilisés et le traitement en eau

geno <- data.frame(genotype = unique(data$Unit.ID))

geno$type <- sapply(strsplit(as.character(geno$genotype), split = "/"), '[', 3)
#geno$water <- sapply(strsplit(as.character(geno$genotype), split = "/"), '[', 4)

# si osef du traitement en eau

#geno_final <- data.frame(genotype = unique(geno$type)) # contient les 30 génotypes 
#geno_final$type <- as.character(geno_final$genotype)


# data <- inner_join(data, geno_final, by=c("genotype")) #MARCHE PAS 
reduced_data <- cbind(data[1:446,],geno$type)
names(reduced_data)[7] <- "genotype"
date <- date(reduced_data$Timestamp)
reduced_data <- cbind(reduced_data, date)

reduced_reduced_data <- reduced_data[1:38,]

# subset pour un seul trait

estimated_plantHeight <- data[data$Estimated_PlantHeight,]
estimated_plantHeight <- droplevels(estimated_plantHeight) # MARCHE PAS? SERT A QUOI?


TP_M3P <- createTimePoints(dat=reduced_data,                 # ou bien reduced_reduced_data
                           experimentName="EPPN2020_M3P",
                           genotype="genotype",
                           timePoint="Timestamp",
                           plotId="Unit.ID")

summary(TP_M3P)

# on commence les plots

# plotlayout mais il faut rajouter les nrow et ncol dans les données alors -> grâce au fichier ISA-tab
#plot(TP_M3P, plotType="layout", timePoints=5, highlight=c("EPPN7_T","EPPN1_H","EPPN1_L"))


plot(TP_M3P, plotType="raw", traits="Estimated_Plant_Leaf_Area") # MARCHE PAS





























