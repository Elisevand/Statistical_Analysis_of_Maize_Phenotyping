################################################################################
# UCPH #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/UCPH/UCPH_Initial Code Draft")
library(statgenHTP)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggplot2)

################################################################################
# DATA PREPARATION #
################################################################################

# Data importation
data_height <- read.csv("Cross Platform_Automatic_Height.xlsx - PhenoLab_01.06-02.07.2021.csv", header=TRUE)
data_biomass <- read.csv("EPPN_Biomass_PhenoLab.xlsx - Sheet1.csv", header=TRUE)
isaTAB <- read.csv("ISA_EPPN2020_UCPH - s_exp.csv", header=TRUE)


# Remove first row of data_biomass
names(data_biomass) <- data_biomass[1, ]
data_biomass <- data_biomass[-1, ]


# Add columns for genotype and soil type
data_biomass$Soil <- sapply(strsplit(as.character(data_biomass$Maize_cultivar), split =","), '[', 1)
data_biomass$Genotype <- sapply(strsplit(as.character(data_biomass$Maize_cultivar), split =","), '[', 2)


# Rename columns 
colnames(data_biomass) <- c("Number", "Maize_cultivar", "DW_shoot_g", "DW_root_g", "Soil", "Genotype")
colnames(data_height) <- c("Timestamp", "Type", "Number", "Height_mm")


# Create a replication variable
data_biomass$Replication <- data_biomass$Maize_cultivar
join <- unique(data_biomass$Maize_cultivar)

for (valeur in join) {
  count <- 1
  for (idx in 1:nrow(data_biomass)) {
    if (data_biomass$Maize_cultivar[idx] == valeur) {
      data_biomass$Replication[idx] <- count
      count <- (count + 1)   
    }
  }
  print(paste("Found " , count , " " , valeur))
}


# Add variables to data_height
Replication <- data.frame(data_biomass$Number, data_biomass$Soil, data_biomass$Genotype, data_biomass$Replication, data_biomass$Maize_cultivar)
colnames(Replication) <- c("Number","Soil", "Genotype", "Replication", "Maize_cultivar")
str(Replication)
Replication$Number <- as.numeric(Replication$Number)


# Add replication variable to data_height
data_height <- left_join(data_height, Replication, by="Number")

# Convert to format date for data_height
data_height$Timestamp <- as.POSIXct(data_height$Timestamp, format = "%d/%m/%Y %H.%M.%S")

# Reverse columns for data_height because they are inverted
data_height_rev <- data.frame(rev(data_height$Timestamp), rev(data_height$Number), rev(data_height$Height_mm), rev(data_height$Soil), rev(data_height$Genotype), rev(data_height$Replication))
colnames(data_height_rev) <- c("Timestamp", "Number", "Height_mm", "Soil", "Genotype", "Replication")




################################################################################
# Descriptive statistics #######################################################
################################################################################

summary(data_bio$DW_shoot_g)
summary(data_bio$DW_root_g)



















################################################################################
# TIME POINT OBJECT #
################################################################################

TP_data_UCPH <- createTimePoints(dat = data_height,
                                experimentName = "EPPN2020 UCPH",
                                genotype = "Genotype",
                                timePoint = "Timestamp",
                                plotId = "Number",
                                repId = "Replication")

###### DETECT OUTLIERS

plant_selection <- unique(data_height$Number[data_height$Soil == "S1" & data_height$Genotype == "EPPN1_L"])

outlier_detection_test <- detectSingleOut(TP = TP_data_UCPH, 
                                          trait = "Estimated_Plant_Height_mm", 
                                          plotIds = plant_selection,
                                          confIntSize = 3, 
                                          nnLocfit = 0.5, 
                                          checkEdges = TRUE)

plot(outlier_detection_test,
     outOnly = FALSE)

nrow(outlier_detection_test[outlier_detection_test$outlier == 1,])

outlier_detection_test_clean <- removeSingleOut(TP_data_M3P, outlier_detection_test)

plot(outlier_detection_test_clean,
     outOnly = FALSE)


outlier_detection <- detectSingleOut(TP = TP_data_M3P, 
                                     trait = "Estimated_Plant_Height_mm",
                                     confIntSize = 5,
                                     nnLocfit = 0.5,
                                     checkEdges = TRUE)
# HOW MANY OUTLIERS

nrow(outlier_detection[outlier_detection$outlier == 1,])

# HOW MANY OUTLIERS PER DATE -> Ne marchera pas car on a trop de timepoints différents...

#with(outlier_detection[outlier_detection$outlier == 1,], table(timePoint))

# REMOVE OUTLIERS

TP_data_M3P_clean <- removeSingleOut(TP_data_M3P, 
                                     outlier_detection)

data_M3P_clean <- as.data.frame(TP_data_M3P_clean)


###### DATA VISUALISATION

ggplot(data_M3P_clean[data_M3P_clean$Datetime %in% c("2020-02-14","2020-02-15", "2020-02-16", "2020-02-17"), ], aes(x = Datetime, y = Estimated_Plant_Height_mm, color = Datetime)) +
  geom_violin()+
  geom_boxplot(width=0.1)+ 
  geom_point()+
  ggtitle("Estimated_Plant_Height_mm on the 2020-02-14, 2020-02-15, 02-16 and 02-17 after outlier detection")

data_M3P_genotype_1_clean <- data_M3P_clean[data_M3P_clean$Genotype == "EPPN7_L",]

ggplot(data = data_M3P_genotype_1, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_line(aes(colour = Rep)) +
  geom_point(aes(colour = Rep))+
  facet_wrap(~Water)+
  ggtitle("Estimated Plant Height in mm for genotype EPPN7_L after outlier detection")

test <- na.omit(data_M3P_clean)







