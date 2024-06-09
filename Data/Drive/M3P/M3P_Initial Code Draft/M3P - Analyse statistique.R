################################################################################
# M3P #
################################################################################

setwd("~/Mémoire/Main/Data/Drive/M3P/M3P_Initial Code Draft")
library(statgenHTP)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggplot2)

################################################################################
# DATA PREPARATION #
################################################################################

data <- read.table("img_extracted.txt", header=TRUE)
data <- na.omit(data)
isaTAB <- read.csv("ISA_EPPN2020_M3P - s_exp.csv", header=TRUE)

test <- read.table("img_raw.txt", header=TRUE)


###### FOR DATA VARIABLE #######
genotype_list_data <- data.frame(Unit.ID = data$Unit.ID)
data$genotype <- cbind(sapply(strsplit(as.character(genotype_list_data$Unit.ID), split = "/"), '[', 3))
data$water <- cbind(sapply(strsplit(as.character(genotype_list_data$Unit.ID), split = "/"), '[', 4))
data$replication <- cbind(sapply(strsplit(as.character(genotype_list_data$Unit.ID), split ="/"), '[', 5))

data$join <- paste(data$genotype, data$water, data$replication)

###### FOR ISATAB VARIABLE #######
genotype_list_isaTAB <- data.frame(Sample.Name = isaTAB$Sample.Name)
isaTAB$genotype <- cbind(sapply(strsplit(as.character(genotype_list_isaTAB$Sample.Name), split = "/"), '[', 3))
isaTAB$water <- cbind(sapply(strsplit(as.character(genotype_list_isaTAB$Sample.Name), split = "/"), '[', 4))
isaTAB$replication <- cbind(sapply(strsplit(as.character(genotype_list_isaTAB$Sample.Name), split ="/"), '[', 5))

isaTAB$join <- paste(isaTAB$genotype, isaTAB$water, isaTAB$replication)

###### FOR COORDINATES DATA FRAME ######
coordinates <- isaTAB$Characteristics.Spatial.Distribution.
nrow <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 1)
nrow <- sapply(strsplit(as.character(nrow), split="="), '[', 2)
ncol <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 2)
ncol <- sapply(strsplit(as.character(ncol), split ="="), '[', 2)
rep <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 3)
rep <- sapply(strsplit(as.character(rep), split ="="), '[', 2)

join <- isaTAB$join
position <- data.frame(cbind(nrow, ncol, rep, join))

###### ONE DF CONTAINING THE COORDINATES ######
data_M3P <- left_join(data, position, by="join")

###### DATA FORMATTING FOR THE TIMEPOINTS OBJECT ######
data_M3P$Timestamp <- as.POSIXct(data_M3P$Timestamp)
data_M3P$Datetime <- format(data_M3P$Timestamp, format = "%Y-%m-%d")

data_M3P <- data_M3P %>%
  rename(Estimated_Plant_LeafArea_mm2=Estimated_PlantLeafArea,
         Estimated_Plant_Biomass_g_FW=Estimated_PlantBiomass,
         Estimated_Plant_Height_mm=Estimated_PlantHeight,
         Genotype=genotype,
         Water=water,
         Nrow=nrow,
         Ncol=ncol,
         Rep=rep
         ) #FW signifie Fresh Weight



# Repartition dans la serre
data_M3P <- data_M3P %>%
  mutate(Highlight = ifelse(Genotype == "EPPN20_T", "EPPN20_T", "Other"))
ggplot(data_M3P, aes(x = ncol, y = nrow, fill = Highlight, label = Unit.ID)) +
  geom_tile(aes(alpha = Highlight), color = "white") + 
  geom_text(size = 5) + 
  scale_fill_manual(values = c("EPPN20_T" = "red", "Other" = "grey80")) + 
  scale_alpha_manual(values = c("EPPN20_T" = 1, "Other" = 0.5)) +
  theme_minimal() + 
  labs(title = "Carte des coordonnées", x = "Colonne", y = "Ligne") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

################################################################################
# DATA MANIPULATION #
################################################################################
recuperation <- data_M3P


################################################################################
data_M3P <- recuperation
################################################################################



###### DATA VISUALISATION

data_M3P_filtered <- data_M3P %>%
  group_by(Genotype, Unit.ID, Water)

# Genotype graph
ggplot(data_M3P_filtered, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_point(aes(colour = Unit.ID, shape = factor(Water)))+
  geom_line(aes(colour = Unit.ID))+
  theme(legend.position = "none")+
  facet_wrap(~Genotype)

# Water treatment graph
ggplot(data_M3P_filtered, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_point(aes(colour = Unit.ID, shape = factor(Water))) +
  geom_line(aes(colour = Unit.ID)) +
  theme(legend.position = "none") +
  facet_wrap(~Water) +
  labs(
    title = "Estimated Plant Height Over Time by Water Treatment",
    x = "Timestamp",
    y = "Estimated Plant Height (mm)"
  )


###### HOW MANY DAYS ARE THERE PER OBSERVATION UNIT ? IS THERE MORE THAN ONE OBSERVATION PER DAY ?

data_M3P_grouped <- data_M3P %>%
  group_by(Unit.ID)

data_M3P_grouped <- data_M3P_grouped %>%
  mutate(
    n_days_unique = n_distinct(Datetime),
    n_days_duplicated = n() - n_days_unique,
    n_days_total = n_days_unique + n_days_duplicated
    )

data_time_M3P <- data_M3P_grouped %>%
  select(Unit.ID, n_days_unique, n_days_duplicated, n_days_total)
data_time_M3P <- distinct(data_time_M3P, Unit.ID, n_days_unique, n_days_duplicated, n_days_total)
print(min(data_time_M3P$n_days_unique))



###### STRANGE VALUE FOR THE PLANT NAME EEPN_2/WD1/EPPN_Rep_2

ggplot(data_M3P[data_M3P$Unit.ID %in% "0148/ZM4526/EPPN2_L/WD1/EPPN_Rep_2/03_28/ARCH2020-02-03", ], aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_line()+
  geom_point()+
  ggtitle("Growth of plant EPPN2_L/WD1/EPPN_Rep_2")

ggplot(data_M3P[data_M3P$Unit.ID %in% "0006/ZM4526/EPPN2_L/WD1/EPPN_Rep_1/01_06/ARCH2020-02-03", ], aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_line()+
  geom_point()+
  ggtitle("Growth of plant EPPN2_L/WD1/EPPN_Rep_1")

ggplot(data_M3P[data_M3P$Unit.ID %in% c("0148/ZM4526/EPPN2_L/WD1/EPPN_Rep_2/03_28/ARCH2020-02-03", "0006/ZM4526/EPPN2_L/WD1/EPPN_Rep_1/01_06/ARCH2020-02-03"), ], aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_line(aes(colour = Unit.ID)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Estimated Plant Height Over Time",
    x = "Timestamp",
    y = "Estimated Plant Height (mm)",
    colour = "Unit ID"
  )
  
# GET RID OF THE PLANT 0148/ZM4526/EPPN2_L/WD1/EPPN_Rep_2/03_28/ARCH2020-02-03

data_M3P <- data_M3P[!(data_M3P$Unit.ID == "0148/ZM4526/EPPN2_L/WD1/EPPN_Rep_2/03_28/ARCH2020-02-03"), ]



###### STRANGE VALUES ON THE BEGINNING OF THE EXPERIMENT 02_14 -> 02-16

ggplot(data_M3P[data_M3P$Datetime %in% c("2020-02-14", "2020-02-15", "2020-02-16", "2020-02-17"), ], 
       aes(x = Datetime, y = Estimated_Plant_Height_mm, color = Datetime)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, size = 1.5) +
  labs(
    title = "Distribution of Estimated Plant Height Over Selected Dates",
    x = "Date",
    y = "Estimated Plant Height (mm)",
    color = "Date"
  ) +
  theme(
    legend.position = "right"
  )

count_high_values <- data_M3P %>%
  filter(Datetime %in% as.Date(c("2020-02-14", "2020-02-15", "2020-02-16")) & Estimated_Plant_Height_mm > 250) %>%
           nrow()
print(count_high_values)

# GET RID OF THE VALUES OVER 250

data_M3P <- data_M3P %>%
  filter(!(Datetime == c("2020-02-14") & Estimated_Plant_Height_mm > 250))

data_M3P <- data_M3P %>%
  filter(!(Datetime == c("2020-02-15") & Estimated_Plant_Height_mm > 250))

data_M3P <- data_M3P %>%
  filter(!(Datetime == c("2020-02-16") & Estimated_Plant_Height_mm > 250))

# DATA VISUALISATION AFTER MANIPULATIONS 

ggplot(data_M3P[data_M3P$Datetime %in% c("2020-02-14","2020-02-15", "2020-02-16", "2020-02-17"), ], aes(x = Datetime, y = Estimated_Plant_Height_mm, color = Datetime)) +
  geom_violin()+
  geom_boxplot(width=0.1)+  
  geom_point()+
  ggtitle("Estimated_Plant_Height_mm on the 2020-02-14, 2020-02-15, 02-16 and 02-17 after removal of abnormal values")



###### VERIFICATION 

data_M3P_filtered <- data_M3P %>%
  group_by(Genotype, Unit.ID, Water)

# Genotype graph
ggplot(data_M3P_filtered, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_point(aes(colour = Unit.ID, shape = factor(Water)))+
  geom_line(aes(colour = Unit.ID))+
  theme(legend.position = "none")+
  facet_wrap(~Genotype)


###### COUNT OF THE DAYS AFTER REMOVAL OF ABNORBAL VALUES

data_M3P_grouped <- data_M3P %>%
  group_by(Unit.ID)

data_M3P_grouped <- data_M3P_grouped %>%
  mutate(
    n_days_unique_clean = n_distinct(Datetime),
    n_days_duplicated_clean = n() - n_days_unique_clean,
    n_days_total_clean = n_days_unique_clean + n_days_duplicated_clean
  )

data_time_M3P_clean <- data_M3P_grouped %>%
  select(Unit.ID, n_days_unique_clean, n_days_duplicated_clean, n_days_total_clean)
data_time_M3P_clean <- distinct(data_time_M3P_clean, Unit.ID, n_days_unique_clean, n_days_duplicated_clean, n_days_total_clean)


###### IS IT ON THE SAME DAYS ?



###### CAN WE AVERAGE PER DAY ? 

#average_height <- NA
#average_timestamp <- NA

#for (i in 2:nrow(data_M3P)) {
#  if (data_M3P$Unit.ID[i] == data_M3P$Unit.ID[i - 1]) {
#    if (data_M3P$Datetime[i] == data_M3P$Datetime[i - 1]){
#      average_height[i] <- (data_M3P$Estimated_Plant_Height_mm[i-1] + data_M3P$Estimated_Plant_Height_mm[i]) / 2
#      average_timestamp[i] <- (as.numeric(data_M3P$Timestamp[i-1]) + as.numeric(data_M3P$Timestamp[i])) / 2 
#      average_timestamp[i] <- as.Date(average_timestamp, format = "%Y-%m-%d %H:%M:%S")
#    }
#  }
#}

## Rajouter une colonne Unit.ID et une Datetime pour garder les infos, puis faire un left/right/inner_join des deux datasets

#average <- data.frame(average_timestamp, average_height)
#average <- na.omit(average)

###### LAST DATA VISUALISATION

ggplot(data_M3P_filtered, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_point(aes(colour = Unit.ID, shape = factor(Water)))+
  geom_line(aes(colour = Unit.ID))+
  theme(legend.position = "none")+
  facet_wrap(~Genotype)

data_M3P_genotype_1 <- data_M3P[data_M3P$Genotype == "EPPN7_L",]
data_M3P_genotype_1$Water <- as.factor(data_M3P_genotype_1$Water)


ggplot(data = data_M3P_genotype_1, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
  geom_line(aes(colour = Rep)) +
  geom_point(aes(colour = Rep))+
  facet_wrap(~Water)+
  ggtitle("Estimated Plant Height in mm for genotype EPPN7_L")


################################################################################
# TIME POINT OBJECT #
################################################################################

TP_data_M3P <- createTimePoints(dat = data_M3P,
                            experimentName = "EPPN2020 M3P",
                            genotype = "Genotype",
                            timePoint = "Timestamp",
                            plotId = "Unit.ID",
                            repId = "Rep", rowNum = "Nrow", colNum = "Ncol")

summary(TP_data_M3P)

###### DETECT OUTLIERS

plant_selection <- unique(data_M3P$Unit.ID[data_M3P$Water == "WW" & data_M3P$Genotype == "EPPN7_L"])

outlier_detection_test <- detectSingleOut(TP = TP_data_M3P, 
                                     trait = "Estimated_Plant_Height_mm", 
                                     plotIds = plant_selection,
                                     confIntSize = 5, 
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



################################################################################
# CORRECTING FOR SPATIAL TRENDS #
################################################################################

model <- fitModels(TP = TP_data_M3P_clean,
                   trait = "Estimated_Plant_Height_mm",
                   extraFixedFactors = c("Rep"),
                   geno.decomp = "type")






ggplot(data = data_M3P_clean[1:2000,], aes( y = plotId, x = timePoint))+
  geom_point()+
  theme(legend.position = "none")




