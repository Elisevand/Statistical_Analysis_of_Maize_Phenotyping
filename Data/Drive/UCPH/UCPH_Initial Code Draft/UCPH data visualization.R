################################################################################
# UCPH data visualization ######################################################
################################################################################

library(ggplot2)
library(viridis)
library(lubridate)
library(ggforce)

################################################################################
# IMPORT DATA + DATA PREPARATION ###############################################
################################################################################

data <- data_height_rev
data[1, ]

data_bio <- data_biomass

# Convert to the right format 

data$Height_mm <- gsub(",", ".", data$Height_mm)
data$Height_mm <- as.numeric(data$Height_mm)

data$Number <- as.factor(data$Number)
data$Soil <- as.factor(data$Soil)
data$Genotype <- as.factor(data$Genotype)
data$Replication <- as.factor(data$Replication)
data$Date <- as.Date(data$Timestamp)
str(data)


data_bio$DW_shoot_g <- as.numeric(gsub(",", ".", data_bio$DW_shoot_g))
data_bio$DW_root_g <- as.numeric(gsub(",", ".", data_bio$DW_root_g))

data_bio$Number <- as.factor(data_bio$Number)
data_bio$Maize_cultivar <- as.factor(data_bio$Maize_cultivar)
data_bio$Soil <- as.factor(data_bio$Soil)
data_bio$Genotype <- as.factor(data_bio$Genotype)
data_bio$Replication <- as.factor(data_bio$Replication)
str(data_bio)

################################################################################
################################################################################
# FOR HEIGHT ###################################################################



################################################################################
# Histogram of the number of observations per day ##############################
################################################################################

ggplot(data, mapping = (aes(x = Timestamp)))+
  geom_histogram(aes(fill = Genotype))+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  labs(x = "Date of the experiment", y = "Number of observations", title = "Number of observations per day")+
  scale_y_continuous(breaks = seq(from = 0, to = 325, by = 25))+
  scale_x_date(date_breaks = "2 days", date_labels = "%b %d")+
  geom_vline(xintercept = 10)
  
# With automatic date etiquettes
ggplot(data, mapping = (aes(x = Date)))+
  geom_histogram(bins=30, aes(fill = Genotype))+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  labs(x = "Date of the experiment", y = "Number of observations", title = "Number of observations per day")+
  scale_y_continuous(breaks = seq(from = 0, to = 325, by = 25))+
  scale_x_date(labels = scales::label_date_short())

################################################################################
# Scatter plots by Genotype and by Soil factors ################################
################################################################################

# Scatter plot of the genotype factor by timestamp
ggplot(data, aes(x = Timestamp, y = Height_mm))+
  geom_point(aes(colour=Soil))+
  scale_y_continuous(breaks = seq(from = 0, to = 700, by = 100))+
  labs(x = "Date", y = "Height [mm]", title = "Evolution of the height for all genotypes")+
  facet_wrap(~Genotype)

# Scatter plot of the soil factor by timestamp
ggplot(data, aes(x = Timestamp, y = Height_mm))+
  geom_point(aes(colour=Genotype))+
  facet_wrap(~Soil)

# Scatter plot of the genotype factor by date with geom_smooth
ggplot(data, aes(x = Date, y = Height_mm))+
  geom_point(aes(colour=Soil))+
  scale_y_continuous(breaks = seq(from = 0, to = 700, by = 100))+
  scale_x_date(labels = scales::label_date_short())+
  geom_smooth(aes(colour = Soil))+
  facet_wrap(~Genotype)

# Scatter plot of the soil factor by date
ggplot(data, aes(x = Date, y = Height_mm)) +
  geom_point(aes(colour = Genotype)) +  
  scale_y_continuous(breaks = seq(from = 0, to = 700, by = 100)) +
  scale_x_date(labels = scales::label_date_short()) +
  facet_wrap(~Soil) +
  labs(
    title = "Plant Height Over Time by Genotype and Soil Type",
    x = "Date",
    y = "Height (mm)",
    colour = "Genotype"
  ) 

# Scatter plot of the plant number 1 
ggplot(data[data$Number==1, ], aes(x = Timestamp, y = Height_mm))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "Height [mm]", title = "Evolution of the height of plant number 1")




# Scatter plot of the soil factor by date with viridis palette
ggplot(data, aes(x = Date, y = Height_mm))+
  geom_point(aes(colour=Genotype))+  
  scale_y_continuous(breaks = seq(from = 0, to = 700, by = 100))+
  scale_x_date(labels = scales::label_date_short())+
  scale_color_viridis(discrete=TRUE, option = "D")+
  facet_wrap(~Soil)


# Scatter plot of the soil factor by date for genotype EPPN_T
data_EPPN_T <- data[data$Genotype == "EPPN_T", ]
ggplot(data_EPPN_T, aes(x = Date, y = Height_mm))+
  geom_point(aes(colour=Replication))+
  geom_line(aes(colour=Replication))+
  scale_y_continuous(breaks = seq(from = 0, to = 700, by = 100))+
  scale_x_date(labels = scales::label_date_short())+
  labs(x = "Date of the experiment", y = "Height [mm]", title = "Evolution of the height for genotype EPPN_T")+
  facet_wrap(~Soil)




################################################################################
################################################################################
# FOR BIOMASS ##################################################################



ggplot(data_bio, aes(y = DW_shoot_g))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = "Boxplot of the dry weight of the roots in g per genotype")

ggplot(data_bio, aes(y = DW_root_g))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = "Boxplot of the dry weight of the shoots in g per genotype")  



data_bio$DW_plant_g <- data_bio$DW_shoot_g+data_bio$DW_root_g

ggplot(data_bio, aes(y = DW_plant_g))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = "Boxplot of the dry weight of the plant in g per genotype")



ggplot(data_bio, aes(y = DW_plant_g, x = Genotype)) +
  geom_violin(aes(fill = Genotype), color = "white", alpha = 0.3) +
  geom_sina(size = 1, aes(color = Genotype)) +
  labs(
    title = "Dry Weight of the Plant by Genotype",
    x = "Genotype",
    y = "Dry Weight (g)"
  ) +
  theme(legend.position = "none") 


# Violin and sina plot of the dry weight of the plant by genotype and soil
ggplot(data_bio, aes(y = DW_plant_g, x = Maize_cultivar))+
  geom_violin(aes(fill = Maize_cultivar), color = "white", alpha = 0.3)+
  geom_sina(size=1, aes(color = Maize_cultivar))+
  labs(title = "Violin and sina plot of the dry weight of the plant by genotype and soil")      



################################################################################
################################################################################
# Correlation plots

# TEST 1
pairs(data_bio[,2:5],col=data_bio$Genotype, oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(data_bio$Genotype)),fill=c(1,2,3,4,5,6,7,8,9))


# Corr plot in general

var <- data.frame(data_bio$DW_shoot_g, data_bio$DW_root_g)
group <- data_bio$Genotype


pairs(var)
# equivalent de 
pairs(~ DW_shoot_g + DW_root_g, data = data_bio)

pairs(var,                     # Data frame of variables
      labels = colnames(var),  # Variable names
      pch = 21,                 # Pch symbol
      bg = rainbow(9)[group],  # Background color of the symbol (pch 21 to 25)
      col = rainbow(9)[group], # Border color of the symbol
      main = "Biomass dataset",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)+
  legend(0.85,0.6, as.vector(unique(data_bio$Genotype)),fill=c(1,2,3,4,5,6,7,8,9))
# Font style of the diagonal text











