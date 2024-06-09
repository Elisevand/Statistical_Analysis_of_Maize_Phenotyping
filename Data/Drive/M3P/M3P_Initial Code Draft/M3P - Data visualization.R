##### M3P ########
library(ggplot2)
library(dplyr)
library(cowplot)
library(stringr)

data <- data_M3P

#data <- read.table("img_extracted.txt", header=TRUE)
#data <- data %>%
#  rename(Estimated_Plant_LeafArea_mm2=Estimated_PlantLeafArea,
#         Estimated_Plant_Biomass_g_FW=Estimated_PlantBiomass,
#         Estimated_Plant_Height_mm=Estimated_PlantHeight) #FW signifie Fresh Weight

# Grouper les données par Unit.ID
data_grouped <- data %>%
  group_by(Unit.ID)



# Detection des outliers ####

ecart_type_limite <-2
data_grouped_filtre <- data_grouped %>% 
  filter(abs(Estimated_Plant_LeafArea_mm2) < ecart_type_limite * sd(Estimated_Plant_LeafArea_mm2))
data_grouped_filtre <- data_grouped %>%
  filter(abs(Estimated_Plant_Height_mm) < ecart_type_limite * sd(Estimated_Plant_Height_mm))
data_grouped_filtre <- data_grouped %>%
  filter(abs(Estimated_Plant_Biomass_g_FW) < ecart_type_limite * sd(Estimated_Plant_Biomass_g_FW))
























# Variables pour les graphiques
plots_list <- list() # Liste vide pour stocker les graphiques
plot_count <- 0  # Compteur du nombre de graphiques
plot_number <- 5 # Nombre de graphiques 


#####################################################################################
########################## GRAPHIQUE AVEC LES 3 VARIABLES ###########################
#####################################################################################

# Boucle pour les données groupées
for (group in unique(data_grouped$Unit.ID)) {
  if (plot_count >= plot_number) {
    break  # Sortir de la boucle une fois que le nombre de graphiques est atteint
  }
  
  # Données par groupe actuel
  data_unit <- data_grouped %>% filter(Unit.ID == group)
  
  # La colonne timestamp doit être sous le format année-mois-jour-heure-minute-seconde
  data_unit$Timestamp <- as.POSIXct(data_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  # Graphique
  g <- ggplot(data_unit, aes(x = Timestamp)) +
    geom_line(aes(y = Estimated_Plant_LeafArea_mm2, color = "Estimated Plant Leaf Area")) +
    geom_line(aes(y = Estimated_Plant_Biomass_g_FW, color = "Estimated Plant Biomass")) +
    geom_line(aes(y = Estimated_Plant_Height_mm, color = "Estimated Plant Height")) +
    labs(x = "Timestamp", y = "Values", color = "Variables") +
    scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue", "Estimated Plant Biomass" = "red", "Estimated Plant Height" = "green")) +
    facet_wrap(~Unit.ID, ncol = 2) +
    theme_minimal()
  
  # Graphique ajouté dans la liste
  plots_list[[as.character(group)]] <- g
  plot_count <- plot_count + 1  # Incrémenter le compteur
}

lapply(plots_list, print)

#####################################################################################
####################### 3 GRAPHIQUES AVEC CHAQUE VARIABLE ###########################
#####################################################################################

# Boucle à travers les données groupées
for (group in unique(data_grouped$Unit.ID)) {
  if (plot_count >= plot_number) {
    break 
  }
  
  data_unit <- data_grouped %>% filter(Unit.ID == group)
  data_unit$Timestamp <- as.POSIXct(data_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  # Graphique
  g1 <- ggplot(data_unit, aes(x = Timestamp, y = Estimated_Plant_LeafArea_mm2)) +
    geom_line() +
    labs(x = "Timestamp", y = "Estimated Plant Leaf Area", title = paste("Unit.ID", group))
  
  g2 <- ggplot(data_unit, aes(x = Timestamp, y = Estimated_Plant_Biomass_g_FW)) +
    geom_line() +
    labs(x = "Timestamp", y = "Estimated Plant Biomass", title = paste("Unit.ID", group))
  
  g3 <- ggplot(data_unit, aes(x = Timestamp, y = Estimated_Plant_Height_mm)) +
    geom_line() +
    labs(x = "Timestamp", y = "Estimated Plant Height", title = paste("Unit.ID", group))
  
  plots_list[[as.character(group)]] <- list(g1, g2, g3)
  plot_count <- plot_count + 1
}

# Visualisation des graphiques
for (i in 1:plot_number) {
  grid.arrange(grobs = plots_list[[i]])
}


#####################################################################################
################# GRAPHIQUE PAR GENOTYPE POUR TOUS LES GENOTYPES ####################
#####################################################################################

# On rajoute une colonne génotype grâce à Unit.ID où se trouve le génotype
dataGenotype <- data %>%
  mutate(Genotype = str_extract(Unit.ID, "([A-Z0-9]+_[A-Z0-9]+)"))

# Grouper les données
data_grouped_Genotype <- dataGenotype %>%
  group_by(Genotype)

plots_list <- list()

# Boucle 
for (genotype in unique(data_grouped_Genotype$Genotype)) {
  # Données pour le génotype actuel
  data_genotype <- data_grouped_Genotype %>% filter(Genotype == genotype)
  
  # Graphique
  g <- ggplot(data_genotype, aes(x = Timestamp, y = Estimated_Plant_LeafArea_mm2, color = "Estimated Plant Leaf Area")) +
    geom_point() +
    geom_line() +
    labs(x = "Timestamp", y = "Estimated Plant Leaf Area", title = genotype) +
    scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue")) +
    theme_minimal()
  
  g <- g + geom_point(aes(y = Estimated_Plant_Biomass_g_FW, color = "Estimated Plant Biomass")) +
    geom_line(aes(y = Estimated_Plant_Biomass_g_FW, color = "Estimated Plant Biomass")) +
    scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue", "Estimated Plant Biomass" = "red"))
  
  g <- g + geom_point(aes(y = Estimated_Plant_Height_mm, color = "Estimated Plant Height")) +
    geom_line(aes(y = Estimated_Plant_Height_mm, color = "Estimated Plant Height")) +
    scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue", "Estimated Plant Biomass" = "red", "Estimated Plant Height" = "green"))
  
  
  plots_list[[genotype]] <- g
}

# Visualisation des graphiques
lapply(plots_list, print)


#####################################################################################
################# GRAPHIQUE PAR GENOTYPE POUR LE PREMIER GENOTYPE ###################
#####################################################################################

genotype1_data <- dataGenotype %>%
  filter(Genotype == "EPPN7_L")

g <- ggplot(genotype1_data, aes(x = Timestamp)) +
  geom_point(aes(y = Estimated_Plant_LeafArea_mm2, color = "Estimated Plant Leaf Area")) +
  geom_line(aes(y = Estimated_Plant_LeafArea_mm2, color = "Estimated Plant Leaf Area")) +
  labs(x = "Timestamp", y = "Estimated Plant Leaf Area", title = "EPPN7_L") +
  scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue")) +
  theme_minimal()

g <- g + geom_point(aes(y = Estimated_Plant_Biomass_g_FW, color = "Estimated Plant Biomass")) +
  geom_line(aes(y = Estimated_Plant_Biomass_g_FW, color = "Estimated Plant Biomass")) +
  scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue", "Estimated Plant Biomass" = "red"))

g <- g + geom_point(aes(y = Estimated_Plant_Height_mm, color = "Estimated Plant Height")) +
  geom_line(aes(y = Estimated_Plant_Height_mm, color = "Estimated Plant Height")) +
  scale_color_manual(values = c("Estimated Plant Leaf Area" = "blue", "Estimated Plant Biomass" = "red", "Estimated Plant Height" = "green"))

print(g)

liste_genotypes <- sort(unique(data_grouped_Genotype$Genotype))

library(VIM)
matrixplot(data,cex.axis=0.6)




