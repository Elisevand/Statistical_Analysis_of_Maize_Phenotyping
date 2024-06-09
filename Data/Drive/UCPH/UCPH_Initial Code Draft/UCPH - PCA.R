#### IMPORT DES DONNEES 

# Pour que ca marche comme ca tu dois mettre le csv dans le meme dossier que le .R
# Puis en haut dans la barre de commandes : Session  -> Set working directory -> To source file location
# Sinon tu peux utiliser le truc à droit d'importation du dataset

data <- read.csv("data_biomass.csv")

data$DW_shoot_g <- as.numeric(gsub(",", ".", data$DW_shoot_g))
data$DW_root_g <- as.numeric(gsub(",", ".", data$DW_root_g))
data$Number <- as.factor(data$Number)
data$Maize_cultivar <- as.factor(data$Maize_cultivar)
data$Soil <- as.factor(data$Soil)
data$Genotype <- as.factor(data$Genotype)
data$Replication <- as.factor(data$Replication)
str(data)

" Pour l'explication : 
Jsp pq y a une colonne X mais on s'en fout
Number c'est le numéro de la plante. Il y a donc 117 plantes en tout
Maize_cultivar c'est le cumul des facteurs Soil et Genotype
DW_shoot_g c'est le dry weight des feuilles + tige en g
DW_root_g c'est le dry weight des racines en g
Soil le facteur sol : y a deux types de sol 
Genotype c fassil c'est la même en fr. Au total 9 niveaux et EPPN_T c'est le témoin
Replication c'était pour voir combien de plantes avec le même génotype il y a dans le dataset
"

# Pour faire des graphiques souvent besoin de cette librairie
library(ggplot2)

#Exemple de graphe moche
ggplot(data, aes(y = DW_shoot_g))+
  geom_boxplot(aes(x = Genotype, fill = Genotype))+
  theme(legend.position = "none")+
  labs(title = "Boxplot of the dry weight of the roots in g per genotype")+
  scale_color_viridis(discrete=TRUE, option = "D")
  
# A toi de jouer 8)
# PCA

print("mercii")





