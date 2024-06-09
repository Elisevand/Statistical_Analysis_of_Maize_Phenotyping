# FONCTION RETRAIT DE COORDONNEES SPATIALES DU ISATAB

coordinates_isaTAB <- function(isaTAB){
  coordinates <- isaTAB$Characteristics.Spatial.Distribution.
  nrow <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 1)
  nrow <- sapply(strsplit(as.character(nrow), split="="), '[', 2)
  ncol <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 2)
  ncol <- sapply(strsplit(as.character(ncol), split ="="), '[', 2)
  rep <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 3)
  rep <- sapply(strsplit(as.character(rep), split ="="), '[', 2)
  position <- data.frame(cbind(nrow, ncol, rep))
  return(position)
}

###### Example
#isaTAB <- read.csv("ISA_EPPN2020_SPPU - s_exp.csv")
#position <- coordinates_isaTAB(isaTAB)

###### 