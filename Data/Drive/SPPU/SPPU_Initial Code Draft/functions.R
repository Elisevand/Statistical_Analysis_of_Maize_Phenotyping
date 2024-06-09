################################################################################
# FUNCTIONS #
################################################################################

# FONCTION RETRAIT DE COORDONNEES SPATIALES DU ISATAB

coordinates_isaTAB <- function(isaTAB){
  coordinates <- isaTAB$Characteristics.Spatial.Distribution.
  nrow <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 1)
  nrow <- sapply(strsplit(as.character(nrow), split="="), '[', 2)
  ncol <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 2)
  ncol <- sapply(strsplit(as.character(ncol), split ="="), '[', 2)
  rep <- sapply(strsplit(as.character(coordinates), split ="; "), '[', 3)
  rep <- sapply(strsplit(as.character(rep), split ="="), '[', 2)
  Sample.Name <- isaTAB$Sample.Name
  position <- data.frame(cbind(Sample.Name, nrow, ncol, rep))
  return(position)
}

###### Example
#isaTAB <- read.csv("ISA_EPPN2020_SPPU - s_exp.csv")
#position <- coordinates_isaTAB(isaTAB)

###### 


# FONCTION POUR CHANGER LES , PAR DES .

switch_coma_by_period <- function(data){
  for (column in 1:ncol(data)){
    for (row in 1:nrow(data)){
      data[row, column] <- gsub(",", ".", data[row, column])
    }
  }
}


test_data <- data.frame(c(1,2,3, "7,5"), c("oui, peut etre", "non, peut etre", 
                                           "oui non", "non oui"))
colnames(test_data) <- c("valeurs", "reponses")

answ <- switch_coma_by_period(test_data)

test_data[2,2] <- gsub(",", ".", test_data[2, 2]) # ceci marche




