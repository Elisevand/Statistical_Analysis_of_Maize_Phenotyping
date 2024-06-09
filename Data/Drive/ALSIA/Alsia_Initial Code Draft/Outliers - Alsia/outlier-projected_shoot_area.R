rm( list = ls())
library(statgenHTP)
library( ggplot2)
library(gridExtra)
library(dplyr)
WDIR <- getwd()
subdir <- 'out/miappe'
datadir <- sprintf( '%s/%s', WDIR, subdir)
alsiaData <- readr::read_tsv( sprintf( '%s/extracted_imaging.txt', datadir))

colnames(alsiaData)[ grepl('height_above', colnames(alsiaData))] <- "height_above_reference"

final_traitnames <- c( "projected_shoot_area", "solidity-T", "solidity-S", 
                       "height_above_reference", "wue", "area-T", "area-S", 
                       "convex_hull_area-T", "convex_hull_area-S")

# Formatting the data = create a list with one data frame per time point
phenoTPals <- createTimePoints(dat = alsiaData,
                               experimentName = "EPPN2020_alsia",
                               genotype = "genotype",
                               timePoint = "date",
                               plotId = "potId",
                               rowNum = "Row",
                               colNum = "Column")

summary(phenoTPals)

tname <- final_traitnames[1]
countValid(phenoTPals, tname)
getTimePoints(phenoTPals)

num_timepoints <- getTimePoints(phenoTPals)[1,1]
# Check the layout at one time point
plot(phenoTPals, 
     plotType = "layout",
     timePoints = num_timepoints,
     highlight =  c("EPPN20_T", "EPPN1_H","EPPN1_L"))

# Check the heatmap of the raw data at one time point
plot(phenoTPals,
     plotType = "layout",
     timePoints = num_timepoints,
     traits = tname)


# Check some time courses of raw data
plot(phenoTPals, 
     traits = tname,
     plotType = "raw")

# Boxplot
plot(phenoTPals, 
     plotType = "box",
     traits = tname)


# First select a subset of plants to adjust the settings
plantSel <- c("c14r17","c22r15","c22r18","c23r18","c18r14","c20r15","c8r16")

# Then run on the subset
cutoffA <- 1 
ci <- c(5)[cutoffA] # confidence intervale
nn <- c(0.5)[cutoffA] # nearest neighbour (0 - 1 greater number smoother curve)
ce <- c(FALSE)[cutoffA]
resuSingAlstest <- detectSingleOut(TP = phenoTPals,
                                   trait = tname,
                                   plotIds = plantSel,
                                   confIntSize = ci,
                                   nnLocfit = nn,
                                   checkEdges = ce)

plot(resuSingAlstest,
     plotIds = plantSel[1],
     outOnly = FALSE)

# Visualize the selected plants
plot(resuSingAlstest,
     outOnly = FALSE)

###########


# run on all
# *********** first table **************
resuSingAls <- detectSingleOut(TP = phenoTPals,
                               trait = tname,
                               confIntSize = ci,
                               nnLocfit = nn,
                               checkEdges = ce)

## How many outliers?
resuSingAls[resuSingAls$outlier == 1,]


## How many outliers per date?
with(resuSingAls[resuSingAls$outlier == 1,], table(timePoint))

phenoTPalsOut <- removeSingleOut(phenoTPals,
                                 resuSingAls)
readr::write_tsv( resuSingAls, sprintf( "%s/singlePointOutliers_%s.tsv", datadir, tname))



## Fit a model for all time points with no extra fixed effects.
modPhenoSp <- fitModels(TP = phenoTPals,
                        trait = tname,
                        geno.decomp = "type")


plot(modPhenoSp,
     timePoints = 6, 
     plotType = "spatial",
     spaTrend = "percentage")

#
plot(modPhenoSp,
     timePoints = 11, 
     plotType = "spatial",
     spaTrend = "percentage")

plot(modPhenoSp,
     timePoints = 1, 
     plotType = "spatial",
     spaTrend = "percentage")


plot(modPhenoSp,
     plotType = "timeLapse",
     spaTrend = "percentage",
     outFile = sprintf( "%s/out/statgen/test.gif", WDIR))
########################

plot(modPhenoSp, 
     plotType = "herit",
     yLim = c(0.4,1))

#|—— Observing the effective dimensions ——|
  
plot(modPhenoSp,
     plotType = "effDim",
     EDType = "ratio",
     yLim = c(0,1))

#|—— Observing the spatially corrected values ——|
  
plot(modPhenoSp, 
     plotType = "corrPred")


####################

spatCorrectedAlsia <- getCorrected(modPhenoSp)


#|—— Fit the spline for every plant and inspect the objects ——|
severity <- 1
knots <- c( 30)[severity]
mintimepoints <- c( 9)[severity]
# No need to select a subset of genotypes
fit.splineAlsia <- fitSpline(inDat = spatCorrectedAlsia, 
                             trait = paste0(tname, "_corr"),
                             knots = knots,
                             minNoTP = mintimepoints)

# Extracting the tables of predicted values and P-spline coefficients
predDatAlsia <- fit.splineAlsia$predDat
coefDatAlsia <- fit.splineAlsia$coefDat

#The object fit.spline contains the P-spline model coefficients (coefDat) and the predicted value (pred.value in predDat), i.e the values predicted using the P-spline model coefficients. Predictions are made on a denser grid of time points: the time points for prediction are calculated as the smallest gap between two time points divided by 9, so dividing the smallest gap in 10 segments. The object fit.spline also contains the first and second derivatives (deriv and deriv2 in predDat).

# plantSel <- alsiaData[ grepl( 'EPPN20_T', genotype), unique( potId)]

plot(fit.splineAlsia,
     plotIds = plantSel,
     plotType =  "predictions")

plot(fit.splineAlsia,
     plotIds = plantSel,
     plotType =  "derivatives")

plot(fit.splineAlsia,
     plotIds = plantSel,
     plotType =  "derivatives2")


#### Time series outliers
# *************** second Table ***************
cutoff <- 1
thrCor <- c(0.9)[cutoff] # correlation threshold
thrPca <- c(10)[cutoff] # pca angle threshold
thrSlope <- c(0.7)[cutoff] # slope threshold
outAlsia <- detectSerieOut(corrDat = spatCorrectedAlsia,
                           predDat = predDatAlsia,
                           coefDat = coefDatAlsia,
                           trait = paste0( tname, "_corr"),
                           thrCor = thrCor,
                           thrPca = thrPca,
                           thrSlope = thrSlope,
                           geno.decomp = "geno.decomp")

plot(outAlsia, genotypes = levels(factor(outAlsia$genotype)))




# Manual check and override decision (when necessary):
plotToKeep <- c("c6r14", "c5r14", "c6r18")
outAlsiaFinal <- outAlsia[!(outAlsia$plotId %in% plotToKeep),]
spatCorrectedAlsiaOut <- spatCorrectedAlsia

# *************** second Table ***************
spatCorrectedAlsiaOut <- removeSerieOut(dat = spatCorrectedAlsia,
                                        serieOut = outAlsia)
## remove all point/plant with NA in the LeafArea_corr 
# WARNINGS: be carefull when removing whole plant from the data !
colNum <- which( names( spatCorrectedAlsiaOut) %in% paste0( tname, "_corr"))
spatCorrectedAlsiaOut <- 
  spatCorrectedAlsiaOut[!is.na(spatCorrectedAlsiaOut[,colNum]),]
spatCorrectedAlsiaOut <- droplevels(spatCorrectedAlsiaOut)

readr::write_tsv( spatCorrectedAlsiaOut, sprintf( "%s/timeSeriesOutliers_%s.tsv", datadir, tname))


#######################

phenoTPalsBIS <- createTimePoints(dat = spatCorrectedAlsiaOut,
                                  experimentName = "EPPN2020_alsia_clean",
                                  genotype = "genotype",
                                  timePoint = "timePoint",
                                  plotId = "plotId",
                                  rowNum = "rowId",
                                  colNum = "colId")

## Correct for spatial
modPhenoSpBIS <- fitModels(TP = phenoTPalsBIS,
                           trait = paste0( tname),
                           geno.decomp = c("geno.decomp"))
## Extract corrected values
spatCorrectedAlsiaBIS <- getCorrected(modPhenoSpBIS)

#Check the values before and after:
  
h21<-plot(modPhenoSp, output = FALSE,
          plotType = "herit",
          yLim = c(0.4,1))

h22<-plot(modPhenoSpBIS, output = FALSE,
          plotType = "herit",
          yLim = c(0.4,1))

grid.arrange(h21, h22,  nrow = 1) 

#
ed2<-plot(modPhenoSpBIS,output = FALSE,
          plotType = "effDim",
          EDType = "ratio",
          yLim = c(0,1))

ed1<-plot(modPhenoSp,output = FALSE,
          plotType = "effDim",
          EDType = "ratio",
          yLim = c(0,1))

grid.arrange(ed1, ed2,  nrow = 1) 

spatCorr <- 
  inner_join(spatCorrectedAlsia[,c("timeNumber","plotId",paste0( tname, "_corr"))], 
             spatCorrectedAlsiaBIS[,c("timeNumber","plotId",paste0( tname, "_corr"))], 
             by = c("timeNumber","plotId") )


col1 <- paste0( tname, "_corr.x")
col2 <- paste0( tname, "_corr.y")
ggplot(spatCorr, aes(x = .data[[col1]], y = .data[[col2]])) +
  geom_point(size=3) +
  geom_abline(slope=1, intercept = 0,col="red",lty=2) +
  ylab("Outliers removed") + xlab("Outliers") + theme_light()



# fitSplie again

fit.splineAlsiaBIS <- fitSpline(inDat = spatCorrectedAlsiaBIS, 
                                trait = paste0( tname, "_corr"),
                                knots = 30,
                                minNoTP = 9)


param1bis <- 
  estimateSplineParameters(HTPSpline = fit.splineAlsiaBIS,
                           estimate = "derivatives",
                           what = "max")
head(param1bis)

param1bis[,'genotype'] <- factor( param1bis[, 'genotype'], 
                              levels = c( "EPPN20_T", "EPPN1_L", "EPPN1_H", "EPPN2_L", 
                                          "EPPN2_H", "EPPN3_L", "EPPN3_H", "EPPN4_L", "EPPN4_H"))


# Visualize the variability 
ggplot(param1bis,
       aes(x = genotype, y = max_derivatives)) + 
  geom_boxplot(na.rm = TRUE) +
  ylab("Max growth rate") +
  theme_classic() 


