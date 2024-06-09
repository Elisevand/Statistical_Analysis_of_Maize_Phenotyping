# FONCTION RETRAIT DE COORDONNEES SPATIALES DU ISATAB

coordinates_isaTAB <- function(isaTAB){
  coordinates <- isaTAB$`Characteristics[Spatial Distribution]`
  nrow <- sapply(strsplit(as.character(coordinates), split =";"), '[', 1)
  nrow <- sapply(strsplit(as.character(nrow), split="="), '[', 2)
  ncol <- sapply(strsplit(as.character(coordinates), split =";"), '[', 2)
  ncol <- sapply(strsplit(as.character(ncol), split ="="), '[', 2)
  rep <- sapply(strsplit(as.character(coordinates), split =";"), '[', 3)
  rep <- sapply(strsplit(as.character(rep), split ="="), '[', 2)
  Sample.Name <- isaTAB$`Sample Name`
  position <- data.frame(cbind(Sample.Name, nrow, ncol, rep))
  return(position)
}

###### Example
#isaTAB <- read.csv("ISA_EPPN2020_SPPU - s_exp.csv")
#isaTAB <- read_excel("ISA_EPPN2020_ALSIA.xlsx", sheet = "s_exp")

#position <- coordinates_isaTAB(isaTAB)

###### 


################################################################################
# NORMALITY TEST FUNCTION
################################################################################
# DENSITY HISTOGRAM AND SHAPIRO-WILK NORMALITY HYPOTHESIS
normality_test_histogram <- function(data) {
  numeric_vars <- Filter(is.numeric, data)
  
  normality_results <- list()
  
  for (variable in names(numeric_vars)) {
    # 
    test_result <- shapiro.test(numeric_vars[[variable]])
    normality_results[[variable]] <- test_result
    
    hist_title <- paste("Histogram of", variable, "\nNormality Test: p =", round(test_result$p.value, 4))
    hist <- ggplot(data, aes_string(x = variable)) +
      geom_histogram(aes(y = ..density..), bins = 20, color = "skyblue", fill = "skyblue", alpha = 0.5) +
      geom_density(color = "blue", size = 0.7, linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = mean(numeric_vars[[variable]]), sd = sd(numeric_vars[[variable]])), color = "red", size = 0.7) +
      labs(title = hist_title, x = variable, y = "Density") +
      theme_minimal()
    qq_title <- paste("QQ Plot of", variable)
    qq <- qqPlot(numeric_vars[[variable]], main = qq_title, ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
    print(hist)
    print(qq)
  }
  return(normality_results)
}




















################################################################################
# DATA VISUALIZATION FUNCTIONS
################################################################################


# FONCTION CORRELATION SPATIALE
calculate_correlation_plot <- function(data, var1, var2) {
  correlation <- cor(data[[var1]], data[[var2]], use = "pairwise.complete.obs")
  
  plot <- ggplot(data, aes_string(x = var1, y = var2)) +
    geom_point() +
    labs(title = paste("Correlation Plot between", var1, "and", var2),
         x = var1,
         y = var2) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Add linear regression line
    geom_text(x = min(data[[var1]]), y = max(data[[var2]]), 
              label = paste("Correlation:", round(correlation, 2)), hjust = 0, vjust = 1)
  
  print(plot)
}

# FONCTION DE DETECTION DES OUTLIERS
# On detecte et remplace les outliers par NA
detect_replace_outliers_by_genotype <- function(data) {
  for (genotype in unique(data$Genotype)) {
    subset_data <- data[data$Genotype == genotype, ]
    for (variable in variables) {
      var <- subset_data[[variable]]
      
      Q <- quantile(var, probs = c(0.25, 0.75), na.rm = TRUE)
      
      IQR <- Q[2] - Q[1]
      
      lower_bound <- Q[1] - 1.5 * IQR
      upper_bound <- Q[2] + 1.5 * IQR
      
      outliers <- var < lower_bound | var > upper_bound
      
      var[outliers] <- NA
      
      subset_data[[variable]] <- var
    }
    endpoint_clean[data$Genotype == genotype, variables] <- subset_data[variables]
  }
  return(endpoint_clean)
}


# FONCTION POUR LES BOXPLOTS EN FONCTION DU GENOTYPE OU PLANT TYPE
create_boxplots <- function(data, variables, grouping_var) {
  boxplots <- list()
  
  for (var in variables) {
    p <- ggplot(data, aes_string(y = var, x = grouping_var)) +
      geom_boxplot(aes_string(fill = grouping_var)) +
      labs(title = var, y = var) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), 
            legend.position = "right") +   
      guides(fill = guide_legend(title = grouping_var))
    
    boxplots[[var]] <- p
  }
  combined_plot <- wrap_plots(boxplots) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "right")  
  print(combined_plot)
}


# FONCTION POUR LES VIOLIN ET SINA PLOT EN FONCTION DU GENOTYPE OU PLANT TYPE
create_violin_plots <- function(data, variables, grouping_var) {
  violin_plots <- list()
  
  for (var in variables) {
    p <- ggplot(data, aes_string(y = var, x = grouping_var)) +
      geom_violin(aes_string(fill = grouping_var), color = "white", alpha = 0.4) +
      geom_sina(size = 1, aes_string(color = grouping_var)) +
      theme(legend.position = "none") +
      labs(title = var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  
            panel.grid.major.x = element_line(color = "lightgray", size = 0.5),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank())
    
    violin_plots[[var]] <- p
  }
  combined_plot <- wrap_plots(violin_plots) + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  print(combined_plot)
}

# FONCTION POUR FAIRE UN SCATTER PLOT DUN GENOTYPE SPECIFIQUE
plot_scatter_by_genotype <- function(data, variables, genotype) {
  for (var in variables) {
    p <- ggplot(data[data$Genotype == genotype, ], aes_string(x = "Timestamp", y = var, group = "Replication", color = "factor(Replication)")) +
      geom_point() +
      geom_line() +
      labs(title = paste("Scatterplot of", var, "for Genotype", genotype),
           x = "Time", y = var, color = "Replication") +
      theme(legend.position = "bottom")
    print(p)
  }
}


# FONCITON POUR FAIRE UN SCATTER PLOT DES TIMESERIES AVEC UN GEOM SMOOTH
plot_scatter_with_smooth <- function(data, variables) {
  for (var in variables) {
    p1 <- ggplot(data, aes_string(x = "Timestamp", y = var, group = "Unit.ID", color = "factor(Plant_type)")) +
      geom_point() +
      geom_line() +
      labs(title = paste("Scatterplot of", var, "by Plant type"),
           x = "Time", y = var, color = "Plant_type") +
      theme(legend.position = "bottom")
    
    p2 <- ggplot(data, aes_string(x = "Timestamp", y = var, group = "Plant_type", color = "factor(Plant_type)")) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Smooth line of", var, "by Plant type"),
           x = "Time", y = var, color = "Plant_type") +
      theme(legend.position = "bottom")
    
    print(p1)
    print(p2)
  }
}

# FONCTION POUR FAIRE UN SCATTERPLOT DES TIMESERIES AVEC UN GEOM SMOOTH ET AVEC TRAITTEMENT EAU
plot_scatter_with_smooth_water<- function(data, variables) {
  for (var in variables) {
    # Scatterplot with individual plant lines, facetted by Soil
    p1 <- ggplot(data, aes_string(x = "Timestamp", y = var, group = "Unit.ID", color = "factor(Plant_type)")) +
      geom_point() +
      geom_line() +
      labs(title = paste("Scatterplot of", var, "by Plant type and Soil"),
           x = "Time", y = var, color = "Plant_type") +
      theme(legend.position = "bottom") +
      facet_wrap(~Soil)
    
    # Smooth line by Plant type, facetted by Soil
    p2 <- ggplot(data, aes_string(x = "Timestamp", y = var, group = "Plant_type", color = "factor(Plant_type)")) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Smooth line of", var, "by Plant type and Soil"),
           x = "Time", y = var, color = "Plant_type") +
      theme(legend.position = "bottom") +
      facet_wrap(~Soil)
    
    print(p1)
    print(p2)
  }
}

