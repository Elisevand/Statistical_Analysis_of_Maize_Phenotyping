# Find the file
setwd("C:/Users/elise/Documents/Mémoire/Main/Data/")
Sys.setlocale("LC_TIME", "C")

# Import the libraries
library(tidyverse)
library(plan)

list.files()

data <- read.delim("ganttdiagram.txt")
str(data)

data <- data %>% 
  mutate(start = as.Date(start, format = "%Y-%m-%d"), 
         harvest = as.Date(harvest, format = "%Y-%m-%d"))
data <- data %>% arrange(start)

data$platform <- factor(data$platform, levels = unique(data$platform))

ggplot(data, aes(x = start, xend = harvest, y = fct_rev(platform), yend = platform)) +
  geom_segment(size = 10, color = "darkblue") +
  labs(title = "Gantt chart of platform timelines", x = "Date", y = "Platform") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +  # Ajouter une échelle de date continue
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
