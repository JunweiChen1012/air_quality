plot_comparative_pollutant_levels <- function(dataset) {
  library(tidyverse)
  library(ggplot2)
  
  long_dataset <- dataset %>%
    pivot_longer(cols = c(`CO AQI Value`, `Ozone AQI Value`, `NO2 AQI Value`, `PM2.5 AQI Value`),
                 names_to = "Pollutant", values_to = "Value")
  
  # Correctly specifying levels for the 'Pollutant' factor based on unique values in the 'Pollutant' column
  long_dataset$Pollutant <- factor(long_dataset$Pollutant, levels = unique(long_dataset$Pollutant))
  
  # Plotting the comparative analysis of pollutant levels
  p <- ggplot(long_dataset, aes(x = Pollutant, y = Value, fill = Pollutant)) +
    geom_boxplot() +
    labs(title = "Comparative Analysis of Pollutant Levels",
         x = "Pollutant",
         y = "AQI Value",
         fill = "Pollutant",
         caption = "Data Source: Global Air Pollution Dataset") +
    theme_minimal() +
    theme(text = element_text(size = 14), # General text size for the plot, adjusts all text elements
          axis.title = element_text(size = 16), # Axis titles
          plot.title = element_text(size = 18, face = "bold"), # Plot title
          legend.title = element_text(size = 16), # Legend title
          legend.text = element_text(size = 14)) # Legend items
  
  print(p)
}
