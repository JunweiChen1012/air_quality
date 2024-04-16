plot_histogram_pollutants <- function(dataset) {
  library(tidyverse)
  library(ggplot2)
  
  dataset_long <- dataset %>%
    pivot_longer(cols = c(`CO AQI Value`, `Ozone AQI Value`, `NO2 AQI Value`, `PM2.5 AQI Value`),
                 names_to = "Pollutant", values_to = "Value")
  
  # Plotting histograms for each pollutant using the corrected column names
  p <- ggplot(dataset_long, aes(x = `Value`, fill = Pollutant)) +
    geom_histogram(binwidth = 10, alpha = 0.6) +
    facet_wrap(~Pollutant, scales = "free_y") +
    labs(title = "Distribution of AQI Values for Each Pollutant",
         x = "AQI Value",
         y = "Frequency") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  print(p)
}
