plot_global_aqi_distribution <- function(dataset) {
  library(ggplot2)
  
  # Plotting the global distribution of AQI values for each pollutant
  p1 <- ggplot(dataset, aes(x = `AQI Value`)) +
    geom_histogram(aes(fill = `AQI Category`), bins = 30, alpha = 0.6) +
    facet_wrap(~`AQI Category`, scales = "free_y") +
    labs(title = "Global Distribution of AQI Values by Category",
         x = "AQI Value",
         y = "Count",
         fill = "AQI Category") +
    theme_minimal()
  
  print(p1)
}
