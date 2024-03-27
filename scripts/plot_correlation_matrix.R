plot_correlation_matrix <- function(dataset) {
  library(corrplot)
  
  # Selecting AQI values for pollutants
  pollutants_aqi_values <- dataset %>%
    select(`CO AQI Value`, `Ozone AQI Value`, `NO2 AQI Value`, `PM2.5 AQI Value`)
  
  # Calculating the correlation matrix
  correlation_matrix <- cor(pollutants_aqi_values, use = "complete.obs")
  
  # Plotting the correlation matrix
  corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")
}
