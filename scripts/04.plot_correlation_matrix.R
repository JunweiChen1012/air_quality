plot_correlation_matrix <- function(dataset) {
  library(corrplot)
  
  # Selecting AQI values for pollutants
  pollutants_aqi_values <- dataset %>%
    select(`CO AQI Value`, `Ozone AQI Value`, `NO2 AQI Value`, `PM2.5 AQI Value`)
  
  # Calculating the correlation matrix
  correlation_matrix <- cor(pollutants_aqi_values, use = "complete.obs")
  
  # Plotting the correlation matrix
  corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black",
           tl.cex = 0.8, # Adjust text size of labels on correlation plot
           cl.pos = "r", # Position of the color legend (right side by default)
           cl.cex = 0.8, # Adjust the size of the legend text
           cl.lim = c(-1, 1)) # Adjust this if needed to set limits for color scale
}
