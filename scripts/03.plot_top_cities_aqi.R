plot_top_cities_aqi <- function(dataset) {
  library(tidyverse)
  library(ggplot2)
  
  # Calculating average AQI values for each city
  avg_aqi_per_city <- dataset %>%
    group_by(City) %>%
    summarise(Across_All_Pollutants_Avg_AQI = mean(c(`CO AQI Value`, `Ozone AQI Value`, `NO2 AQI Value`, `PM2.5 AQI Value`), na.rm = TRUE)) %>%
    arrange(desc(Across_All_Pollutants_Avg_AQI)) %>%
    top_n(20) # Adjust this number based on your preference
  
  # Plotting with improved clarity
  p <- ggplot(avg_aqi_per_city, aes(x = reorder(City, -Across_All_Pollutants_Avg_AQI), y = Across_All_Pollutants_Avg_AQI)) +
    geom_bar(stat = "identity", fill = "coral") +
    coord_flip() + # Flipping coordinates for better readability
    labs(title = "Top 20 Cities by Average AQI Value Across Pollutants",
         x = "City",
         y = "Average AQI Value",
         caption = "Data Source: Global Air Pollution Dataset") +
    theme_minimal()
  
  print(p)
}
