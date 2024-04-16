plot_regression_aqi_vs_gdp <- function(dataset) {
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(ggplot2)
  
  # Fetching GDP per capita data for all countries
  gdp_data <- WDI(indicator = "NY.GNP.PCAP.CD", country = "all", start = 2020, end = 2020, extra = TRUE)
  
  # Merging the GDP data with the AQI dataset on country ISO codes
  dataset$Country_ISO <- countrycode(dataset$Country, "country.name", "iso3c")
  merged_data <- merge(dataset, gdp_data, by.x = "Country_ISO", by.y = "iso3c")
  
  # Performing the regression analysis using individual AQI values and GDP per capita
  lm_model <- lm(`AQI Value` ~ NY.GNP.PCAP.CD, data = merged_data)
  
  # Plotting the regression analysis
  p <- ggplot(merged_data, aes(x = NY.GNP.PCAP.CD, y = `AQI Value`)) +
    geom_point(aes(color = income), size = 3, alpha = 0.6) + # Color by income level if applicable, with transparency for clarity
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    scale_x_log10() + # Use log scale for GDP if highly skewed
    labs(title = "Regression Analysis: Individual AQI Values vs. GDP per Capita",
         x = "GDP per Capita (log scale)",
         y = "AQI Value",
         color = "Income Level") +
    theme_minimal()
  
  print(p)
}
