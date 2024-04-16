plot_aqi_vs_gdp <- function(dataset) {
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(ggplot2)
  
  # Fetching income level data
  income_data <- WDI::WDI(indicator = "NY.GNP.PCAP.CD", country = "all", extra = TRUE, start = 2020, end = 2020)
  
  # Merging income level data with AQI dataset
  dataset$Country_ISO <- countrycode::countrycode(dataset$Country, "country.name", "iso2c")
  merged_data <- merge(dataset, income_data[,c("iso2c", "income", "NY.GNP.PCAP.CD")], by.x = "Country_ISO", by.y = "iso2c", all.x = TRUE)
  
  # Simplifying the dataset for analysis
  analysis_data <- merged_data %>%
    dplyr::group_by(income) %>%
    dplyr::summarise(Average_AQI = mean(`AQI Value`, na.rm = TRUE),
                     GDP_per_capita = mean(NY.GNP.PCAP.CD, na.rm = TRUE))
  
  # Plotting the relationship
  ggplot(analysis_data %>% filter(!is.na(income)), aes(x = GDP_per_capita, y = Average_AQI, color = income)) +
    geom_point(size = 5) +
    geom_text(aes(label = income), vjust = -1) +
    scale_x_log10() +
    labs(title = "Average AQI Value vs. GDP per Capita by Income Level",
         x = "GDP per Capita (log scale)",
         y = "Average AQI Value",
         color = "Income Level",
         caption = "Data Source: World Bank, Global Air Pollution Dataset") +
    theme_minimal()
}
