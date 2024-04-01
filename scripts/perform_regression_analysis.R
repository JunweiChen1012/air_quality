perform_regression_analysis <- function(dataset) {
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(broom)
  library(knitr)
  library(kableExtra)
  
  # Fetching GDP per capita data for all countries
  gdp_data <- WDI(indicator = "NY.GNP.PCAP.CD", country = "all", start = 2020, end = 2020, extra = TRUE)
  
  # Merging the GDP data with the AQI dataset on country ISO codes
  dataset$Country_ISO <- countrycode(dataset$Country, "country.name", "iso3c")
  merged_data <- merge(dataset, gdp_data, by.x = "Country_ISO", by.y = "iso3c")
  
  # Performing the regression analysis using individual AQI values and GDP per capita
  lm_model <- lm(`AQI Value` ~ NY.GNP.PCAP.CD, data = merged_data)

  saveRDS(lm_model, file = "lm_model_AQI_vs_GDP.rds")
  
  # Tidying the linear model for a clean summary
  tidy_lm_model <- tidy(lm_model)
  
  # Creating the table with knitr::kable() and styling it with kableExtra
  kable(tidy_lm_model, format = "simple", caption = "Regression Analysis Summary: AQI vs. GDP per Capita") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
}
