load_data <- function(filepath) {
  # Load necessary libraries
  library(tidyverse)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(corrplot)
  library(dplyr)
  library(tidyr)
  library(WDI)
  library(countrycode)
  library(broom)
  
  # Load the dataset
  dataset <- read_csv(filepath)
  
  # Clean the dataset
  dataset <- dataset %>% 
    drop_na() %>% 
    distinct()
  
  return(dataset)
}