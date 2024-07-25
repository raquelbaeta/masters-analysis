# Start

# title: "Background on United Nations Conventions and Economic Indicators (in USD)"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Set working directory
setwd("~/Desktop/working-sessions/regression")

# Load libraries
library(c(readr, dplyr))

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Create year intervals
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", 
             "2011-2013", "2014-2016")
)

# Group data by region, country, and UN commitment
print(economic_results <- data %>%
        group_by(
          region, country, any_UN) %>%
        summarize(
          mean_seizures = mean(seizures, na.rm = TRUE),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp, na.rm = TRUE),
          mean_milex_gdp = mean(milex_gdp, na.rm = TRUE),
          mean_trade_ratio = mean(trade_ratio, na.rm = TRUE),
          mean_exports_gdp = mean(exports_gdp, na.rm = TRUE),
          mean_imports_gdp = mean(imports_gdp, na.rm = TRUE)
        )
      )

# Time series analysis by year interval
economic_data <- data %>%
  group_by(region, year_interval_3yr) %>%
  summarize(
    mean_seizures = mean(seizures, na.rm = TRUE),
    mean_log_adjusted_gdp = mean(log_adjusted_gdp, na.rm = TRUE),
    mean_milex_gdp = mean(milex_gdp, na.rm = TRUE),
    mean_trade_ratio = mean(trade_ratio, na.rm = TRUE),
    mean_exports_gdp = mean(exports_gdp, na.rm = TRUE),
    mean_imports_gdp = mean(imports_gdp, na.rm = TRUE)
  )

# Summarize economic data by region
print(economic_region_data <- data %>%
        group_by(region) %>%
        summarize(
          mean_seizures = mean(seizures, na.rm = TRUE),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp, na.rm = TRUE),
          mean_milex_gdp = mean(milex_gdp, na.rm = TRUE),
          mean_trade_ratio = mean(trade_ratio, na.rm = TRUE)
        ))

# Filter data by region and write CSV files
regions <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
             "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa")

for (region in regions) {
  filtered_data <- filter(economic_data, region == region)
  write.csv(
    filtered_data, 
    paste0("filtered_", gsub(" ", "_", tolower(region)), "_economic.csv"), 
    row.names = FALSE)
  
  print(paste("Summary for", region))
  print(summary(filtered_data$mean_log_adjusted_gdp))
  print(summary(filtered_data$mean_milex_gdp))
  print(summary(filtered_data$mean_trade_ratio))
  print(summary(filtered_data$mean_exports_gdp))
  print(summary(filtered_data$mean_imports_gdp))
}

# To get averages for each region
print(economic_averages_by_region <- economic_results %>%
        group_by(region) %>%
        summarize(
          average_mean_seizures = mean(mean_seizures, na.rm = TRUE),
          average_log_adjusted_gdp = mean(mean_log_adjusted_gdp, na.rm = TRUE),
          average_milex_gdp = mean(mean_milex_gdp, na.rm = TRUE),
          average_exports_gdp = mean(mean_exports_gdp, na.rm = TRUE),
          average_imports_gdp = mean(mean_imports_gdp, na.rm = TRUE)
        )
      )

# To get averages for each region for three-year intervals
print(economic_averages_by_region_interval <- data %>%
        group_by(
          region, year_interval_3yr) %>%
        summarize(
          interval_mean_seizures = mean(seizures, na.rm = TRUE),
          interval_mean_log_adjusted_gdp = mean(log_adjusted_gdp, na.rm = TRUE),
          interval_mean_milex_gdp = mean(milex_gdp, na.rm = TRUE),
          interval_mean_exports_gdp = mean(exports_gdp, na.rm = TRUE),
          interval_mean_imports_gdp = mean(imports_gdp, na.rm = TRUE)
        )
      )

# Filter and print rows for the USA
us_rows <- data %>% filter(code == "USA")
print(us_rows, n = 21)

# End