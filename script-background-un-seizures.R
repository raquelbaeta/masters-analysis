# Start

# title: "Background on United Nations Conventions and cocaine seizures (in kgs)"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Set working directory
setwd("~/Desktop/working-sessions/regression")

# Load required libraries
library(c(readr, dplyr))

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Group data
results <- data %>%
  group_by(
    region, code, country, any_UN, UN1961, UN1971, UN1988) %>%
  summarize(
    total_seizures = sum(seizures), 
    mean_seizures = mean(seizures))

# Averages by region for UN commitment years
un_averages_by_region <- results %>%
  group_by(region) %>%
  summarize(
    average_total_any_UN = mean(any_UN, na.rm = TRUE),
    average_mean_UN1961 = mean(UN1961, na.rm = TRUE),
    average_mean_UN1971 = mean(UN1971, na.rm = TRUE),
    average_mean_UN1988 = mean(UN1988, na.rm = TRUE))

print(un_averages_by_region)

# Averages by region for seizures
averages_by_region <- results %>%
  group_by(region) %>%
  summarize(
    average_total_seizures = mean(total_seizures, na.rm = TRUE),
    average_mean_seizures = mean(mean_seizures, na.rm = TRUE))

print(averages_by_region)

# Count of unique countries with '0' and '1' in the any_UN column
count_zero <- sum(data$any_UN == 0, na.rm = TRUE)
countries_with_zero <- unique(data$country[data$any_UN == 0])
count_zero_countries <- length(countries_with_zero)

count_one <- sum(data$any_UN == 1, na.rm = TRUE)
countries_with_one <- unique(data$country[data$any_UN == 1])
count_one_countries <- length(countries_with_one)

print(paste("Countries with '0' in any_UN:", count_zero))
print(paste("Unique countries with '0':", count_zero_countries))
print(paste("Countries with '1' in any_UN:", count_one))
print(paste("Unique countries with '1':", count_one_countries))

# Similar counts for UN1961, UN1971, UN1988 columns
count_zero_1961 <- sum(data$UN1961 == 0, na.rm = TRUE)
countries_with_zero_1961 <- unique(data$country[data$UN1961 == 0])
count_zero_countries_1961 <- length(countries_with_zero_1961)

count_one_1961 <- sum(data$UN1961 == 1, na.rm = TRUE)
countries_with_one_1961 <- unique(data$country[data$UN1961 == 1])
count_one_countries_1961 <- length(countries_with_one_1961)

count_zero_1971 <- sum(data$UN1971 == 0, na.rm = TRUE)
countries_with_zero_1971 <- unique(data$country[data$UN1971 == 0])
count_zero_countries_1971 <- length(countries_with_zero_1971)

count_one_1971 <- sum(data$UN1971 == 1, na.rm = TRUE)
countries_with_one_1971 <- unique(data$country[data$UN1971 == 1])
count_one_countries_1971 <- length(countries_with_one_1971)

count_zero_1988 <- sum(data$UN1988 == 0, na.rm = TRUE)
countries_with_zero_1988 <- unique(data$country[data$UN1988 == 0])
count_zero_countries_1988 <- length(countries_with_zero_1988)

count_one_1988 <- sum(data$UN1988 == 1, na.rm = TRUE)
countries_with_one_1988 <- unique(data$country[data$UN1988 == 1])
count_one_countries_1988 <- length(countries_with_one_1988)

print(paste("Countries with '0' in UN1961:", count_zero_1961))
print(paste("Unique countries with '0' in UN1961:", count_zero_countries_1961))
print(paste("Countries with '1' in UN1961:", count_one_1961))
print(paste("Unique countries with '1' in UN1961:", count_one_countries_1961))

print(paste("Countries with '0' in UN1971:", count_zero_1971))
print(paste("Unique countries with '0' in UN1971:", count_zero_countries_1971))
print(paste("Countries with '1' in UN1971:", count_one_1971))
print(paste("Unique countries with '1' in UN1971:", count_one_countries_1971))

print(paste("Countries with '0' in UN1988:", count_zero_1988))
print(paste("Unique countries with '0' in UN1988:", count_zero_countries_1988))
print(paste("Countries with '1' in UN1988:", count_one_1988))
print(paste("Unique countries with '1' in UN1988:", count_one_countries_1988))

# Summary of seizures by region
regions <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", 
             "North America", "South Asia", "Sub-Saharan Africa")

for (region in regions) {
  filtered_data <- data %>% filter(region == region)
  print(paste("Summary for region:", region))
  print(summary(filtered_data$seizures))
  
  commitment_summary <- filtered_data %>%
    group_by(
      region, code, country, any_UN, UN1961, UN1971, UN1988) %>%
    summarize(
      total_seizures = sum(seizures),
      mean_seizures = mean(seizures))
  print(commitment_summary)
  
  # Count and list of countries by any_UN value
  n_any_un_1 <- nrow(
    commitment_summary %>% 
      filter(any_UN == 1))
  
  n_any_un_0 <- nrow(
    commitment_summary %>% 
      filter(any_UN == 0))
  
  countries_with_any_un_1 <- commitment_summary %>% 
    filter(any_UN == 1) %>% 
    pull(country)
 
   countries_with_any_un_0 <- commitment_summary %>% 
    filter(any_UN == 0) %>% 
    pull(country)
  
  cat("Number of countries with any_UN value of 1:", n_any_un_1, "\n")
  cat("Number of countries with any_UN value of 0:", n_any_un_0, "\n")
  cat("Countries with any_UN value of 1:", paste(countries_with_any_un_1, collapse = ", "), "\n")
  cat("Countries with any_UN value of 0:", paste(countries_with_any_un_0, collapse = ", "), "\n")
}

# End
