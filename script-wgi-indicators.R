# Start

# title: "Background on United Nations Conventions and World Governance Indicators (WGI)"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Set working directory
setwd("~/Desktop/working-sessions/regression")

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Calculate mean WGI score
data <- data %>%
  rowwise() %>%
  mutate(
    mean_WGI = mean(c(CC.EST, GE.EST, RQ.EST, RL.EST, VA.EST, PV.EST), na.rm = TRUE))

# Intervals and labels
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016")
)

# Group data by Country averages 
governance_data <- data %>%
  group_by(
    region, code, country, any_UN, UN1961, UN1971, UN1988) %>%
  summarize(
    mean_seizures = mean(seizures, na.rm = TRUE),
    mean_CC.EST = mean(CC.EST, na.rm = TRUE),
    mean_GE.EST = mean(GE.EST, na.rm = TRUE),
    mean_RQ.EST = mean(RQ.EST, na.rm = TRUE),
    mean_RL.EST = mean(RL.EST, na.rm = TRUE),
    mean_VA.EST = mean(VA.EST, na.rm = TRUE),
    mean_PV.EST = mean(PV.EST, na.rm = TRUE),
    mean_WGI.EST = mean(mean_WGI, na.rm = TRUE)
  )

# Time series analysis 
governance_data_time_series <- data %>%
  group_by(
    region, year_interval_3yr) %>%
  summarize(
    mean_seizures = mean(seizures, na.rm = TRUE),
    mean_CC.EST = mean(CC.EST, na.rm = TRUE),
    mean_GE.EST = mean(GE.EST, na.rm = TRUE),
    mean_RQ.EST = mean(RQ.EST, na.rm = TRUE),
    mean_RL.EST = mean(RL.EST, na.rm = TRUE),
    mean_VA.EST = mean(VA.EST, na.rm = TRUE),
    mean_PV.EST = mean(PV.EST, na.rm = TRUE),
    mean_WGI.EST = mean(mean_WGI, na.rm = TRUE)
  )

# Filter and write CSV files for different regions
regions <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", "North America", 
             "South Asia", "Sub-Saharan Africa")

for (region in regions) {
  filtered_data <- filter(
    governance_data_time_series, region == region)
  write.csv(
    filtered_data, 
    paste0("filtered_", gsub(" ", "_", tolower(region)), "_wgi.csv"),
    row.names = FALSE)
  
  print(summary(filtered_data$mean_WGI.EST))
  print(summary(filtered_data$mean_CC.EST))
  print(summary(filtered_data$mean_GE.EST))
  print(summary(filtered_data$mean_RQ.EST))
  print(summary(filtered_data$mean_RL.EST))
  print(summary(filtered_data$mean_VA.EST))
  print(summary(filtered_data$mean_PV.EST))
}

# Print and summarize US data
us_rows <- data %>% filter(code == "USA")
print(us_rows, n = 21)

# End
