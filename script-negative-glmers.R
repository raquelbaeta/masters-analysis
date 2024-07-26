# Start

# title: "Regression Analysis"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Start Regression Analysis

#
# Setting up
# 

# Set working directory
setwd("~/Desktop/working-sessions/models")

# Load data (Ensure the file format is correct)
data <- readRDS("~/Desktop/working-sessions/models/data.csv.rds")
print(data)

# Look for duplicates
any_duplicates <- any(duplicated(data))
print(any_duplicates)

#
# Aggregating data
#

# Cut into year_intervals
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = FALSE)

#
# Model 1: "any seizures"
# 

# Group by 3-year intervals for binary
grouped_data_3yr_binary <- data %>%
  group_by(region, country, code, year_interval_3yr) %>%
  summarise(
    any_seizures = max(seizures_binary),
    any_UN = max(any_UN),
    mean_log_adjusted_gdp_cap = mean(log_adjusted_gdp_cap),
    mean_log_gdp = mean(log_gdp),
    mean_milex_gdp = mean(milex_gdp),
    mean_trade_ratio = mean(trade_ratio),
    mean_CC.EST = mean(CC.EST),
    mean_RQ.EST = mean(RQ.EST),
    mean_RL.EST = mean(RL.EST),
    mean_PV.EST = mean(PV.EST),
    mean_GE.EST = mean(GE.EST),
    mean_VA.EST = mean(VA.EST)
  )

#
# Cleaning
# 

# Replace NA values with a placeholder (Ensure this is appropriate)
grouped_data_3yr_binary$year_interval_3yr[is.na(grouped_data_3yr_binary$year_interval_3yr)] <- 8

# Verify the change
summary(grouped_data_3yr_binary$year_interval_3yr)

# Look for duplicates
any_duplicates <- any(duplicated(grouped_data_3yr_binary))
print(any_duplicates)

# Save data as a .rds
saveRDS(grouped_data_3yr_binary, "~/Desktop/working-sessions/models/grouped_data_3yr_binary.csv.rds")

#
# Logistic regression model with random effects
#

# Model for "any seizures" and "Governance Effectiveness"
model_glmer.GE <- glmer(
  any_seizures ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio + mean_GE.EST + 
    (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_binary,
  family = binomial
)

# Print
summary(model_glmer.GE)

# Marginal effect of UN commitment on predicted seizures
ggeffect_glmer.GE <- ggeffect(model_glmer.GE, terms = "any_UN")
print(ggeffect_glmer.GE)

# Generate predicted probabilities
plot_obj_GE <- plot_model(
  model_glmer.GE, 
  type = "pred", 
  terms = c("any_UN", "mean_GE.EST"), 
  .plot = FALSE)

# Plot predicted probabilities
plot_obj_GE <- plot_obj_GE +
  theme_minimal() +
  labs(
    title = "Predicted Seizures Based on UN Commitment and GE",
    x = "UN Commitment",
    y = "Predicted Probability of Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_GE)

# Save the plot as a PDF/PNG
ggsave("predicted_probabilities_seizures_ge_plot.pdf", plot = plot_obj_GE, 
       width = 8, height = 6)
ggsave("predicted_probabilities_seizures_ge_plot.png", plot = plot_obj_GE, 
       width = 8, height = 6)

# Model for "any seizures" and "Rule of Law"
model_glmer.RL <- glmer(
  any_seizures ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio + mean_RL.EST + 
    (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_binary,
  family = binomial
)

# Print
summary(model_glmer.RL)

# Marginal effect of UN commitment on predicted seizures
ggeffect_glmer.RL <- ggeffect(model_glmer.RL, terms = "any_UN")
print(ggeffect_glmer.RL)

# Generate predicted probabilities
plot_obj_RL <- plot_model(
  model_glmer.RL, 
  type = "pred", 
  terms = c("any_UN", "mean_RL.EST"), 
  .plot = FALSE)

# Plot predicted probabilities
plot_obj_RL <- plot_obj_RL +
  theme_minimal() +
  labs(
    title = "Predicted Seizures Based on UN Commitment and RL",
    x = "UN Commitment",
    y = "Predicted Probability of Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_RL)

# Save the plot as a PDF/PNG
ggsave("predicted_probabilities_seizures_rl_plot.pdf", plot = plot_obj_RL, 
       width = 8, height = 6)
ggsave("predicted_probabilities_seizures_rl_plot.png", plot = plot_obj_RL, 
       width = 8, height = 6)

# Model for "any seizures" and "PV"
model_glmer.PV <- glmer(
  any_seizures ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio + mean_PV.EST + 
    (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_binary,
  family = binomial
)

# Print
summary(model_glmer.PV)

# Marginal effect of UN commitment on predicted seizures
ggeffect_glmer.PV <- ggeffect(model_glmer.PV, terms = "any_UN")
print(ggeffect_glmer.PV)

# Generate predicted probabilities
plot_obj_PV <- plot_model(
  model_glmer.PV, 
  type = "pred", 
  terms = c("any_UN", "mean_PV.EST"), 
  .plot = FALSE)

# Plot predicted probabilities
plot_obj_PV <- plot_obj_PV +
  theme_minimal() +
  labs(
    title = "Predicted Seizures Based on UN Commitment and PV",
    x = "UN Commitment",
    y = "Predicted Probability of Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_PV)

# Save the plot as a PDF/PNG
ggsave("predicted_probabilities_seizures_pv_plot.pdf", plot = plot_obj_PV, 
       width = 8, height = 6)
ggsave("predicted_probabilities_seizures_pv_plot.png", plot = plot_obj_PV, 
       width = 8, height = 6)

# Combine model plots
grid_arrangement_log_reg_rand_eff <- arrangeGrob(
  plot_obj_GE, plot_obj_RL, plot_obj_PV,
  ncol = 2, 
  nrow = 3
)

print(grid_arrangement_log_reg_rand_eff)

# Save the combined plot as a PDF/PNG
ggsave("grid_arrangement_log_reg_rand_eff.pdf", 
       plot = grid_arrangement_log_reg_rand_eff, width = 12, height = 8)
ggsave("grid_arrangement_log_reg_rand_eff.png", 
       plot = grid_arrangement_log_reg_rand_eff, width = 12, height = 8)

#
# Model 2: log(mean_seizures)
#

# Group by 3-year intervals for mean
grouped_data_3yr_mean <- data %>%
  group_by(region, country, code, year_interval_3yr) %>%
  summarise(
    mean_seizures = mean(seizures),
    any_UN = max(any_UN),
    mean_log_adjusted_gdp_cap = mean(log_adjusted_gdp_cap),
    mean_log_gdp = mean(log_gdp),
    mean_milex_gdp = mean(milex_gdp),
    mean_trade_ratio = mean(trade_ratio),
    mean_CC.EST = mean(CC.EST),
    mean_RQ.EST = mean(RQ.EST),
    mean_RL.EST = mean(RL.EST),
    mean_PV.EST = mean(PV.EST),
    mean_GE.EST = mean(GE.EST),
    mean_VA.EST = mean(VA.EST)
  )

#
# Cleaning
# 

# Replace NA values with a placeholder (Ensure this is appropriate)
grouped_data_3yr_mean$year_interval_3yr[is.na(grouped_data_3yr_mean$year_interval_3yr)] <- 8

# Verify the change
summary(grouped_data_3yr_mean$year_interval_3yr)

# Look for duplicates
any_duplicates <- any(duplicated(grouped_data_3yr_mean))
print(any_duplicates)

#
# Filtering
#

# Filter out "0"
grouped_data_3yr_mean_only <- grouped_data_3yr_mean %>%
  filter(mean_seizures > 0)

# Save data set as a .rds
saveRDS(grouped_data_3yr_mean, "~/Desktop/working-sessions/models/grouped_data_3yr_mean.csv.rds")

#
# Linear mixed effects models
#

# Model for mean_seizures and GE
model_lmm.GE <- lmer(
  log(mean_seizures) ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio +
    mean_GE.EST + (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_mean_only
)

# Print
summary(model_lmm.GE)

# Marginal effect of UN commitment on predicted seizures
ggeffect_lmm.GE <- ggeffect(model_lmm.GE, terms = "any_UN")
print(ggeffect_lmm.GE)

# Generate predicted values
plot_obj_lmm_GE <- plot_model(
  model_lmm.GE, 
  type = "pred", 
  terms = c("any_UN", "mean_GE.EST"), 
  .plot = FALSE)

# Plot predicted values
plot_obj_lmm_GE <- plot_obj_lmm_GE +
  theme_minimal() +
  labs(
    title = "Predicted Mean Seizures Based on UN Commitment and GE",
    x = "UN Commitment",
    y = "Predicted Mean Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_lmm_GE)

# Save the plot as a PDF/PNG
ggsave("predicted_mean_seizures_ge_plot.pdf", 
       plot = plot_obj_lmm_GE, width = 8, height = 6)
ggsave("predicted_mean_seizures_ge_plot.png", 
       plot = plot_obj_lmm_GE, width = 8, height = 6)

# Model for mean_seizures and RL
model_lmm.RL <- lmer(
  log(mean_seizures) ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio + mean_RL.EST + 
    (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_mean_only
)

# Print
summary(model_lmm.RL)

# Marginal effect of UN commitment on predicted seizures
ggeffect_lmm.RL <- ggeffect(model_lmm.RL, terms = "any_UN")
print(ggeffect_lmm.RL)

# Generate predicted values
plot_obj_lmm_RL <- plot_model(
  model_lmm.RL, 
  type = "pred", 
  terms = c("any_UN", "mean_RL.EST"), 
  .plot = FALSE)

# Plot predicted values
plot_obj_lmm_RL <- plot_obj_lmm_RL +
  theme_minimal() +
  labs(
    title = "Predicted Mean Seizures Based on UN Commitment and RL",
    x = "UN Commitment",
    y = "Predicted Mean Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_lmm_RL)

# Save the plot as a PDF/PNG
ggsave("predicted_mean_seizures_rl_plot.pdf", plot = plot_obj_lmm_RL, 
       width = 8, height = 6)
ggsave("predicted_mean_seizures_rl_plot.png", plot = plot_obj_lmm_RL, 
       width = 8, height = 6)

# Model for mean_seizures and PV
model_lmm.PV <- lmer(
  log(mean_seizures) ~ any_UN +
    mean_log_adjusted_gdp_cap + mean_log_gdp +
    mean_milex_gdp + mean_trade_ratio + mean_PV.EST + 
    (1 | region) + (1 | year_interval_3yr),
  data = grouped_data_3yr_mean_only
)

# Print
summary(model_lmm.PV)

# Marginal effect of UN commitment on predicted seizures
ggeffect_lmm.PV <- ggeffect(model_lmm.PV, terms = "any_UN")
print(ggeffect_lmm.PV)

# Generate predicted values
plot_obj_lmm_PV <- plot_model(
  model_lmm.PV, 
  type = "pred", 
  terms = c("any_UN", "mean_PV.EST"), 
  .plot = FALSE)

# Plot predicted values
plot_obj_lmm_PV <- plot_obj_lmm_PV +
  theme_minimal() +
  labs(
    title = "Predicted Mean Seizures Based on UN Commitment and PV",
    x = "UN Commitment",
    y = "Predicted Mean Seizures") +
  theme(axis.text.x = element_text(hjust = 1))

print(plot_obj_lmm_PV)

# Save the plot as a PDF/PNG
ggsave("predicted_mean_seizures_pv_plot.pdf", plot = plot_obj_lmm_PV, 
       width = 8, height = 6)
ggsave("predicted_mean_seizures_pv_plot.png", plot = plot_obj_lmm_PV, 
       width = 8, height = 6)

# Combine model plots
grid_arrangement_lmm_log_seizures <- arrangeGrob(
  plot_obj_lmm_GE, plot_obj_lmm_RL, plot_obj_lmm_PV,
  ncol = 2, 
  nrow = 3
)

print(grid_arrangement_lmm_log_seizures)

# Save the combined plot as a PDF/PNG
ggsave("grid_arrangement_lmm_log_seizures.pdf", 
       plot = grid_arrangement_lmm_log_seizures, width = 12, height = 8)
ggsave("grid_arrangement_lmm_log_seizures.png", 
       plot = grid_arrangement_lmm_log_seizures, width = 12, height = 8)

#
# Model summaries
#

# Logistic regression model summary
stargazer(model_glmer.GE, model_glmer.RL, model_glmer.PV, 
          title = "Logistic Regression Models", 
          type = "text", 
          out = "logistic_regression_models_summary.txt")

# Linear mixed effects models summary
stargazer(model_lmm.GE, model_lmm.RL, model_lmm.PV, 
          title = "Linear Mixed Effects Models", 
          type = "text", 
          out = "linear_mixed_effects_models_summary.txt")

#
# Check residuals for LMMs
#

# Check residuals for logistic regression models
qqnorm(residuals(model_glmer.GE))
qqline(residuals(model_glmer.GE))

qqnorm(residuals(model_glmer.RL))
qqline(residuals(model_glmer.RL))

qqnorm(residuals(model_glmer.PV))
qqline(residuals(model_glmer.PV))

# Check residuals for linear mixed effects models
qqnorm(residuals(model_lmm.GE))
qqline(residuals(model_lmm.GE))

qqnorm(residuals(model_lmm.RL))
qqline(residuals(model_lmm.RL))

qqnorm(residuals(model_lmm.PV))
qqline(residuals(model_lmm.PV))

# End