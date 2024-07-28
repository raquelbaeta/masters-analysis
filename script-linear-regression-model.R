# Start

# title: "Linear Regression"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Set working directory
setwd("~/Desktop/working-sessions/regression")

# Load required libraries
library(readr, MASS, car, lmtest, sandwich)

# Load the dataset
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Check for duplicates in the dataset
any_duplicates <- any(duplicated(data))
if (any_duplicates) {
  print("There are duplicates in the data frame.")
} else {
  print("No duplicates found in the data frame.")
}

# Fit a linear model
linear_model <- lm(
  seizures_binary ~ 
    log_adjusted_gdp + trade_ratio + milex_gdp + any_UN + CC.EST,
  data = data
)
summary(linear_model)

# Residual diagnostics
# Display the first few residuals
head(resid(linear_model), 4)
head(rstandard(linear_model), 4)
head(rstudent(linear_model), 4)

# Outlier and leverage diagnostics
# QQ plot for assessing normality of residuals
qqPlot(linear_model, id = list(n = 2, cex = 0.6))

# Test for outliers
outlierTest(linear_model)

# Plot residuals
plot(
  resid(linear_model), 
  main = "Residuals Plot", 
  xlab = "Observation", 
  ylab = "Residuals")
points(213, resid(linear_model)[213], col = "red", pch = 16, cex = 2) # Highlight observation 213
abline(h = 0, col = "blue", lty = 2)

# Influence measures
# Display top influential points based on Cook's distance
head(sort(cooks.distance(linear_model), decreasing = TRUE), 9)
influencePlot(linear_model, id = list(n = 3, cex = 0.6))

# Check for heteroskedasticity
residualPlots(
  linear_model, ~ 1, 
  fitted = TRUE, 
  id = list(n = 0), 
  quadratic = FALSE, 
  tests = FALSE
)

ncvTest(linear_model)

# Collinearity diagnostics
vif(linear_model)

# RESET test for model specification
resettest(linear_model) # Checks for model misspecification

# Breusch-Pagan test for heteroscedasticity
bptest(linear_model) # Tests for non-constant variance of residuals

# Calculate Cook's distance and identify outliers
cooksd <- cooks.distance(linear_model)
threshold_percentile <- quantile(cooksd, 0.95)
outliers <- which(cooksd > threshold_percentile)

# Remove outliers from the dataset
data_without_outliers <- data[-outliers, ]

# Fit a new linear model without outliers
linear_model_without_outliers <- lm(
  seizures_binary ~ 
    log_adjusted_gdp + trade_ratio + milex_gdp + any_UN + CC.EST,
  data = data_without_outliers
)

summary(linear_model_without_outliers)

# Residual diagnostics for model without outliers
head(resid(linear_model_without_outliers), 4)
head(rstandard(linear_model_without_outliers), 4)
head(rstudent(linear_model_without_outliers), 4)

# Outlier and leverage diagnostics for model without outliers
qqPlot(linear_model_without_outliers, id = list(n = 2, cex = 0.6))
outlierTest(linear_model_without_outliers)

# Plot residuals for observation 1234
plot(
  resid(linear_model_without_outliers), 
  main = "Residuals Plot", 
  xlab = "Observation", 
  ylab = "Residuals")
points(1234, 
       resid(linear_model_without_outliers)[1234], 
       col = "red", 
       pch = 16, 
       cex = 2) # Highlight observation 1234
abline(h = 0, col = "blue", lty = 2)

# Influence measures for model without outliers
head(sort(cooks.distance(linear_model_without_outliers), decreasing = TRUE), 9)
influencePlot(linear_model_without_outliers, id = list(n = 3, cex = 0.6))

# Check for heteroskedasticity in model without outliers
residualPlots(
  linear_model_without_outliers, ~ 1, 
  fitted = TRUE, 
  id = list(n = 0), 
  quadratic = FALSE, 
  tests = FALSE
)

ncvTest(linear_model_without_outliers)

# Collinearity diagnostics for model without outliers
vif(linear_model_without_outliers)

# RESET test for model without outliers
resettest(linear_model_without_outliers)

# Breusch-Pagan test for heteroscedasticity in model without outliers
bptest(linear_model_without_outliers)

# Plot residuals for observation 1234 in the model without outliers
plot(
  resid(linear_model_without_outliers), 
  main = "Residuals Plot", 
  xlab = "Observation", 
  ylab = "Residuals")
points(1234, 
       resid(linear_model_without_outliers)[1234], 
       col = "red", 
       pch = 16, 
       cex = 2)
abline(h = 0, col = "blue", lty = 2)

# Function to calculate AIC and BIC
calculate_information_criteria <- function(model) {
  nobs <- length(model$residuals)
  k <- length(coef(model))
  log_likelihood <- -0.5 * sum(log(2 * pi) + log(model$sigma^2) + (model$residuals/model$sigma)^2)
  aic <- -2 * log_likelihood + 2 * k
  bic <- -2 * log_likelihood + k * log(nobs)
  return(c(AIC = aic, BIC = bic))
}

# Calculate AIC and BIC for both models
info_criteria_outlier <- calculate_information_criteria(linear_model)
info_criteria_without_outlier <- calculate_information_criteria(linear_model_without_outliers)

# Print the AIC and BIC results
print("AIC and BIC for Linear Model with Outliers:")
print(info_criteria_outlier)
print("AIC and BIC for Linear Model without Outliers:")
print(info_criteria_without_outlier)

# End
