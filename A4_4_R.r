# Load necessary libraries
library(dplyr)
library(tidyr)
library(lmtest)

# Load the dataset
pizza_data <- read.csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/pizza_data.csv")

# Data cleaning: Convert price to numeric
pizza_data$price <- as.numeric(gsub("[\\$,]", "", pizza_data$price))

# Data cleaning: Extract numeric part from weight and convert to numeric
pizza_data$weight <- as.numeric(gsub("[^0-9.]", "", pizza_data$weight))

# Perform further data preprocessing: One-hot encoding
pizza_data <- pizza_data %>%
  mutate_if(is.character, as.factor) %>%
  select(-brand) %>%
  dummy_cols(remove_first_dummy = TRUE)

# Standardize numerical variables: Price and Weight
pizza_data[, c("price", "weight")] <- scale(pizza_data[, c("price", "weight")])

# Fit a linear regression model
model <- lm(ranking ~ ., data = pizza_data)

# Print model summary
summary(model)

# Extract coefficients as part-worth utilities
part_worths <- coef(model)[-1]  # Exclude intercept

# Print part-worth utilities
cat("Part-Worth Utilities (Coefficients):\n")
print(part_worths)

# Calculate relative importance of attributes
total_importance <- sum(abs(part_worths))
relative_importance <- abs(part_worths) / total_importance

# Print relative importance of attributes
cat("\nRelative Importance of Attributes:\n")
print(relative_importance)
