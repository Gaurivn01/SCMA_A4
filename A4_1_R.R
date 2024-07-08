install.packages(c("tidyverse", "psych", "factoextra"))

library(tidyverse)
library(psych)
library(factoextra)
library(GPArotation)
library(ggplot2)


# Load the dataset
df <- read.csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/Survey.csv")

# Display the first few rows of the dataset
head(df)

# Data Cleaning
# Handle missing values (if any)
df <- na.omit(df)

# Select numerical columns for PCA
numerical_cols <- df %>% select_if(is.numeric)

# Exploratory Data Analysis (EDA)
# Pairplot for initial exploration (only for a few columns to keep it simple)
pairs(numerical_cols[1:5], main = "Pairplot of First 5 Numerical Features")

# Correlation matrix (only for a few columns to keep it simple)
corr_matrix <- cor(numerical_cols[1:10])
corrplot::corrplot(corr_matrix, method = "circle")

# Standardize the data
df_scaled <- scale(numerical_cols)

# Principal Component Analysis (PCA)
pca <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca)

# Scree plot to visualize the explained variance
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# Choose number of components (e.g., based on explained variance > 90%)
explained_variance <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
n_components <- which(explained_variance > 0.9)[1]
print(paste("Number of components explaining > 90% variance:", n_components))



# Factor Analysis (FA)
# Determine the number of factors using the Kaiser criterion (Eigenvalues > 1)
fa_model <- fa(df_scaled, nfactors = length(numerical_cols), rotate = "none")
eigenvalues <- fa_model$e.values

# Scree plot to visualize eigenvalues
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Factors", ylab = "Eigenvalue")
abline(h = 1, col = "red", lty = 2)

# Choose the number of factors (e.g., based on eigenvalues > 1)
n_factors <- sum(eigenvalues > 1)
print(paste("Number of factors with eigenvalue > 1:", n_factors))

# Apply Factor Analysis with the chosen number of factors
fa_model <- fa(df_scaled, nfactors = n_factors, rotate = "varimax")

# Visualize factor analysis results
# Factor loadings
fviz_famd_var(fa_model, repel = TRUE)

# Biplot of individuals and variables
fviz_famd_ind(fa_model, repel = TRUE)

