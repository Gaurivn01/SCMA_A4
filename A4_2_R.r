# Load necessary libraries
library(tidyverse)
library(caret)
library(factoextra)
library(cluster)
library(dplyr)

# Load the dataset
df <- read.csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/Survey.csv")

# Inspect the data
head(df)
str(df)

# Handle concatenated strings (if applicable)
df <- df %>%
  mutate(across(where(is.character), ~str_split(.x, "(?<!\\w)(?=[A-Z])", simplify = TRUE)[,1]))

# Verify the data after cleaning
head(df)
str(df)

# Select only categorical columns and numerical columns for clustering
categorical_cols <- select(df, where(is.character))
numerical_cols <- select(df, where(is.numeric))

# Handle missing values in numerical data
numerical_cols[is.na(numerical_cols)] <- sapply(numerical_cols, function(x) mean(x, na.rm = TRUE))

# Remove categorical columns with only one unique value
categorical_cols <- categorical_cols %>% select_if(~ n_distinct(.) > 1)

# Encode categorical variables using One-Hot Encoding
dummies <- dummyVars(~ ., data = categorical_cols)
encoded_cat <- predict(dummies, newdata = categorical_cols)
encoded_cat <- as.data.frame(encoded_cat)

# Combine encoded categorical data with numerical data
df_encoded <- cbind(encoded_cat, numerical_cols)

# Feature scaling
preprocess <- preProcess(df_encoded, method = c("center", "scale"))
df_encoded_scaled <- predict(preprocess, df_encoded)

# Determine optimal number of clusters using the Elbow method
wss <- (nrow(df_encoded_scaled) - 1) * sum(apply(df_encoded_scaled, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(df_encoded_scaled, centers = i, nstart = 20)$tot.withinss)
}

# Plot Elbow method
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Perform KMeans clustering
k <- 3  # Choose the optimal number of clusters based on the Elbow method or Silhouette score
kmeans_result <- kmeans(df_encoded_scaled, centers = k, nstart = 20)
clusters <- kmeans_result$cluster

# Add cluster labels to the original dataframe
df$Cluster <- as.factor(clusters)

# Analyze cluster characteristics (e.g., centroids, cluster sizes)
cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(cluster_summary)

# Optional: Visualize clusters (PCA for dimensionality reduction if needed)
pca_result <- prcomp(df_encoded_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)
pca_data$Cluster <- as.factor(clusters)

# PCA Plot of Clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA Plot of Clusters", x = "PCA Component 1", y = "PCA Component 2") +
  scale_color_viridis_d()

# Assess cluster quality with silhouette score
silhouette_score <- silhouette(clusters, dist(df_encoded_scaled))
fviz_silhouette(silhouette_score)

