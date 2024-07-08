library(ggplot2)

# Step 1: Load and Clean Data
df <- read.csv("C:/Users/gauri/OneDrive/Documents/VCU/SCMA/icecream.csv")

# Check for missing values
print(colSums(is.na(df)))

# Step 2: EDA and Data Preparation
# Summarize dataset
print(summary(df))

# Visualize distributions or relationships as needed (e.g., using ggplot2, base R plots)
# Using ggplot2's pairs plot (similar to seaborn's pairplot)
pairs <- ggplot(df, aes(color = Brand)) +
  geom_point(aes(x = Price, y = Taste)) +
  labs(title = "Pairplot of Ice Cream Attributes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(~ Brand)
print(pairs)

#Correlation Matrix Heatmap
# Using base R for heatmap
cor_matrix <- cor(df[, -1])  # Exclude 'Brand' column
heatmap(cor_matrix, 
        Rowv = NA, Colv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        scale = "none",
        margins = c(5, 10),
        main = "Correlation Matrix of Ice Cream Attributes")

#Individual Attribute Distributions (example with Price)
# Using base R for histogram
hist(df$Price, 
     breaks = 10,
     col = "blue",
     border = "black",
     xlab = "Price",
     ylab = "Count",
     main = "Distribution of Price")


# Step 3: Standardize the Data (if necessary)
# Example using scale function
df_scaled <- scale(df[, -1])  # Exclude 'Brand' column for scaling

# Step 4: Apply Multidimensional Scaling (MDS)
# Calculate dissimilarities
dissim <- dist(df_scaled, method = "euclidean")

# MDS fitting
set.seed(42)  # for reproducibility
mds <- cmdscale(dissim, k = 2)

# Step 5: Visualize MDS Results
plot(mds, type = "n", main = "Multidimensional Scaling (MDS) of Ice Cream Brands", 
     xlab = "Dimension 1", ylab = "Dimension 2")
text(mds, labels = df$Brand, col = "blue", cex = 0.8)
grid()
