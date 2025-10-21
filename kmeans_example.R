# K-Means Clustering Examples and Demonstration
# Author: Claude
# Description: Examples showing how to use the k-means implementation

# Load the k-means implementation
source("kmeans.R")

cat("=== K-Means Clustering Examples ===\n\n")

# Example 1: Simple 2D clustering
cat("Example 1: Simple 2D Clustering\n")
cat("--------------------------------\n")

# Generate sample data with 3 distinct clusters
set.seed(42)
data_2d <- rbind(
  matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2),   # Cluster 1
  matrix(rnorm(100, mean = 4, sd = 0.5), ncol = 2),   # Cluster 2
  matrix(rnorm(100, mean = c(0, 4), sd = 0.5), ncol = 2)  # Cluster 3
)

# Run k-means with k=3
result_2d <- kmeans_custom(data_2d, k = 3, seed = 42)

cat("Number of clusters:", length(unique(result_2d$clusters)), "\n")
cat("Iterations until convergence:", result_2d$iterations, "\n")
cat("Total inertia:", round(result_2d$inertia, 2), "\n")
cat("Converged:", result_2d$converged, "\n")
cat("\nCluster sizes:\n")
print(table(result_2d$clusters))
cat("\nCluster centers:\n")
print(round(result_2d$centers, 2))

# Visualize the results
cat("\nCreating visualization...\n")
visualize_kmeans_2d(data_2d, result_2d, title = "K-Means Clustering: 3 Clusters")


# Example 2: Using the Elbow Method
cat("\n\nExample 2: Elbow Method for Optimal k\n")
cat("--------------------------------------\n")

# Generate new dataset
set.seed(123)
data_elbow <- rbind(
  matrix(rnorm(80, mean = 0), ncol = 2),
  matrix(rnorm(80, mean = 5), ncol = 2),
  matrix(rnorm(80, mean = c(5, 0)), ncol = 2),
  matrix(rnorm(80, mean = c(0, 5)), ncol = 2)
)

# Find optimal k using elbow method
cat("Testing k from 1 to 8...\n")
elbow_results <- elbow_method(data_elbow, max_k = 8, plot = TRUE)

cat("\nElbow method results:\n")
print(elbow_results)


# Example 3: Clustering with different k values
cat("\n\nExample 3: Comparing Different k Values\n")
cat("----------------------------------------\n")

# Generate circular-like clusters
set.seed(456)
angle <- seq(0, 2*pi, length.out = 100)
data_circles <- rbind(
  cbind(cos(angle) + rnorm(100, 0, 0.1),
        sin(angle) + rnorm(100, 0, 0.1)),
  cbind(2*cos(angle) + rnorm(100, 0, 0.1),
        2*sin(angle) + rnorm(100, 0, 0.1))
)

# Try different k values
par(mfrow = c(2, 2))
for (k in c(2, 3, 4, 5)) {
  result <- kmeans_custom(data_circles, k = k, seed = 42)
  visualize_kmeans_2d(data_circles, result,
                      title = paste("k =", k, "| Inertia:", round(result$inertia, 1)))
}
par(mfrow = c(1, 1))


# Example 4: Silhouette Score Analysis
cat("\n\nExample 4: Silhouette Score Analysis\n")
cat("-------------------------------------\n")

# Generate well-separated clusters
set.seed(789)
data_silhouette <- rbind(
  matrix(rnorm(60, mean = 0, sd = 0.5), ncol = 2),
  matrix(rnorm(60, mean = 5, sd = 0.5), ncol = 2),
  matrix(rnorm(60, mean = c(0, 5), sd = 0.5), ncol = 2)
)

# Calculate silhouette scores for different k
cat("Calculating silhouette scores for k = 2 to 6...\n")
silhouette_scores <- numeric(5)
k_range <- 2:6

for (i in 1:5) {
  k <- k_range[i]
  result <- kmeans_custom(data_silhouette, k = k, seed = 42)
  silhouette_scores[i] <- silhouette_score(data_silhouette, result$clusters)
  cat("k =", k, "| Silhouette score:", round(silhouette_scores[i], 4), "\n")
}

# Plot silhouette scores
plot(k_range, silhouette_scores, type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Analysis",
     col = "darkgreen", pch = 19, lwd = 2)
grid()

optimal_k <- k_range[which.max(silhouette_scores)]
cat("\nOptimal k based on silhouette score:", optimal_k, "\n")


# Example 5: High-dimensional data (using Iris dataset)
cat("\n\nExample 5: Iris Dataset Clustering\n")
cat("-----------------------------------\n")

# Load iris dataset (built-in R dataset)
data(iris)
iris_features <- iris[, 1:4]  # Use only numeric features

# Standardize the features
iris_scaled <- scale(iris_features)

# Run k-means with k=3 (iris has 3 species)
result_iris <- kmeans_custom(iris_scaled, k = 3, seed = 42)

cat("Clustering Iris dataset (4 features, 150 samples)\n")
cat("Number of iterations:", result_iris$iterations, "\n")
cat("Total inertia:", round(result_iris$inertia, 2), "\n\n")

cat("Cluster sizes:\n")
print(table(result_iris$clusters))

cat("\nComparison with actual species:\n")
comparison_table <- table(Predicted_Cluster = result_iris$clusters,
                          Actual_Species = iris$Species)
print(comparison_table)

# Calculate accuracy (best matching)
accuracy <- max(
  sum(diag(comparison_table)),
  sum(comparison_table[c(1,2,3), c(1,3,2)]),
  sum(comparison_table[c(1,2,3), c(2,1,3)]),
  sum(comparison_table[c(1,2,3), c(2,3,1)]),
  sum(comparison_table[c(1,2,3), c(3,1,2)]),
  sum(comparison_table[c(1,2,3), c(3,2,1)])
) / nrow(iris)

cat("\nBest matching accuracy:", round(accuracy * 100, 2), "%\n")

# Visualize using first 2 principal components
pca_result <- prcomp(iris_scaled)
iris_pca <- pca_result$x[, 1:2]

# Plot PCA results with clusters
colors <- rainbow(3)
plot(iris_pca[, 1], iris_pca[, 2],
     col = colors[result_iris$clusters],
     pch = 19,
     xlab = paste("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% variance)"),
     ylab = paste("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% variance)"),
     main = "Iris Clustering (PCA Visualization)")
legend("topright",
       legend = paste("Cluster", 1:3),
       col = colors,
       pch = 19,
       cex = 0.8)


# Example 6: Comparison with R's built-in kmeans
cat("\n\nExample 6: Comparison with R's Built-in kmeans()\n")
cat("-------------------------------------------------\n")

# Use the same 2D data from Example 1
result_custom <- kmeans_custom(data_2d, k = 3, seed = 42, n_init = 1)
result_builtin <- kmeans(data_2d, centers = 3, nstart = 1)

cat("Custom implementation:\n")
cat("  Iterations:", result_custom$iterations, "\n")
cat("  Total inertia:", round(result_custom$inertia, 2), "\n")

cat("\nR built-in kmeans():\n")
cat("  Iterations:", result_builtin$iter, "\n")
cat("  Total within SS:", round(result_builtin$tot.withinss, 2), "\n")

cat("\nBoth implementations should give similar results!\n")

cat("\n=== Examples completed successfully! ===\n")
