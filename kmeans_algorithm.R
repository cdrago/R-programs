###########################################################################
#
# K-Means Clustering Algorithm Implementation in R
#
# This script implements the K-Means clustering algorithm from scratch
# and provides examples of its usage.
#
###########################################################################

# K-Means Algorithm Implementation
# 
# Arguments:
#   data: A numeric matrix or data frame where rows are observations and columns are features
#   k: Number of clusters
#   max_iter: Maximum number of iterations (default: 100)
#   seed: Random seed for reproducibility (default: NULL)
#
# Returns:
#   A list containing:
#     - clusters: Vector of cluster assignments for each observation
#     - centers: Matrix of final cluster centers
#     - iterations: Number of iterations performed
#     - wcss: Within-cluster sum of squares

kmeans_algorithm <- function(data, k, max_iter = 100, seed = NULL) {
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Convert data to matrix if it's a data frame
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  
  # Check if data is numeric
  if (!is.numeric(data)) {
    stop("Data must be numeric")
  }
  
  # Get dimensions
  n <- nrow(data)
  p <- ncol(data)
  
  # Check if k is valid
  if (k < 1 || k > n) {
    stop("k must be between 1 and the number of observations")
  }
  
  # Step 1: Initialize cluster centers by randomly selecting k observations
  initial_indices <- sample(1:n, k)
  centers <- data[initial_indices, , drop = FALSE]
  
  # Initialize cluster assignments
  clusters <- rep(0, n)
  
  # Iteration counter
  iter <- 0
  
  # Flag to check convergence
  converged <- FALSE
  
  # Main K-Means loop
  while (iter < max_iter && !converged) {
    iter <- iter + 1
    
    # Store old cluster assignments to check for convergence
    old_clusters <- clusters
    
    # Step 2: Assign each observation to the nearest cluster center
    for (i in 1:n) {
      # Calculate distances to all cluster centers
      distances <- apply(centers, 1, function(center) {
        sqrt(sum((data[i, ] - center)^2))
      })
      
      # Assign to the closest cluster
      clusters[i] <- which.min(distances)
    }
    
    # Step 3: Update cluster centers
    for (j in 1:k) {
      # Get all points assigned to cluster j
      cluster_points <- data[clusters == j, , drop = FALSE]
      
      # If cluster has points, update center as mean of points
      if (nrow(cluster_points) > 0) {
        centers[j, ] <- colMeans(cluster_points)
      }
    }
    
    # Check for convergence (cluster assignments haven't changed)
    if (all(clusters == old_clusters)) {
      converged <- TRUE
    }
  }
  
  # Calculate within-cluster sum of squares (WCSS) using vectorized operations
  wcss <- 0
  for (j in 1:k) {
    cluster_points <- data[clusters == j, , drop = FALSE]
    if (nrow(cluster_points) > 0) {
      # Vectorized calculation: subtract center from all points and sum squared distances
      center_matrix <- matrix(centers[j, ], nrow = nrow(cluster_points), ncol = p, byrow = TRUE)
      distances_sq <- rowSums((cluster_points - center_matrix)^2)
      wcss <- wcss + sum(distances_sq)
    }
  }
  
  # Return results
  return(list(
    clusters = clusters,
    centers = centers,
    iterations = iter,
    wcss = wcss,
    converged = converged
  ))
}


###########################################################################
# Example 1: Simple 2D clustering
###########################################################################

cat("\n===== Example 1: Simple 2D Clustering =====\n\n")

# Generate sample data with 3 clusters
set.seed(123)
n_points <- 150

# Cluster 1: centered around (2, 2)
cluster1 <- data.frame(
  x = rnorm(n_points/3, mean = 2, sd = 0.5),
  y = rnorm(n_points/3, mean = 2, sd = 0.5)
)

# Cluster 2: centered around (8, 3)
cluster2 <- data.frame(
  x = rnorm(n_points/3, mean = 8, sd = 0.5),
  y = rnorm(n_points/3, mean = 3, sd = 0.5)
)

# Cluster 3: centered around (5, 7)
cluster3 <- data.frame(
  x = rnorm(n_points/3, mean = 5, sd = 0.5),
  y = rnorm(n_points/3, mean = 7, sd = 0.5)
)

# Combine all clusters
sample_data <- rbind(cluster1, cluster2, cluster3)

# Run K-Means algorithm
result <- kmeans_algorithm(sample_data, k = 3, seed = 42)

# Display results
cat("Number of iterations:", result$iterations, "\n")
cat("Converged:", result$converged, "\n")
cat("Within-cluster sum of squares:", round(result$wcss, 2), "\n\n")

cat("Cluster Centers:\n")
print(result$centers)

cat("\nCluster assignments (first 20 points):\n")
print(result$clusters[1:20])

cat("\nCluster sizes:\n")
print(table(result$clusters))


###########################################################################
# Example 2: Iris dataset clustering
###########################################################################

cat("\n\n===== Example 2: Iris Dataset Clustering =====\n\n")

# Load the iris dataset (built-in R dataset)
data(iris)

# Use only the numeric features (exclude Species column)
iris_features <- iris[, 1:4]

# Run K-Means with k=3 (since iris has 3 species)
iris_result <- kmeans_algorithm(iris_features, k = 3, seed = 42)

cat("Number of iterations:", iris_result$iterations, "\n")
cat("Converged:", iris_result$converged, "\n")
cat("Within-cluster sum of squares:", round(iris_result$wcss, 2), "\n\n")

cat("Cluster Centers:\n")
print(iris_result$centers)

cat("\nCluster sizes:\n")
print(table(iris_result$clusters))

# Compare with actual species (for reference)
cat("\nActual species distribution:\n")
print(table(iris$Species))

cat("\nCross-tabulation of clusters vs. actual species:\n")
print(table(Cluster = iris_result$clusters, Species = iris$Species))


###########################################################################
# Example 3: Determining optimal number of clusters (Elbow Method)
###########################################################################

cat("\n\n===== Example 3: Elbow Method for Optimal K =====\n\n")

# Test different values of k
k_values <- 1:10
wcss_values <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  result <- kmeans_algorithm(sample_data, k = k_values[i], seed = 42)
  wcss_values[i] <- result$wcss
}

# Display the WCSS values for different k
cat("WCSS values for different k:\n")
wcss_df <- data.frame(k = k_values, WCSS = round(wcss_values, 2))
print(wcss_df)

cat("\nNote: The 'elbow' in the plot would suggest the optimal number of clusters.\n")
cat("For this data, k=3 shows a significant drop in WCSS.\n")


###########################################################################
# Example 4: Comparing with R's built-in kmeans function
###########################################################################

cat("\n\n===== Example 4: Comparison with Built-in kmeans() =====\n\n")

# Run our implementation
our_result <- kmeans_algorithm(sample_data, k = 3, seed = 42)

# Run R's built-in kmeans with same seed for fair comparison
set.seed(42)
builtin_result <- kmeans(sample_data, centers = 3, nstart = 1)

cat("Our implementation WCSS:", round(our_result$wcss, 2), "\n")
cat("Built-in kmeans WCSS:", round(builtin_result$tot.withinss, 2), "\n")

cat("\nOur cluster sizes:\n")
print(table(our_result$clusters))

cat("\nBuilt-in cluster sizes:\n")
print(table(builtin_result$cluster))

cat("\nNote: Results may differ slightly due to initialization and implementation details,\n")
cat("but both should produce similar clustering quality.\n")


###########################################################################
# End of K-Means Algorithm Implementation
###########################################################################
