# K-Means Clustering Algorithm Implementation in R
# Author: Claude
# Description: Implementation of the k-means clustering algorithm from scratch

#' K-Means Clustering Algorithm
#'
#' Performs k-means clustering on a dataset
#'
#' @param data A numeric matrix or data frame where rows are observations and columns are features
#' @param k Number of clusters
#' @param max_iter Maximum number of iterations (default: 100)
#' @param tolerance Convergence tolerance (default: 1e-4)
#' @param n_init Number of times to run k-means with different initializations (default: 10)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list containing:
#'   - clusters: Vector of cluster assignments for each observation
#'   - centers: Matrix of cluster centers
#'   - iterations: Number of iterations until convergence
#'   - wcss: Within-cluster sum of squares
#'   - inertia: Total within-cluster sum of squared distances
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' data <- rbind(
#'   matrix(rnorm(100, mean = 0), ncol = 2),
#'   matrix(rnorm(100, mean = 3), ncol = 2),
#'   matrix(rnorm(100, mean = 6), ncol = 2)
#' )
#'
#' # Run k-means
#' result <- kmeans_custom(data, k = 3)
#' print(result$clusters)
#' print(result$centers)
kmeans_custom <- function(data, k, max_iter = 100, tolerance = 1e-4, n_init = 10, seed = NULL) {

  # Input validation
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }

  # Convert to matrix if data frame
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # Check for numeric data
  if (!is.numeric(data)) {
    stop("Data must be numeric")
  }

  # Remove any rows with missing values
  if (any(is.na(data))) {
    warning("Removing rows with missing values")
    data <- data[complete.cases(data), ]
  }

  n_samples <- nrow(data)
  n_features <- ncol(data)

  # Validate k
  if (k < 1 || k > n_samples) {
    stop(paste("k must be between 1 and", n_samples))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Run k-means multiple times with different initializations
  best_result <- NULL
  best_inertia <- Inf

  for (init in 1:n_init) {
    # Initialize cluster centers randomly by selecting k random data points
    center_indices <- sample(1:n_samples, k)
    centers <- data[center_indices, , drop = FALSE]

    # Initialize cluster assignments
    clusters <- integer(n_samples)
    prev_centers <- centers

    converged <- FALSE
    iter <- 0

    # Iterative optimization
    while (!converged && iter < max_iter) {
      iter <- iter + 1

      # Assignment step: assign each point to nearest centroid
      for (i in 1:n_samples) {
        point <- data[i, ]

        # Calculate distances to all centroids
        distances <- numeric(k)
        for (j in 1:k) {
          distances[j] <- sqrt(sum((point - centers[j, ])^2))
        }

        # Assign to nearest centroid
        clusters[i] <- which.min(distances)
      }

      # Update step: recalculate centroids
      prev_centers <- centers
      for (j in 1:k) {
        cluster_points <- data[clusters == j, , drop = FALSE]

        # Handle empty clusters by reinitializing with random point
        if (nrow(cluster_points) == 0) {
          centers[j, ] <- data[sample(1:n_samples, 1), ]
        } else {
          centers[j, ] <- colMeans(cluster_points)
        }
      }

      # Check for convergence
      center_shift <- sqrt(sum((centers - prev_centers)^2))
      if (center_shift < tolerance) {
        converged <- TRUE
      }
    }

    # Calculate within-cluster sum of squares (inertia)
    inertia <- 0
    wcss <- numeric(k)

    for (j in 1:k) {
      cluster_points <- data[clusters == j, , drop = FALSE]
      if (nrow(cluster_points) > 0) {
        cluster_wcss <- sum(apply(cluster_points, 1, function(point) {
          sum((point - centers[j, ])^2)
        }))
        wcss[j] <- cluster_wcss
        inertia <- inertia + cluster_wcss
      }
    }

    # Keep the best result (lowest inertia)
    if (inertia < best_inertia) {
      best_inertia <- inertia
      best_result <- list(
        clusters = clusters,
        centers = centers,
        iterations = iter,
        wcss = wcss,
        inertia = inertia,
        converged = converged
      )
    }
  }

  return(best_result)
}


#' Elbow Method for Optimal K
#'
#' Helps determine the optimal number of clusters using the elbow method
#'
#' @param data A numeric matrix or data frame
#' @param max_k Maximum number of clusters to test (default: 10)
#' @param plot Whether to plot the elbow curve (default: TRUE)
#'
#' @return A data frame with k values and corresponding inertia
elbow_method <- function(data, max_k = 10, plot = TRUE) {

  inertias <- numeric(max_k)
  k_values <- 1:max_k

  for (k in k_values) {
    result <- kmeans_custom(data, k = k, n_init = 5)
    inertias[k] <- result$inertia
  }

  elbow_data <- data.frame(k = k_values, inertia = inertias)

  if (plot) {
    plot(k_values, inertias, type = "b",
         xlab = "Number of Clusters (k)",
         ylab = "Within-Cluster Sum of Squares (Inertia)",
         main = "Elbow Method for Optimal k",
         col = "blue", pch = 19, lwd = 2)
    grid()
  }

  return(elbow_data)
}


#' Silhouette Score for Cluster Validation
#'
#' Calculate silhouette score for clustering quality assessment
#'
#' @param data A numeric matrix or data frame
#' @param clusters Vector of cluster assignments
#'
#' @return Average silhouette score
silhouette_score <- function(data, clusters) {

  n_samples <- nrow(data)
  unique_clusters <- unique(clusters)
  k <- length(unique_clusters)

  if (k == 1) {
    return(0)
  }

  silhouettes <- numeric(n_samples)

  for (i in 1:n_samples) {
    point <- data[i, ]
    cluster_i <- clusters[i]

    # Calculate average distance to points in same cluster (a)
    same_cluster_points <- data[clusters == cluster_i, , drop = FALSE]
    if (nrow(same_cluster_points) > 1) {
      distances_same <- apply(same_cluster_points, 1, function(p) {
        sqrt(sum((point - p)^2))
      })
      a <- mean(distances_same[distances_same > 0])
    } else {
      a <- 0
    }

    # Calculate average distance to points in nearest other cluster (b)
    b <- Inf
    for (other_cluster in unique_clusters) {
      if (other_cluster != cluster_i) {
        other_cluster_points <- data[clusters == other_cluster, , drop = FALSE]
        distances_other <- apply(other_cluster_points, 1, function(p) {
          sqrt(sum((point - p)^2))
        })
        avg_dist <- mean(distances_other)
        if (avg_dist < b) {
          b <- avg_dist
        }
      }
    }

    # Calculate silhouette for this point
    if (max(a, b) > 0) {
      silhouettes[i] <- (b - a) / max(a, b)
    } else {
      silhouettes[i] <- 0
    }
  }

  return(mean(silhouettes))
}


#' Visualize K-Means Results (2D)
#'
#' Create a scatter plot of the clustering results for 2D data
#'
#' @param data A numeric matrix or data frame with 2 columns
#' @param result Result object from kmeans_custom
#' @param title Plot title (default: "K-Means Clustering Results")
visualize_kmeans_2d <- function(data, result, title = "K-Means Clustering Results") {

  if (ncol(data) != 2) {
    stop("This function only works with 2D data. Use PCA for higher dimensions.")
  }

  # Create color palette
  colors <- rainbow(length(unique(result$clusters)))
  point_colors <- colors[result$clusters]

  # Plot data points
  plot(data[, 1], data[, 2],
       col = point_colors,
       pch = 19,
       xlab = "Feature 1",
       ylab = "Feature 2",
       main = title)

  # Plot cluster centers
  points(result$centers[, 1], result$centers[, 2],
         col = colors,
         pch = 4,
         cex = 3,
         lwd = 3)

  # Add legend
  legend("topright",
         legend = paste("Cluster", 1:nrow(result$centers)),
         col = colors,
         pch = 19,
         cex = 0.8)

  # Add centers to legend
  legend("bottomright",
         legend = "Centers",
         col = "black",
         pch = 4,
         cex = 0.8)
}
