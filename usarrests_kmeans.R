#!/usr/bin/env Rscript

# Analisi riproducibile k-means del dataset built-in USArrests.
# Non richiede pacchetti esterni a R.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  getwd()
}
output_dir <- file.path(script_dir, "results", "usarrests_kmeans")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

seed <- 20261004L
nstart <- 100L
k_values <- 2:10

data("USArrests", package = "datasets")
x <- as.matrix(USArrests)
x_scaled <- scale(x)

# Silhouette media implementata in base R: evita dipendenze da pacchetti.
mean_silhouette <- function(x, groups) {
  distances <- as.matrix(dist(x))
  silhouettes <- vapply(seq_len(nrow(x)), function(i) {
    own_group <- groups[i]
    own_members <- which(groups == own_group)
    own_members <- own_members[own_members != i]
    # Per convenzione la silhouette di un cluster singleton e' zero.
    if (!length(own_members)) return(0)
    a_i <- mean(distances[i, own_members])

    other_groups <- setdiff(unique(groups), own_group)
    b_i <- min(vapply(other_groups, function(g) {
      mean(distances[i, groups == g])
    }, numeric(1)))

    if (max(a_i, b_i) == 0) 0 else (b_i - a_i) / max(a_i, b_i)
  }, numeric(1))
  mean(silhouettes)
}

fits <- vector("list", length(k_values))
diagnostics <- data.frame(
  k = k_values,
  total_withinss = NA_real_,
  between_over_total = NA_real_,
  mean_silhouette = NA_real_
)

for (i in seq_along(k_values)) {
  k <- k_values[i]
  # Seed specifico e documentato per rendere riproducibile ogni diagnostica.
  set.seed(seed + k)
  fit <- kmeans(x_scaled, centers = k, nstart = nstart, iter.max = 100)
  fits[[i]] <- fit
  diagnostics$total_withinss[i] <- fit$tot.withinss
  diagnostics$between_over_total[i] <- fit$betweenss / fit$totss
  diagnostics$mean_silhouette[i] <- mean_silhouette(x_scaled, fit$cluster)
}

# Criterio esplicito: massimo coefficiente silhouette medio (k = 2,...,10).
selected_index <- which.max(diagnostics$mean_silhouette)
selected_k <- diagnostics$k[selected_index]
final_fit <- fits[[selected_index]]

assignments <- data.frame(
  state = rownames(x),
  cluster = final_fit$cluster,
  USArrests,
  row.names = NULL,
  check.names = FALSE
)

scaled_centers <- data.frame(
  cluster = seq_len(selected_k),
  final_fit$centers,
  row.names = NULL,
  check.names = FALSE
)
original_centers_matrix <- sweep(final_fit$centers, 2, attr(x_scaled, "scaled:scale"), "*")
original_centers_matrix <- sweep(original_centers_matrix, 2, attr(x_scaled, "scaled:center"), "+")
original_centers <- data.frame(
  cluster = seq_len(selected_k),
  original_centers_matrix,
  row.names = NULL,
  check.names = FALSE
)

write.csv(diagnostics, file.path(output_dir, "diagnostics.csv"), row.names = FALSE)
write.csv(assignments, file.path(output_dir, "assignments.csv"), row.names = FALSE)
write.csv(scaled_centers, file.path(output_dir, "centers_scaled.csv"), row.names = FALSE)
write.csv(original_centers, file.path(output_dir, "centers_original_units.csv"), row.names = FALSE)

cluster_sizes <- tabulate(final_fit$cluster, nbins = selected_k)
summary_lines <- c(
  "K-means su USArrests",
  sprintf("Seed di base: %d", seed),
  sprintf("Standardizzazione: z-score con scale() sulle 4 variabili"),
  sprintf("Candidati valutati: k = %d,...,%d; nstart = %d", min(k_values), max(k_values), nstart),
  "Criterio di selezione: massimo coefficiente silhouette medio",
  sprintf("k selezionato: %d", selected_k),
  sprintf("Silhouette media: %.6f", diagnostics$mean_silhouette[selected_index]),
  sprintf("WSS totale: %.6f", final_fit$tot.withinss),
  sprintf("Quota devianza tra cluster/totale: %.6f", final_fit$betweenss / final_fit$totss),
  sprintf("Dimensioni cluster: %s", paste(sprintf("C%d=%d", seq_len(selected_k), cluster_sizes), collapse = ", ")),
  "",
  "Centroidi nelle unita originali:",
  paste(capture.output(print(original_centers, row.names = FALSE, digits = 4)), collapse = "\n")
)
writeLines(summary_lines, file.path(output_dir, "summary.txt"))

cat(paste(summary_lines, collapse = "\n"), "\n")
