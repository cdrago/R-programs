# =============================================================================
# Algoritmo K-Means da zero in R
# =============================================================================

kmeans_custom <- function(data, k, max_iter = 100, seed = NULL) {
  # Converte in matrice se necessario
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)

  if (k > n) stop("k non puo' essere maggiore del numero di osservazioni")
  if (k < 1) stop("k deve essere almeno 1")

  # Inizializzazione: seleziona k centroidi casuali tra le osservazioni
  if (!is.null(seed)) set.seed(seed)
  idx_iniziali <- sample(seq_len(n), k)
  centroidi <- data[idx_iniziali, , drop = FALSE]

  assegnazioni <- integer(n)

  for (iter in seq_len(max_iter)) {
    assegnazioni_prec <- assegnazioni

    # Passo 1: Assegna ogni punto al centroide piu' vicino
    for (i in seq_len(n)) {
      distanze <- rowSums((sweep(centroidi, 2, data[i, ]))^2)
      assegnazioni[i] <- which.min(distanze)
    }

    # Passo 2: Ricalcola i centroidi come media dei punti assegnati
    for (j in seq_len(k)) {
      membri <- which(assegnazioni == j)
      if (length(membri) > 0) {
        centroidi[j, ] <- colMeans(data[membri, , drop = FALSE])
      }
    }

    # Controlla convergenza
    if (identical(assegnazioni, assegnazioni_prec)) {
      message("Convergenza raggiunta dopo ", iter, " iterazioni")
      break
    }
  }

  # Calcola le statistiche finali
  wcss <- 0
  wcss_per_cluster <- numeric(k)
  for (j in seq_len(k)) {
    membri <- which(assegnazioni == j)
    if (length(membri) > 0) {
      diff <- sweep(data[membri, , drop = FALSE], 2, centroidi[j, ])
      wcss_per_cluster[j] <- sum(diff^2)
    }
  }
  wcss <- sum(wcss_per_cluster)

  list(
    cluster = assegnazioni,
    centroidi = centroidi,
    wcss_totale = wcss,
    wcss_per_cluster = wcss_per_cluster,
    iterazioni = iter,
    k = k
  )
}

# =============================================================================
# Esempio di utilizzo con il dataset iris
# =============================================================================

cat("=== Algoritmo K-Means - Esempio con dataset Iris ===\n\n")

# Usa solo le colonne numeriche di iris
dati <- iris[, 1:4]

# Esegui K-Means con k=3
risultato <- kmeans_custom(dati, k = 3, seed = 42)

cat("Centroidi finali:\n")
colnames(risultato$centroidi) <- colnames(dati)
print(round(risultato$centroidi, 3))

cat("\nDistribuzione dei cluster:\n")
print(table(Cluster = risultato$cluster))

cat("\nWCSS totale:", round(risultato$wcss_totale, 3), "\n")

# Confronto con le specie reali
cat("\nConfronto cluster vs specie reali:\n")
print(table(Cluster = risultato$cluster, Specie = iris$Species))

# =============================================================================
# Metodo del gomito per scegliere k ottimale
# =============================================================================

metodo_gomito <- function(data, k_max = 10, seed = NULL) {
  wcss_valori <- numeric(k_max)
  for (k in seq_len(k_max)) {
    ris <- kmeans_custom(data, k = k, seed = seed)
    wcss_valori[k] <- ris$wcss_totale
  }

  plot(seq_len(k_max), wcss_valori,
       type = "b", pch = 19, col = "steelblue",
       xlab = "Numero di cluster (k)",
       ylab = "WCSS",
       main = "Metodo del gomito")

  wcss_valori
}

cat("\n=== Metodo del gomito ===\n")
wcss <- metodo_gomito(dati, k_max = 8, seed = 42)

# =============================================================================
# Visualizzazione dei cluster (prime 2 componenti principali)
# =============================================================================

pca <- prcomp(dati, scale. = TRUE)
dati_pca <- pca$x[, 1:2]

colori <- c("tomato", "steelblue", "seagreen")[risultato$cluster]

plot(dati_pca,
     col = colori, pch = 19, cex = 1.2,
     xlab = "PC1", ylab = "PC2",
     main = "Cluster K-Means (proiezione PCA)")

# Aggiungi i centroidi proiettati nello spazio PCA
centroidi_pca <- scale(risultato$centroidi,
                       center = pca$center,
                       scale = pca$scale) %*% pca$rotation[, 1:2]
points(centroidi_pca, pch = 4, cex = 3, lwd = 3, col = "black")
legend("topright",
       legend = paste("Cluster", 1:3),
       col = c("tomato", "steelblue", "seagreen"),
       pch = 19)
