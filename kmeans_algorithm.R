# =============================================================================
# Algoritmo K-Means implementato da zero in R
# =============================================================================
# Questo script implementa l'algoritmo K-Means di clustering senza usare
# funzioni built-in come kmeans(). Include visualizzazione e esempi.
# =============================================================================

# -----------------------------------------------------------------------------
# Funzione: calcola_distanza_euclidea
# Calcola la distanza euclidea tra due punti
# -----------------------------------------------------------------------------
calcola_distanza_euclidea <- function(punto1, punto2) {
  sqrt(sum((punto1 - punto2)^2))
}

# -----------------------------------------------------------------------------
# Funzione: inizializza_centroidi
# Inizializza k centroidi usando il metodo k-means++ per una migliore convergenza
# -----------------------------------------------------------------------------
inizializza_centroidi <- function(dati, k, metodo = "kmeans++") {
  n <- nrow(dati)

  if (metodo == "random") {
    # Selezione casuale di k punti come centroidi iniziali
    indici <- sample(1:n, k)
    centroidi <- dati[indici, , drop = FALSE]
  } else if (metodo == "kmeans++") {
    # Inizializzazione k-means++ per una migliore distribuzione iniziale
    centroidi <- matrix(NA, nrow = k, ncol = ncol(dati))

    # Seleziona il primo centroide casualmente
    centroidi[1, ] <- as.numeric(dati[sample(1:n, 1), ])

    # Seleziona i centroidi successivi con probabilita' proporzionale alla distanza
    for (i in 2:k) {
      distanze_min <- apply(dati, 1, function(punto) {
        min(apply(centroidi[1:(i-1), , drop = FALSE], 1, function(c) {
          calcola_distanza_euclidea(punto, c)
        }))
      })
      probabilita <- distanze_min^2 / sum(distanze_min^2)
      nuovo_indice <- sample(1:n, 1, prob = probabilita)
      centroidi[i, ] <- as.numeric(dati[nuovo_indice, ])
    }
    centroidi <- as.data.frame(centroidi)
    colnames(centroidi) <- colnames(dati)
  }

  return(centroidi)
}

# -----------------------------------------------------------------------------
# Funzione: assegna_cluster
# Assegna ogni punto al cluster del centroide piu' vicino
# -----------------------------------------------------------------------------
assegna_cluster <- function(dati, centroidi) {
  n <- nrow(dati)
  k <- nrow(centroidi)
  assegnazioni <- integer(n)

  for (i in 1:n) {
    distanze <- numeric(k)
    for (j in 1:k) {
      distanze[j] <- calcola_distanza_euclidea(dati[i, ], centroidi[j, ])
    }
    assegnazioni[i] <- which.min(distanze)
  }

  return(assegnazioni)
}

# -----------------------------------------------------------------------------
# Funzione: aggiorna_centroidi
# Ricalcola i centroidi come media dei punti assegnati a ciascun cluster
# -----------------------------------------------------------------------------
aggiorna_centroidi <- function(dati, assegnazioni, k) {
  nuovi_centroidi <- matrix(NA, nrow = k, ncol = ncol(dati))

  for (j in 1:k) {
    punti_cluster <- dati[assegnazioni == j, , drop = FALSE]
    if (nrow(punti_cluster) > 0) {
      nuovi_centroidi[j, ] <- colMeans(punti_cluster)
    } else {
      # Se un cluster e' vuoto, reinizializza con un punto casuale
      nuovi_centroidi[j, ] <- as.numeric(dati[sample(1:nrow(dati), 1), ])
    }
  }

  nuovi_centroidi <- as.data.frame(nuovi_centroidi)
  colnames(nuovi_centroidi) <- colnames(dati)
  return(nuovi_centroidi)
}

# -----------------------------------------------------------------------------
# Funzione: calcola_wcss
# Calcola la Within-Cluster Sum of Squares (inerzia)
# -----------------------------------------------------------------------------
calcola_wcss <- function(dati, centroidi, assegnazioni) {
  wcss <- 0
  for (i in 1:nrow(dati)) {
    cluster <- assegnazioni[i]
    wcss <- wcss + calcola_distanza_euclidea(dati[i, ], centroidi[cluster, ])^2
  }
  return(wcss)
}

# -----------------------------------------------------------------------------
# Funzione principale: kmeans_custom
# Implementazione completa dell'algoritmo K-Means
# -----------------------------------------------------------------------------
kmeans_custom <- function(dati, k, max_iter = 100, tolleranza = 1e-6,
                          metodo_init = "kmeans++", verbose = FALSE) {

  # Validazione input
  if (!is.data.frame(dati) && !is.matrix(dati)) {
    stop("I dati devono essere un data.frame o una matrice")
  }
  if (k < 1 || k > nrow(dati)) {
    stop("k deve essere compreso tra 1 e il numero di osservazioni")
  }

  # Converti in data.frame se necessario
  dati <- as.data.frame(dati)

  # Inizializza i centroidi
  centroidi <- inizializza_centroidi(dati, k, metodo = metodo_init)

  # Variabili per tracciare la convergenza
  iter <- 0
  convergenza <- FALSE
  storia_wcss <- numeric()

  while (iter < max_iter && !convergenza) {
    iter <- iter + 1

    # Passo 1: Assegna ogni punto al cluster piu' vicino
    assegnazioni <- assegna_cluster(dati, centroidi)

    # Passo 2: Calcola WCSS corrente
    wcss_corrente <- calcola_wcss(dati, centroidi, assegnazioni)
    storia_wcss <- c(storia_wcss, wcss_corrente)

    # Passo 3: Aggiorna i centroidi
    nuovi_centroidi <- aggiorna_centroidi(dati, assegnazioni, k)

    # Passo 4: Verifica convergenza
    spostamento <- sum(sapply(1:k, function(j) {
      calcola_distanza_euclidea(centroidi[j, ], nuovi_centroidi[j, ])
    }))

    if (verbose) {
      cat(sprintf("Iterazione %d: WCSS = %.4f, Spostamento centroidi = %.6f\n",
                  iter, wcss_corrente, spostamento))
    }

    if (spostamento < tolleranza) {
      convergenza <- TRUE
    }

    centroidi <- nuovi_centroidi
  }

  # Assegnazione finale
  assegnazioni <- assegna_cluster(dati, centroidi)
  wcss_finale <- calcola_wcss(dati, centroidi, assegnazioni)

  # Calcola dimensione di ogni cluster
  dimensioni_cluster <- table(assegnazioni)

  # Risultato
  risultato <- list(
    cluster = assegnazioni,
    centroidi = centroidi,
    wcss = wcss_finale,
    storia_wcss = storia_wcss,
    iterazioni = iter,
    convergenza = convergenza,
    k = k,
    dimensioni_cluster = dimensioni_cluster
  )

  class(risultato) <- "kmeans_custom"
  return(risultato)
}

# -----------------------------------------------------------------------------
# Funzione: print.kmeans_custom
# Metodo print per oggetti kmeans_custom
# -----------------------------------------------------------------------------
print.kmeans_custom <- function(x, ...) {
  cat("=== Risultati K-Means ===\n")
  cat(sprintf("Numero di cluster (k): %d\n", x$k))
  cat(sprintf("Iterazioni: %d\n", x$iterazioni))
  cat(sprintf("Convergenza raggiunta: %s\n", ifelse(x$convergenza, "Si", "No")))
  cat(sprintf("WCSS (Within-Cluster Sum of Squares): %.4f\n", x$wcss))
  cat("\nDimensioni cluster:\n")
  print(x$dimensioni_cluster)
  cat("\nCentroidi:\n")
  print(x$centroidi)
}

# -----------------------------------------------------------------------------
# Funzione: metodo_gomito
# Trova il numero ottimale di cluster usando il metodo del gomito
# -----------------------------------------------------------------------------
metodo_gomito <- function(dati, k_max = 10, ...) {
  wcss_valori <- numeric(k_max)

  for (k in 1:k_max) {
    risultato <- kmeans_custom(dati, k, ...)
    wcss_valori[k] <- risultato$wcss
  }

  return(list(k = 1:k_max, wcss = wcss_valori))
}

# -----------------------------------------------------------------------------
# Funzione: visualizza_cluster
# Visualizza i risultati del clustering (per dati 2D)
# -----------------------------------------------------------------------------
visualizza_cluster <- function(dati, risultato, titolo = "Clustering K-Means") {
  if (ncol(dati) < 2) {
    stop("Servono almeno 2 variabili per la visualizzazione")
  }

  # Usa le prime due colonne per la visualizzazione
  x <- dati[, 1]
  y <- dati[, 2]

  # Colori per i cluster
  colori <- rainbow(risultato$k)
  colori_punti <- colori[risultato$cluster]

  # Plot dei punti
  plot(x, y, col = colori_punti, pch = 19, cex = 1.2,
       main = titolo,
       xlab = colnames(dati)[1],
       ylab = colnames(dati)[2])

  # Aggiungi i centroidi
  points(risultato$centroidi[, 1], risultato$centroidi[, 2],
         col = "black", pch = 4, cex = 2, lwd = 3)
  points(risultato$centroidi[, 1], risultato$centroidi[, 2],
         col = colori, pch = 19, cex = 1.5)

  # Legenda
  legend("topright", legend = paste("Cluster", 1:risultato$k),
         col = colori, pch = 19, cex = 0.8)
}

# -----------------------------------------------------------------------------
# Funzione: visualizza_gomito
# Visualizza il grafico del metodo del gomito
# -----------------------------------------------------------------------------
visualizza_gomito <- function(risultato_gomito) {
  plot(risultato_gomito$k, risultato_gomito$wcss, type = "b",
       pch = 19, col = "blue", lwd = 2,
       main = "Metodo del Gomito",
       xlab = "Numero di Cluster (k)",
       ylab = "WCSS (Within-Cluster Sum of Squares)")
  grid()
}

# =============================================================================
# ESEMPIO DI UTILIZZO
# =============================================================================

# Genera dati di esempio con 3 cluster naturali
set.seed(42)

# Cluster 1: centrato in (2, 2)
cluster1 <- data.frame(
  x = rnorm(50, mean = 2, sd = 0.5),
  y = rnorm(50, mean = 2, sd = 0.5)
)

# Cluster 2: centrato in (6, 6)
cluster2 <- data.frame(
  x = rnorm(50, mean = 6, sd = 0.5),
  y = rnorm(50, mean = 6, sd = 0.5)
)

# Cluster 3: centrato in (2, 6)
cluster3 <- data.frame(
  x = rnorm(50, mean = 2, sd = 0.5),
  y = rnorm(50, mean = 6, sd = 0.5)
)

# Combina i dati
dati_esempio <- rbind(cluster1, cluster2, cluster3)

# Esegui K-Means con k=3
cat("\n========================================\n")
cat("Esecuzione K-Means con k=3\n")
cat("========================================\n")

risultato <- kmeans_custom(dati_esempio, k = 3, verbose = TRUE)
print(risultato)

# Visualizza i risultati (se in ambiente grafico)
if (interactive()) {
  par(mfrow = c(1, 2))

  # Plot dei cluster
  visualizza_cluster(dati_esempio, risultato, "Clustering K-Means (k=3)")

  # Metodo del gomito per trovare k ottimale
  cat("\n========================================\n")
  cat("Metodo del Gomito (k da 1 a 8)\n")
  cat("========================================\n")

  gomito <- metodo_gomito(dati_esempio, k_max = 8)
  visualizza_gomito(gomito)

  par(mfrow = c(1, 1))
}

cat("\n========================================\n")
cat("Script completato con successo!\n")
cat("========================================\n")
