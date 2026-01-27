# Hello World Visualization in R
# Questo script crea una visualizzazione grafica di "Hello World"

# Installazione e caricamento dei pacchetti necessari
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org/")
library(ggplot2)

# Creazione dei dati per la visualizzazione
# Ogni lettera sarà posizionata in un punto specifico con colori diversi

hello_world <- data.frame(
  letter = strsplit("HELLO WORLD", "")[[1]],
  x = 1:11,
  y = c(5, 5.5, 6, 5.5, 5, 4, 6, 5.5, 5, 5.5, 6),
  size = c(12, 14, 16, 14, 12, 8, 16, 14, 12, 14, 16),
  angle = c(-10, -5, 0, 5, 10, 0, -10, -5, 0, 5, 10)
)

# Creazione della palette di colori arcobaleno
colors <- rainbow(11)

# Creazione della visualizzazione
p <- ggplot(hello_world, aes(x = x, y = y, label = letter, color = letter)) +
  geom_text(aes(size = size, angle = angle), fontface = "bold", show.legend = FALSE) +
  scale_size_identity() +
  scale_color_manual(values = colors) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "darkblue"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50")
  ) +
  labs(
    title = "Hello World Visualization",
    subtitle = "Una visualizzazione colorata creata con R e ggplot2"
  ) +
  xlim(0, 12) +
  ylim(2, 8) +
  # Aggiunta di decorazioni
  annotate("segment", x = 1, xend = 11, y = 3, yend = 3,
           color = "lightblue", size = 2, alpha = 0.5) +
  annotate("point", x = seq(1, 11, by = 2), y = rep(3, 6),
           color = rainbow(6), size = 3, alpha = 0.7)

# Visualizzazione del grafico
print(p)

# Salvataggio del grafico come file PNG
ggsave("hello_world_plot.png", plot = p, width = 10, height = 6, dpi = 150)

# Messaggio di conferma
cat("\n✨ Hello World Visualization creata con successo!\n")
cat("📁 Il grafico è stato salvato come 'hello_world_plot.png'\n")
