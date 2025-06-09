
rm(list = ls())
setwd("C:\\Users\\Utente\\Desktop\\MatteoVentura.github.io\\courses\\ordinal_data\\images")

m = 7
pi1 <- 0.7; xi1 <- 0.8
pi <- pi1
xi <- xi1

library(ggplot2)
library(CUB)

CUB <- probcub00(m, pi, xi)
df <- data.frame(
  Modalità = 1:7,
  Frequenza = CUB
)
# Calcola 1 - pi e 1 - xi
one_minus_pi <- round(1 - pi, 2)
one_minus_xi <- round(1 - xi, 2)

# Titolo con i simboli greci
titolo <- bquote("Model A     " (1 - pi) * " = " * .(one_minus_pi) * ",   " *
                    (1 - xi) * " = " * .(one_minus_xi))
p <- ggplot(df, aes(x = Modalità, y = Frequenza)) +
  geom_linerange(aes(ymin = 0, ymax = Frequenza), color = "black") +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  ylim(0,0.5) + 
  labs(title = titolo, x = "", y = "Frequencies") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "cartesian_barplot1.jpg",
       plot = p,
       width = 5, height = 5, units = "in", dpi = 300)



pi2 <- 0.8; xi2 <- 0.2
pi <- pi2
xi <- xi2
library(ggplot2)
library(CUB)
CUB <- probcub00(m, pi, xi)
df <- data.frame(
  Modalità = 1:7,
  Frequenza = CUB
)
# Calcola 1 - pi e 1 - xi
one_minus_pi <- round(1 - pi, 2)
one_minus_xi <- round(1 - xi, 2)

# Titolo con i simboli greci
titolo <- bquote("Model B     " (1 - pi) * " = " * .(one_minus_pi) * ",   " *
                   (1 - xi) * " = " * .(one_minus_xi))
p <- ggplot(df, aes(x = Modalità, y = Frequenza)) +
  geom_linerange(aes(ymin = 0, ymax = Frequenza), color = "black") +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  ylim(0,0.5) + 
  labs(title = titolo, x = "", y = "Frequencies") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "cartesian_barplot2.jpg",
       plot = p,
       width = 5, height = 5, units = "in", dpi = 300)



pi3 <- 0.3; xi3 <- 0.7
pi <- pi3
xi <- xi3
library(ggplot2)
library(CUB)
CUB <- probcub00(m, pi, xi)
df <- data.frame(
  Modalità = 1:7,
  Frequenza = CUB
)
# Calcola 1 - pi e 1 - xi
one_minus_pi <- round(1 - pi, 2)
one_minus_xi <- round(1 - xi, 2)

# Titolo con i simboli greci
titolo <- bquote("Model C     " (1 - pi) * " = " * .(one_minus_pi) * ",   " *
                   (1 - xi) * " = " * .(one_minus_xi))
p <- ggplot(df, aes(x = Modalità, y = Frequenza)) +
  geom_linerange(aes(ymin = 0, ymax = Frequenza), color = "black") +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  ylim(0,0.5) + 
  labs(title = titolo, x = "", y = "Frequencies") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "cartesian_barplot3.jpg",
       plot = p,
       width = 5, height = 5, units = "in", dpi = 300)


pi4 <- 0.2; xi4 <- 0.3
pi <- pi4
xi <- xi4
library(ggplot2)
library(CUB)
CUB <- probcub00(m, pi, xi)
df <- data.frame(
  Modalità = 1:7,
  Frequenza = CUB
)
# Calcola 1 - pi e 1 - xi
one_minus_pi <- round(1 - pi, 2)
one_minus_xi <- round(1 - xi, 2)

# Titolo con i simboli greci
titolo <- bquote("Model D     " (1 - pi) * " = " * .(one_minus_pi) * ",   " *
                   (1 - xi) * " = " * .(one_minus_xi))
p <- ggplot(df, aes(x = Modalità, y = Frequenza)) +
  geom_linerange(aes(ymin = 0, ymax = Frequenza), color = "black") +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  ylim(0,0.5) + 
  labs(title = titolo, x = "", y = "Frequencies") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "cartesian_barplot4.jpg",
       plot = p,
       width = 5, height = 5, units = "in", dpi = 300)




coord_mat <- cbind(c("Model A", "Model B", "Model C", "Model D"),
                   c(pi1,pi2,pi3,pi4),
                   c(xi1,xi2,xi3,xi4))
# Trasforma in data.frame e assegna nomi alle colonne
df <- as.data.frame(coord_mat, stringsAsFactors = FALSE)
colnames(df) <- c("label", "x", "y")

# Converte x e y in numerici
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)

# Crea il piano cartesiano con i punti e le linee tratteggiate
plot <- ggplot(df, aes(x = (1-x), y = (1-y), label = label)) +
  geom_point(size = 3) +
  geom_text(vjust = -1, hjust = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  labs(x = expression((1-pi)), y = expression((1-xi))) +
  theme(panel.grid = element_blank())

ggsave(filename = "cartesianplane.jpg",
       plot = plot,
       width = 7, height = 7, units = "in", dpi = 300)




### Discrete uniform

library(ggplot2)

# Dati per una distribuzione uniforme discreta su 1:7
df <- data.frame(
  Modalità = 1:7,
  Frequenza = rep(1/7, 7)  # frequenza uniforme
)

# Titolo del grafico
titolo <- "Dscrete Uniform"

# Costruzione del grafico
p <- ggplot(df, aes(x = Modalità, y = Frequenza)) +
  geom_linerange(aes(ymin = 0, ymax = Frequenza), color = "black") +
  scale_x_continuous(breaks = 1:7) +
  theme_bw() +
  ylim(0, 0.5) + 
  labs(title = titolo, x = "", y = "Frequencies") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Salvataggio
ggsave(filename = "uniform.jpg",
       plot = p,
       width = 5, height = 5, units = "in", dpi = 300)

