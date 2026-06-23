library(igraph)
library(ggplot2)
 
miLista  <- readRDS("Copia de vector_membresia_por_ventana_fil_080.rds")
memberships <- list()
for(i in 1:215){
  memberships[[i]] <- miLista[[i]]
}
dfec <- read.csv("fechas_fil_080.csv", stringsAsFactors = FALSE)
fechas <-  dfec[1:214, 2]
fechas2 <- as.Date(fechas)
 
n_windows  <- length(memberships)
nmi_values <- numeric(n_windows - 1)

for (i in seq_len(n_windows - 1)) {
  m1 <- memberships[[i]]
  m2 <- memberships[[i + 1]]
  n <- min(length(m1), length(m2))  # 296 en el caso problemático
  nmi_values[i] <- igraph::compare(
    m1[seq_len(n)],
    m2[seq_len(n)],
    method = "nmi"
  )
}
 
df_nmi <- data.frame(
  tiempo = fechas2,  
  valor    = nmi_values,
  metrica = "NMI entre particiones consecutivas" 
)

 ggplot(df_nmi, aes(x = tiempo, y = valor, color = metrica)) +
  geom_line(color = "#457B9D", linewidth = 0.8) +
  geom_point(color = "#457B9D", size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title = "NMI entre particiones consecutivas",
    x     = "Ventana t → t+1",
    y     = "NMI (1 = idénticas, 0 = independientes)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 