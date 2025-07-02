library(tidyverse)

# Leggi i dati
df_agg <- read.delim("C:\\Users\\Utente\\Desktop\\MatteoVentura.github.io\\courses\\ordinal_data\\materials\\eurobarometer.txt", check.names = FALSE)  # oppure usa read_csv se il file è .csv

# Numero di osservazioni desiderate per paese
n_obs_per_country <- 100

# Inizializza un dataframe vuoto per i microdati
microdata_df <- data.frame()

# Itera su ogni paese
for (i in 1:nrow(df_agg)) {
  country_data <- df_agg[i, ]
  country_name <- country_data$Country
  
  # Dataframe temporaneo per le osservazioni di questo paese
  current_country_microdata <- data.frame(Country = rep(country_name, n_obs_per_country))
  
  # Itera su ogni item (D19-1 a D19-6)
  for (item_num in 1:6) {
    item_prefix <- paste0("D19-", item_num)
    
    # Estrai le percentuali per le categorie dell'item corrente
    # Seleziona le colonne che iniziano con l'item_prefix e terminano con _1, _2, _3, _4, _dk
    cols_item <- c(paste0(item_prefix, "_1"), paste0(item_prefix, "_2"), 
                   paste0(item_prefix, "_3"), paste0(item_prefix, "_4"), 
                   paste0(item_prefix, "_dk"))
    
    # Estrai le percentuali e convertile in proporzioni
    percentages <- as.numeric(country_data[cols_item])
    proportions <- percentages / 100
    
    # Definisci le categorie
    categories <- c("1", "2", "3", "4", "dk")
    
    # Calcola il numero di osservazioni per ciascuna categoria
    counts <- round(proportions * n_obs_per_country)
    
    # Gestisci gli arrotondamenti per assicurarti che la somma sia esattamente n_obs_per_country
    # Questo è un approccio comune per distribuire la differenza dovuta agli arrotondamenti
    diff <- n_obs_per_country - sum(counts)
    if (diff != 0) {
      # Distribuisci la differenza tra le categorie più grandi (o le prime)
      # in modo casuale per evitare bias sistematici
      if (diff > 0) {
        # Aggiungi 'diff' osservazioni in modo casuale
        indices_to_add <- sample(length(counts), diff, replace = TRUE, prob = proportions)
        for (idx in indices_to_add) {
          counts[idx] <- counts[idx] + 1
        }
      } else { # diff < 0
        # Rimuovi 'abs(diff)' osservazioni in modo casuale
        indices_to_remove <- sample(length(counts), abs(diff), replace = TRUE, prob = proportions)
        for (idx in indices_to_remove) {
          if (counts[idx] > 0) { # Assicurati di non scendere sotto zero
            counts[idx] <- counts[idx] - 1
          }
        }
      }
    }
    
    # Genera le osservazioni per l'item corrente
    item_observations <- character(0)
    for (j in 1:length(categories)) {
      item_observations <- c(item_observations, rep(categories[j], counts[j]))
    }
    
    # Mischia le osservazioni per renderle casuali (importante per l'indipendenza)
    item_observations <- sample(item_observations)
    
    # Aggiungi le osservazioni al dataframe temporaneo del paese
    current_country_microdata[[item_prefix]] <- item_observations
  }
  
  # Aggiungi i microdati del paese al dataframe complessivo
  microdata_df <- bind_rows(microdata_df, current_country_microdata)
}

# Visualizza le prime righe del dataframe di microdati
print(head(microdata_df))

# Verifica le dimensioni del dataframe finale
cat("\nDimensioni del dataset di microdati:", dim(microdata_df), "\n")

# Puoi anche verificare le frequenze per un paese e un item specifico per assicurarti
# che la distribuzione sia simile a quella aggregata (con lievi differenze dovute all'arrotondamento)
cat("\nVerifica delle frequenze per BE, D19-1 (Microdati):\n")
print(table(microdata_df$D19-1[microdata_df$Country == "BE"]))
cat("\nFrequenze originali per BE, D19-1:\n")
print(df_agg[df_agg$Country == "BE", c("D19-1_1", "D19-1_2", "D19-1_3", "D19-1_4", "D19-1_dk")])

save(microdata_df, file = "C:\\Users\\Utente\\Desktop\\MatteoVentura.github.io\\courses\\ordinal_data\\materials\\microdata_eurobarometer.R")

table(microdata_df$`D19-1`[microdata_df$Country == "BE"])
