#' ---
#' titre: "Analyse des séries temporelles"
#' author: "Rasmata Sawadogo, Courteney Saint-Hubert, Sidy Diop, Romain Lesueur" 
#' date: "28/11/2024"
#' ---

#=======================================================================#
# IUT GON - Description et prévision de données temporelles
# Données :  A REMPLIR
# Source : A REMPLIR
#=======================================================================#

# Mission 1 

# Chargement des librairies
library(tabulapdf)
library(tidyverse)
library(dplyr)
library(data.table)

# Extraction des données des fichiers PDF
café_data <- extract_tables("Futures café US C - Données Historiques.pdf",
                            method = "decide", 
                            encoding = "UTF-8",
                            col_names = FALSE,
                            output = "tibble")


cacao_data <- extract_tables("Futures cacao US - Données Historiques.pdf",
                             method = "decide", 
                             encoding = "UTF-8",
                             col_names = FALSE,
                             output = "tibble")

jus_orange_data <- extract_tables("Futures jus dorange - Données Historiques.pdf",
                                  method = "decide", 
                                  encoding = "UTF-8",
                                  col_names = FALSE,
                                  output = "tibble")

sucre_data <- extract_tables("Futures sucre Londres - Données Historiques.pdf",
                             method = "decide", 
                             encoding = "UTF-8",
                             col_names = FALSE,
                             output = "tibble")

petrol_data <- extract_tables(file = "Futures pétrole Brent - Données Historiques.pdf",
                              method = "decide", 
                              encoding = "UTF-8",
                              col_names = FALSE,
                              output = "tibble")


# Combinaison et traitement des tables pour chaque produit

# Pour le café
data_cafe <- as_tibble(rbindlist(café_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = as.numeric(Closed_Cotation),
    Opened_Cotation = as.numeric(Opened_Cotation),
    Highest_Cotation = as.numeric(Highest_Cotation),
    Lowest_Cotation = as.numeric(Lowest_Cotation)
  )

# Pour le cacao
data_cacao <- as_tibble(rbindlist(cacao_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = as.numeric(Closed_Cotation),
    Opened_Cotation = as.numeric(Opened_Cotation),
    Highest_Cotation = as.numeric(Highest_Cotation),
    Lowest_Cotation = as.numeric(Lowest_Cotation)
  )

# Pour le jus d'orange
data_jus_orange <- as_tibble(rbindlist(jus_orange_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = as.numeric(Closed_Cotation),
    Opened_Cotation = as.numeric(Opened_Cotation),
    Highest_Cotation = as.numeric(Highest_Cotation),
    Lowest_Cotation = as.numeric(Lowest_Cotation)
  )

# Pour le sucre
data_sucre <- as_tibble(rbindlist(sucre_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = as.numeric(Closed_Cotation),
    Opened_Cotation = as.numeric(Opened_Cotation),
    Highest_Cotation = as.numeric(Highest_Cotation),
    Lowest_Cotation = as.numeric(Lowest_Cotation)
  )


# Pour le petrol
data_petrol <- as_tibble(rbindlist(petrol_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = as.numeric(Closed_Cotation),
    Opened_Cotation = as.numeric(Opened_Cotation),
    Highest_Cotation = as.numeric(Highest_Cotation),
    Lowest_Cotation = as.numeric(Lowest_Cotation)
  )



# Ensuite on fusion de toutes les données en un seul dataset
dataset <- bind_rows(
  data_cafe %>% mutate(Product = "Café"),
  data_cacao %>% mutate(Product = "Cacao"),
  data_jus_orange %>% mutate(Product = "Jus d'Orange"),
  data_sucre %>% mutate(Product = "Sucre"),
  data_petrol %>% mutate(Product = "Pétrole")
)

# Affichage des premières lignes pour vérification
head(dataset)
