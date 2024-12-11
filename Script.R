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


# Combine les tables extraites pour chaque produit
# Les données sont initialement sous forme de liste de tables.
# On utilise `rbindlist` pour fusionner toutes les tables d'un même produit en une seule table.

data_cafe <- as.tibble(rbindlist(café_data))
data_cacao <- as.tibble(rbindlist(cacao_data))
data_jus_orange <- as.tibble(rbindlist(jus_orange_data))
data_sucre <- as.tibble(rbindlist(sucre_data))
data_petrol <- as_tibble(rbindlist(petrol_data, fill = TRUE))


# Traitement des tables : renommage des colonnes et conversion des types
# Pour chaque table, les colonnes sont renommées pour refléter les données qu'elles contiennent.
# Les colonnes numériques (cotations) sont converties en type `numeric` et les dates en type `Date`.

data_cafe <- data_cafe %>%
  rename(Date = X1, 
         `Closed_Cotation` = X2, 
         `Opened_Cotation` = X3, 
         `Highest_Cotation` = X4, 
         `Lowest_Cotation` = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Closed_Cotation` = as.numeric(`Closed_Cotation`),
    `Opened_Cotation` = as.numeric(`Opened_Cotation`),
    `Highest_Cotation` = as.numeric(`Highest_Cotation`),
    `Lowest_Cotation` = as.numeric(`Lowest_Cotation`)
  )

# Même traitement appliqué à chaque table

data_cacao <- data_cacao %>%
  rename(Date = X1, 
         `Closed_Cotation` = X2, 
         `Opened_Cotation` = X3, 
         `Highest_Cotation` = X4, 
         `Lowest_Cotation` = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Closed_Cotation` = as.numeric(`Closed_Cotation`),
    `Opened_Cotation` = as.numeric(`Opened_Cotation`),
    `Highest_Cotation` = as.numeric(`Highest_Cotation`),
    `Lowest_Cotation` = as.numeric(`Lowest_Cotation`)
  )

data_jus_orange <- data_jus_orange %>%
  rename(Date = X1, 
         `Closed_Cotation` = X2, 
         `Opened_Cotation` = X3, 
         `Highest_Cotation` = X4, 
         `Lowest_Cotation` = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Closed_Cotation` = as.numeric(`Closed_Cotation`),
    `Opened_Cotation` = as.numeric(`Opened_Cotation`),
    `Highest_Cotation` = as.numeric(`Highest_Cotation`),
    `Lowest_Cotation` = as.numeric(`Lowest_Cotation`)
  )

data_sucre <- data_sucre %>%
  rename(Date = X1, 
         `Closed_Cotation` = X2, 
         `Opened_Cotation` = X3, 
         `Highest_Cotation` = X4, 
         `Lowest_Cotation` = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Closed_Cotation` = as.numeric(`Closed_Cotation`),
    `Opened_Cotation` = as.numeric(`Opened_Cotation`),
    `Highest_Cotation` = as.numeric(`Highest_Cotation`),
    `Lowest_Cotation` = as.numeric(`Lowest_Cotation`)
  )

data_petrol <- data_petrol %>%
  rename(Date = X1, 
         `Closed_Cotation` = X2, 
         `Opened_Cotation` = X3, 
         `Highest_Cotation` = X4, 
         `Lowest_Cotation` = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Closed_Cotation` = as.numeric(`Closed_Cotation`),
    `Opened_Cotation` = as.numeric(`Opened_Cotation`),
    `Highest_Cotation` = as.numeric(`Highest_Cotation`),
    `Lowest_Cotation` = as.numeric(`Lowest_Cotation`)
  )


# Fusionner toutes les données en un seul dataset
# Cette opération crée une table unique contenant les données de tous les produits.
dataset <- bind_rows(data_cacao, data_cafe, data_jus_orange, data_sucre, data_petrol)

# Affichage des premières lignes du dataset fusionné pour vérification
head(dataset)
