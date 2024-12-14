#' ---
#' titre: "Analyse des séries temporelles"
#' author: "Rasmata Sawadogo, Courteney Saint-Hubert, Sidy Diop, Romain Lesieur" 
#' date: "28/11/2024"
#' ---

#=======================================================================#
# IUT GON - Description et prévision de données temporelles
# Données :  A REMPLIR
# Source : A REMPLIR
#=======================================================================#

# Mission 1 

# Liste des librairies nécessaires
libraries <- c("tabulapdf", "tidyverse", "dplyr", "data.table", "ggplot2", "lubridate")

# Fonction pour installer les librairies manquantes
install_if_missing <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# Installation des librairies manquantes
install_if_missing(libraries)

# Chargement des librairies
library(tabulapdf)
library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)

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

# Pour le pétrole
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

# Fusion de toutes les données en un seul dataset
dataset <- bind_rows(
  data_cafe %>% mutate(Product = "Café"),
  data_cacao %>% mutate(Product = "Cacao"),
  data_jus_orange %>% mutate(Product = "Jus d'Orange"),
  data_sucre %>% mutate(Product = "Sucre"),
  data_petrol %>% mutate(Product = "Pétrole")
)

# Affichage des premières lignes pour vérification
head(dataset)
colnames(dataset)

#########################################################################################################

# Mission 2

# Graphique 1 : Boxplots annuels des cotations journalières fermées par produit

# Ajout d'une colonne pour l'année à partir de la date
dataset <- dataset %>%
  mutate(Year = as.integer(format(Date, "%Y")))

# Création des titres personnalisés pour les facettes
product_labels <- c(
  "Café" = "Cotation du Café",
  "Cacao" = "Cotation du Cacao",
  "Jus d'Orange" = "Cotation du Jus d'Orange",
  "Sucre" = "Cotation du Sucre",
  "Pétrole" = "Cotation du Pétrole"
)

# Création des boxplots avec ggplot2
ggplot(dataset, aes(x = as.factor(Year), y = Closed_Cotation, fill = Product)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1) +
  facet_wrap(~ Product, scales = "free_y", labeller = labeller(Product = product_labels)) +
  labs(
    title = "Boxplots annuels des cotations journalières fermées par produit",
    x = "Année",
    y = "Closed Cotation",
    fill = "Produit"
  ) +
  theme_bw() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),  # Mise en forme des titres des facettes
    legend.position = "none"  # Supprime la légende
  )

# Graphique 2 : Évolution moyenne mensuelle des cotations de clôture par matière première

# Création du dataset mensuel
données_mensuelles <- dataset %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Product, Mois) %>%
  summarise(Moyenne_Cotation = mean(Closed_Cotation, na.rm = TRUE))

# Création du graphique de l'évolution moyenne mensuelle
ggplot(données_mensuelles, aes(x = Mois, y = Moyenne_Cotation)) +
  geom_line(alpha = 0.9, color = "#9ACD32", size = 1) +  # Ligne verte pour les données
  geom_smooth(method = "loess", se = FALSE, color = 'black') +  # Courbe de régression noire sans intervalle de confiance
  facet_wrap(~ Product, scales = "free_y") +  # Un graphique par produit
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centre le titre
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90")
  ) +
  labs(
    title = "Évolution moyenne mensuelle des cotations de clôture par matière première",
    x = "Date",
    y = "Cotation moyenne mensuelle"
  )