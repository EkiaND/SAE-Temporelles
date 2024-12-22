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

petrol_data <- extract_text("Futures pétrole Brent - Données Historiques.pdf")


# Combinaison et traitement des tables pour chaque produit

# Pour le café
data_cafe <- as_tibble(rbindlist(café_data, fill = TRUE)) %>%
  rename(
    Date = X1, 
    Closed_Cotation = X2, 
    Opened_Cotation = X3, 
    Highest_Cotation = X4, 
    Lowest_Cotation = X5
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation)), 2),
    Opened_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation)), 2),
    Highest_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation)), 2),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation)), 2)
  )

# Pour le cacao
data_cacao <- as_tibble(rbindlist(cacao_data, fill = TRUE)) %>%
  rename(
    Date = X1, 
    Closed_Cotation = X2, 
    Opened_Cotation = X3, 
    Highest_Cotation = X4, 
    Lowest_Cotation = X5
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Closed_Cotation))), 2),
    Opened_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Opened_Cotation))), 2),
    Highest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Highest_Cotation))), 2),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Lowest_Cotation))), 2)
  )%>%
  drop_na()


# Pour le jus d'orange
data_jus_orange <- as_tibble(rbindlist(jus_orange_data, fill = TRUE)) %>%
  rename(Date = X1, 
         Closed_Cotation = X2, 
         Opened_Cotation = X3, 
         Highest_Cotation = X4, 
         Lowest_Cotation = X5) %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Closed_Cotation))), 2),
    Opened_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Opened_Cotation))), 2),
    Highest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Highest_Cotation))), 2),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Lowest_Cotation))), 2)
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
    Closed_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Closed_Cotation))), 2),
    Opened_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Opened_Cotation))), 2),
    Highest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Highest_Cotation))), 2),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", gsub("\\.", "", Lowest_Cotation))), 2)
  )

# Pour le pétrole

# Diviser les données en lignes
lines <- unlist(strsplit(petrol_data, "\n"))

# Filtrer les lignes contenant "Page" ou "Futures pétrole Brent" et les ignorer
lines_cleaned <- lines[!grepl("Page|Futures pétrole Brent", lines)]

# Filtrer les lignes contenant des dates 
data_lines <- lines[str_detect(lines, "^[0-9]{2}/[0-9]{2}/[0-9]{4}")]

# Séparer les colonnes en utilisant des espaces multiples comme séparateurs
split_data <- str_split(data_lines, "\\s+", simplify = TRUE)

# Ajouter des noms de colonnes
colnames(split_data) <- c("Date", "Closed_Cotation", "Opened_Cotation", "Highest_Cotation", "Lowest_Cotation")

# Convertir en tibble
split_data_tibble <- as_tibble(split_data)

# Transformation des colonnes
data_petrol <- split_data_tibble %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Closed_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation)), 3),
    Opened_Cotation = round(as.numeric(gsub(",", ".", Opened_Cotation)), 3),
    Highest_Cotation = round(as.numeric(gsub(",", ".", Highest_Cotation)), 3),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", Lowest_Cotation)), 3)
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

### --- Graphique 1 --- ###  Boxplots annuels des cotations journalières fermées par produit

# Ajout d'une colonne pour l'année à partir de la date et suppression des NA
dataset <- dataset %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>% 
  filter(!is.na(Year))

# Création des titres personnalisés pour les facettes
product_labels <- c(
  "Café" = "Cotation du Café",
  "Cacao" = "Cotation du Cacao",
  "Jus d'Orange" = "Cotation du Jus d'Orange",
  "Sucre" = "Cotation du Sucre",
  "Pétrole" = "Cotation du Pétrole"
)

# Création des boxplots 
ggplot(dataset, aes(x = as.factor(Year), y = Closed_Cotation, fill = Product)) +
  geom_boxplot(
    color = "black",           
    outlier.color = "black",   
    outlier.size = 1          
  ) +
  facet_wrap(
    ~ Product, 
    scales = "free_y", 
    labeller = labeller(Product = product_labels)
  ) +
  labs(
    title = "Boxplots annuels des cotations journalières fermées par produit",
    x = "Année",
    y = "Closed Cotation",
    fill = "Produit"           
  ) +
  theme_bw() +  # Fond clair avec bordures
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),     
    strip.text = element_text(face = "bold", size = 10),  
    panel.grid.major.y = element_line(color = "gray90"),  
    legend.position = "none",                             
    plot.title = element_text(face = "bold", hjust = 0.5) 
  )


### --- Graphique 2 --- ###   Évolution moyenne mensuelle des cotations de clôture par matière première

# Création du dataset mensuel
données_mensuelles <- dataset %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Product, Mois) %>%
  summarise(Moyenne_Cotation = mean(Closed_Cotation, na.rm = TRUE))

# Création du graphique de l'évolution moyenne mensuelle
ggplot(données_mensuelles, aes(x = Mois, y = Moyenne_Cotation)) +
  geom_line(alpha = 0.9, color = "#9ACD32", size = 1) +  # Ligne verte pour les données
  geom_smooth(method = "loess", span = 0.5,se = FALSE, color = 'black') +  # Courbe de régression noire sans intervalle de confiance
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



### --- Graphique 3 --- ###  
# Évolution de la moyenne mensuelle de la cotation journalière selon la matière première 


données_mensuelles <- données_mensuelles %>%
  group_by(Product) %>%
  arrange(Mois) %>%
  mutate(
    Taux_Evolution = (Moyenne_Cotation - lag(Moyenne_Cotation)) / lag(Moyenne_Cotation) * 100) %>%
  ungroup()

head(données_mensuelles)

# Création du graphique
ggplot(données_mensuelles, aes(x = Mois, y = Taux_Evolution, color = Product)) +
  geom_line(alpha = 0.9, color="deepskyblue1",size = 0.5) +  # Ligne pour visualiser les variations
  geom_smooth(method = "loess", se = TRUE, color='black',span=0.7,method.args=list(degree=1)) +  # Ligne pour indiquer 0%
  facet_wrap(~ Product, scales = "free_y") +  # Une facette par produit
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Taux d'évolution mensuel des cotations fermées par matière première",
       subtitle = "Analyse basée sur les moyennes mensuelles",
       x = "Mois", y = "Taux d'évolution (%)")


### --- Graphique 4 --- ###
# Association existant entre le café et le cacao

cafe_cacao_data <- données_mensuelles %>%
  filter(Product %in% c("Café", "Cacao")) %>%
  select(Mois, Product, Moyenne_Cotation) %>%
  pivot_wider(names_from = Product, values_from = Moyenne_Cotation, names_prefix = "Moyenne_")

head(cafe_cacao_data)

# Création du graphique
ggplot(cafe_cacao_data, aes(x = Moyenne_Café, y = Moyenne_Cacao)) +
  geom_point(color = "blue", size = 1, alpha = 0.7) +
  geom_smooth(method = "loess", color = "black", se = TRUE, fill = "grey70", alpha = 0.3) +  # Courbe LOESS
  labs(title = "Association entre les moyennes mensuelles des cotations de café et de cacao",
       x = "Moyenne mensuelle des cotations (Café)",y = "Moyenne mensuelle des cotations (Cacao)") +
  theme_minimal()




### --- Graphique 5 --- ###


brent_data <- données_mensuelles %>%
  filter(Product == "Pétrole") %>%
  select(Mois, Moyenne_Cotation)

head(brent_data)

# Création du graphique
ggplot(brent_data, aes(x = Mois, y = Moyenne_Cotation)) +
  geom_line(color = "darkkhaki", size = 0.7) +  # Ligne représentant la cotation
  geom_smooth(method = "loess", color = "red", se = FALSE, fill = "grey70", alpha = 0.3) +  # Courbe lissée LOESS avec intervalle
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Évolution de la cotation moyenne mensuelle du Brent",
       subtitle = "Analyse depuis janvier 2010",
       x = "Date", y = "Cotation moyenne mensuelle")
