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
                            dec = ",",
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
    Closed_Cotation = round(as.numeric(gsub(",", ".", Closed_Cotation))/100, 2),
    Opened_Cotation = round(as.numeric(gsub(",", ".", Opened_Cotation))/100, 2),
    Highest_Cotation = round(as.numeric(gsub(",", ".", Highest_Cotation))/100, 2),
    Lowest_Cotation = round(as.numeric(gsub(",", ".", Lowest_Cotation))/100, 2)
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
dataset <- dataset %>% mutate(Product = as.factor(Product))


# Affichage des premières lignes pour vérification
head(dataset)
str(dataset)
colnames(dataset)

#########################################################################################################

# Mission 2

#########################################################################################################

# Boxplots annuels des cotations journalières fermées par produit

#########################################################################################################

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
    fill = "Produit",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  theme_bw() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),     
    strip.text = element_text(face = "bold", size = 10),  
    panel.grid.major.y = element_line(color = "gray90"),  
    legend.position = "none",                             
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5) 
  )

#########################################################################################################

#   Évolution moyenne mensuelle des cotations de clôture par matière première

#########################################################################################################

# Création du dataset mensuel
données_mensuelles <- dataset %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Product, Mois) %>%
  summarise(Moyenne_Cotation = mean(Closed_Cotation, na.rm = TRUE))

# Création du graphique de l'évolution moyenne mensuelle
ggplot(données_mensuelles, aes(x = Mois, y = Moyenne_Cotation)) +
  geom_line(alpha = 0.9, color = "#8BB92D", size = 1) +  
  geom_smooth(method = "loess", span = 0.3,se = FALSE, color = 'black') +  
  facet_wrap(~ Product, scales = "free_y",labeller = labeller(Product = product_labels)
) +  
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5)
  ) +
  labs(
    title = "Évolution moyenne mensuelle des cotations de clôture par matière première",
    x = "Date",
    y = "Cotation moyenne mensuelle",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  )



#########################################################################################################

# Évolution de la moyenne mensuelle de la cotation journalière selon la matière première 

#########################################################################################################

données_mensuelles <- données_mensuelles %>%
  group_by(Product) %>%
  arrange(Mois) %>%
  mutate(
    Taux_Evolution = (Moyenne_Cotation - lag(Moyenne_Cotation)) / lag(Moyenne_Cotation) * 100) %>%
  ungroup()



# Création des titres personnalisés pour les facettes
product_labels2 <- c(
  "Café" = "Variation du café",
  "Cacao" = "Variation du cacao",
  "Jus d'Orange" = "Variation du jus d'orange",
  "Sucre" = "Variation du sucre",
  "Pétrole" = "Variation du pétrole"
)

# Création du graphique
ggplot(données_mensuelles, aes(x = Mois, y = Taux_Evolution, color = Product)) +
  geom_line(alpha = 0.9, color="deepskyblue1",size = 0.5) +  # Ligne pour visualiser les variations
  geom_smooth(method = "loess", se = TRUE, color='black',span=0.7,method.args=list(degree=1)) +  # Ligne pour indiquer 0%
  facet_wrap(~ Product, scales = "free_y",labeller = labeller(Product = product_labels2)
) +  # Une facette par produit
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5)
  ) +
  theme_bw() +  
  labs(title = "Taux d'évolution mensuel des cotations fermées par matière première",
       subtitle = "Analyse basée sur les moyennes mensuelles",
       x = "Mois", y = "Taux d'évolution (%)",
       subtitle = "source : Investing.com",
       caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  )



###########################################################################

# Association existant entre le café et le cacao

###########################################################################

# Préparation des données
moyennes_cafe <- dataset %>%
  filter(Product == "Café") %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Mois) %>%
  summarise(Moyenne_Cafe = mean(Closed_Cotation, na.rm = TRUE))

moyennes_cacao <- dataset %>%
  filter(Product == "Cacao") %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Mois) %>%
  summarise(Moyenne_Cacao = mean(Closed_Cotation, na.rm = TRUE))

# Fusion des données
association_data <- inner_join(moyennes_cafe, moyennes_cacao, by = "Mois")

# Calcul des limites pour le café
IQR_cafe <- IQR(association_data$Moyenne_Cafe, na.rm = TRUE)
limites_cafe <- quantile(association_data$Moyenne_Cafe, c(0.25, 0.75), na.rm = TRUE) + c(-1.5, 1.5) * IQR_cafe

# Calcul des limites pour le cacao
IQR_cacao <- IQR(association_data$Moyenne_Cacao, na.rm = TRUE)
limites_cacao <- quantile(association_data$Moyenne_Cacao, c(0.25, 0.75), na.rm = TRUE) + c(-1.5, 1.5) * IQR_cacao

# Filtrer les valeurs atypiques
outliers <- association_data %>%
  filter(
    Moyenne_Cafe < limites_cafe[1] | Moyenne_Cafe > limites_cafe[2] |
      Moyenne_Cacao < limites_cacao[1] | Moyenne_Cacao > limites_cacao[2]
  )

# Modèle de régression linéaire simple
modele <- lm(Moyenne_Cacao ~ Moyenne_Cafe, data = association_data)
summary(modele)

# Extraction des coefficients du modèle
intercept <- round(coef(modele)[1], 4) 
slope <- round(coef(modele)[2], 4)     
equation <- paste0("Moyenne_Cacao = ", intercept, " + ", slope, " * Moyenne_Cafe") 


# Diagramme de dispersion et corrélation
ggplot(association_data, aes(x = Moyenne_Cafe, y = Moyenne_Cacao)) +
  geom_point(color = "blue", size = 0.5) + 
  geom_point(data = outliers, aes(x = Moyenne_Cafe, y = Moyenne_Cacao), color = "blue", size = 0.7) + 
  geom_text(
    data = outliers, 
    aes(x = Moyenne_Cafe, y = Moyenne_Cacao, label = format(Mois, "%b %Y")), 
    color = "black", 
    size = 3, 
    hjust = 0, 
    vjust = -1
  ) +
  geom_smooth(method = "loess", se = TRUE, color = "black") + 
  labs(
    title = "Association entre les cotations moyennes du café et du cacao",
    x = "Moyenne mensuelle du Café",
    y = "Moyenne mensuelle du Cacao",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  annotate("text", 
           x = min(association_data$Moyenne_Cafe), 
           y = max(association_data$Moyenne_Cacao), 
           label = equation, 
           color = "brown1", 
           hjust = 0, 
           vjust = 1, 
           size = 3) + 
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )


# Corrélation
correlation <- cor(association_data$Moyenne_Cafe, association_data$Moyenne_Cacao, use = "complete.obs")
cat("Coefficient de corrélation :", correlation, "\n")


# Coefficient de détermination
R2 <- summary(modele)$r.squared
cat("Coefficient de détermination (R^2) :", R2, "\n")

### Résumé des résultats ### 
###  Coefficient de corrélation 
# Le coefficient de corrélation entre les moyennes mensuelles des cotations du café 
# et du cacao est 0.4005, indiquant une association modérée positive. Cela signifie que
# les deux variables ont tendance à évoluer dans le même sens, mais la relation n'est pas
# particulièrement forte.

###  Coefficient de détermination 
# Le coefficient de détermination est 0.1604 (16.04%). Cela signifie que 16% de la variabilité
# des cotations moyennes du cacao peut être expliquée par les variations des cotations moyennes
# du café. Les 84% restants sont dus à d'autres facteurs ou à des variations aléatoires.


###  Test d’hypothèse pour la pente
# La valeur de p associée à la pente est 3.04e-08, ce qui est extrêmement significatif (p < 0.05)
# Cela rejette l’hypothèse nulle ( selon laquelle il n’y aurait pas de relation linéaire entre 
# les deux variables. Il existe une relation linéaire significative entre les cotations moyennes
# du café et du cacao.

###########################################################################

# ANALYSE DU BRENT 

###########################################################################

#1.

# Filtrer pour le Brent et calculer les moyennes mensuelles
brent_data <- dataset %>%
  filter(Product == "Pétrole") %>%
  mutate(Mois = floor_date(Date, "month")) %>%
  group_by(Mois) %>%
  summarise(Moyenne_Brent = mean(Closed_Cotation, na.rm = TRUE))


# Visualisation globale
ggplot(brent_data, aes(x = Mois, y = Moyenne_Brent)) +
  geom_line(color = "#8B4513", size = 0.8) +  
  geom_smooth(method = "loess", span = 0.3, se = FALSE, color = "#FF0000", size = 1) +  # Rouge et lissage ajusté
  labs(
    title = "Évolution de la cotation mensuelle du Brent",
    x = "Date",
    y = "Moyenne mensuelle (USD)",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



#2. diagramme des saisonnalités

# Ajouter des colonnes pour l'année et le mois
brent_data <- brent_data %>%
  mutate(
    Annee = year(Mois),
    Mois_simple = factor(month(Mois), levels = 1:12, labels = month.abb)
  )

# Calcul des moyennes mensuelles par année
saisonnalite_annee <- brent_data %>%
  group_by(Annee, Mois_simple) %>%
  summarise(Moyenne_Brent = mean(Moyenne_Brent, na.rm = TRUE)) %>%
  ungroup()


# Graphique des saisonnalités avec courbes par année
ggplot(saisonnalite_annee, aes(x = Mois_simple, y = Moyenne_Brent, color = factor(Annee), group = Annee)) +
  geom_line(size = 1) +  
  labs(
    title = "Saisonnalité des moyennes mensuelles de la cotation du Brent",
    x = "Mois de l'année",
    y = "Moyenne mensuelle (USD)",
    color = "Année",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
  )


#3.

# Filtrer pour les données depuis 2020
brent_2020 <- brent_data %>%
  filter(Mois >= as.Date("2020-01-01"))

# Visualisation avec régression lissée de 2020 à la fin
ggplot(brent_2020, aes(x = Mois, y = Moyenne_Brent)) +
  geom_line(color = "blue", size = 0.8) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, color = "red", size = 0.8) +
  scale_x_date(
    breaks = seq(from = as.Date("2020-06-01"), 
                 to = max(brent_2020$Mois), 
                 by = "6 months"),
    date_labels = "%B %Y",
    name = "Mois de l'année"
  ) +
  labs(
    title = "Comportement de la moyenne mensuelle de cotation journalière du Brent depuis le 1er janvier 2020 ",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0),
    panel.grid.major = element_line(color = "#E5E5E5"),
    axis.title.y = element_blank(),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#4.

# Définir t1 et t2 en termes de Year_Month
t1 <- 2022 + 6 / 12 
t2 <- 2023 + 6 / 12 
brent_2020 <- brent_2020 %>%
  mutate(
    Year_Month = as.numeric(format(Mois, "%Y")) + as.numeric(format(Mois, "%m")) / 12,
    Var_t1 = pmax(0, Year_Month - t1), 
    Var_t2 = pmax(0, Year_Month - t2)  
  )
# Ajuster le modèle
model <- lm(Moyenne_Brent ~ Year_Month + Var_t1 + Var_t2, data = brent_2020)

# Afficher le résumé du modèle
summary(model)

# Ajouter les prédictions pour chaque segment dans les données
brent_2020 <- brent_2020 %>%
  mutate(
    Predicted_Brent = predict(model)  
  )


# Graphique avec les segments de régression
ggplot(brent_2020, aes(x = Mois, y = Moyenne_Brent)) +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(method = "loess", color = "red", size = 1, se = FALSE) +
  geom_line(aes(y = Predicted_Brent), color ="green4", size = 1) +
  geom_vline(xintercept = as.Date(c("2022-06-01", "2023-06-01")), linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "Régression par morceaux avec ruptures en Juin 2022 et Juin 2023",
    x = "Date",
    y = "Moyenne mensuelle du Brent (USD)",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  scale_x_date(
    breaks = seq(from = as.Date("2020-06-01"), 
                 to = max(brent_2020$Mois), 
                 by = "6 months"),
    date_labels = "%B %Y",
    name = "Mois de l'année"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#5.
# Créer les dates pour les 26 prochains mois
future_dates <- seq(from = max(brent_2020$Mois) + months(1), 
                    by = "month", length.out = 26)

#  Créer un nouveau dataframe avec ces dates futures
future_data <- data.frame(Mois = future_dates)
future_data$Year_Month <- as.numeric(format(future_data$Mois, "%Y")) + as.numeric(format(future_data$Mois, "%m")) / 12
future_data$Var_t1 <- pmax(0, future_data$Year_Month - t1)
future_data$Var_t2 <- pmax(0, future_data$Year_Month - t2)

#  Faire la prédiction pour les 26 prochains mois avec l'intervalle de prédiction
future_predictions <- predict(model, newdata = future_data, interval = "prediction")

# Ajouter les prévisions et les intervalles dans le dataframe
future_data$Predicted_Brent <- future_predictions[, "fit"]
future_data$Lower_Brent <- future_predictions[, "lwr"]
future_data$Upper_Brent <- future_predictions[, "upr"]

# Visualiser avec la courbe et la bande de confiance
ggplot() +
  geom_line(data = brent_2020, aes(x = Mois, y = Moyenne_Brent), color = "steelblue", size = 1) +
    geom_smooth(data = brent_2020, aes(x = Mois, y = Moyenne_Brent), method = "loess", color = "red", size = 1, se = FALSE) +
  geom_line(data = future_data, aes(x = Mois, y = Predicted_Brent), color = "darkgreen", size = 1) +
  geom_ribbon(data = future_data, aes(x = Mois, ymin = Lower_Brent, ymax = Upper_Brent), fill = "chocolate3", alpha = 0.2) +
  geom_line(data = future_data, aes(x = Mois, y = Upper_Brent), color = "black", linetype = "dashed", size = 0.3) +
  geom_line(data = future_data, aes(x = Mois, y = Lower_Brent), color = "black", linetype = "dashed", size = 0.3) +
  labs(
    title = "Régression par morceaux avec prévisions et bande de confiance pour les 26 prochains mois",
    x = "Date",
    y = "Moyenne mensuelle du Brent (USD)",
    subtitle = "source : Investing.com",
    caption = c("BUT Science des données - Lisieux", "Auteurs : Rasmata, Romain, Courteney,Sidy")
  ) +
  scale_x_date(
    breaks = seq(from = as.Date("2020-01-01"), to = max(future_data$Mois), by = "1 year"),
    date_labels = "%B %Y", 
    name = "Mois de l'année"
  ) +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = c(0,1), size = 10, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 12, color = "darkblue", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


