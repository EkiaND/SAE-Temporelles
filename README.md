# Analyse des Séries Temporelles des Matières Premières

## Description du projet
Ce projet se concentre sur l'analyse des séries temporelles de cotations de plusieurs matières premières, collectées depuis le site [Investing.com](https://www.investing.com). Les données couvrent la période du 1er janvier 2010 à nos jours et comprennent des informations sur :

- **Café** (Futures café US C - Devise USD)
- **Cacao** (Futures cacao US - Devise USD)
- **Jus d'orange** (Futures jus d'orange - Devise USD)
- **Sucre** (Futures sucre Londres - Devise USD)
- **Pétrole** (Futures pétrole Brent - Devise USD)

L'objectif principal est de regrouper, analyser et visualiser les tendances de ces séries temporelles afin de dégager des informations clés sur les variations de cotations.

## Missions

### Mission 1 : Importation et préparation des données
Pour chaque fichier PDF, nous importons les données et les consolidons dans un objet de classe **tibble**. Les variables suivantes sont définies :

- **Date** : Jour de cotation (type : date)
- **Opened Cotation** : Valeur de cotation à l'ouverture des marchés
- **Closed Cotation** : Valeur de cotation à la fermeture des marchés
- **Hightest Cotation** : Valeur maximale de cotation de la journée
- **Lowest Cotation** : Valeur minimale de cotation de la journée

Remarque : La lecture des données peut varier selon les fichiers PDF. Nous utilisons la bibliothèque **tabulapdf** pour faciliter cette étape.

### Mission 2 : Analyse statistique
#### 1. Boxplots annuels
- Création de boxplots annuels pour la variable **Closed Cotation**, en utilisant **facet_wrap()** pour une visualisation consolidée.
- Analyse individuelle et globale des graphiques.

#### 2. Évolution mensuelle moyenne
- Visualisation de l'évolution mensuelle moyenne de la variable **Closed Cotation** pour chaque matière première.
- Ajout d'une courbe de régression lissée et commentaires sur les tendances observées.

#### 3. Taux d'évolution mensuel
- Calcul du taux d'évolution mensuel de la moyenne de **Closed Cotation**.
- Visualisation des évolutions sous forme de diagrammes.
- Analyse des résultats.

#### 4. Association entre café et cacao
- Analyse de la relation entre les valeurs moyennes mensuelles des cotations journalières pour le café et le cacao.
- Détection des données atypiques et étude de l'association avec :
  - Courbe de régression lissée
  - Modélisation par régression linéaire simple
  - Tests statistiques (hypothèse sur la pente)

#### 5. Étude du Brent
Pour le Brent, plusieurs analyses spécifiques sont réalisées :
1. Visualisation de l'évolution de la moyenne mensuelle depuis 2010 avec une courbe de régression lissée.
2. Analyse des saisonnalités pour détecter une composante saisonnière.
3. Modélisation de la tendance depuis 2020 avec une régression linéaire par morceaux.
4. Prévision de la moyenne mensuelle pour les 26 prochains mois avec bande de confiance à 95 %.

## Fichiers

- **Data - Futures café US C.pdf**
- **Data - Futures cacao US.pdf**
- **Data - Futures jus d'orange.pdf**
- **Data - Futures sucre Londres.pdf**
- **Data - Futures pétrole Brent.pdf**

## Technologies utilisées
- **R** : Langage principal pour l'importation, l'analyse et la visualisation des données.
- **tabulapdf** : Bibliothèque pour l'extraction de tables depuis des fichiers PDF.
- **ggplot2** : Création de visualisations graphiques avancées.
- **dplyr**, **tidyr** : Manipulation et transformation des données.

## Installation
1. Cloner le dépôt :
   ```bash
   git clone https://github.com/EkiaND/SAE-Temporelles.git
   ```

## Auteur
- **Rasmata Sawadogo**
- **Courteney Saint-Hubert**
- **Sidy Diop**
- **Romain Lesueur**

