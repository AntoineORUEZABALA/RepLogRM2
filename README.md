# MIMOSA : A Mixed Input Multinomial Optimization for Statistical Analysis

## Description

MIMOSA (for "Mixed Inputs Multinomial Optimization for Statistical Analysis") provides a custom implementation of logistic regression models, supporting both binary and multinomial classification tasks. The package handles explanatory variables of mixed types, including both quantitative and qualitative variables. It includes preprocessing utilities for missing value imputation, one-hot encoding of categorical variables, and normalization of numerical variables. Designed for flexibility and ease of use, MIMOSA is suitable for a wide range of logistic regression applications.

## Installation

Make sure you have R (>= 4.0.0) installed. The package relies on the following dependencies:

R6
caret
ggplot2
pmml

If needed, you can install the required dependencies separately :
```bash
library(devtools)
install.packages(c("R6", "caret", "ggplot2, "pmml"))
```
To install the package, please use the following code lines:
```bash
library(devtools)
install_github("AntoineORUEZABALA/RepLogRM2")
```
And then to import it :
```bash
library(mimosa)
```

## Documentation

Création d’un package proposant la méthode Régression logistique pour la classification supervisée avec la prise en charge de variables prédictives mixtes.

Le coeur du package est un algorithme s'appuie sur une descente de gradient pour réaliser la régression. Nous l'avons nommé MIMOSA, pour Mixed Inputs Multinomial Optimization for Statistical Analysis.

Le package peut être installé directement à partir de GitHub. Il intègrer un fichier d’aide en anglais aux normes R.

Pour l'implémentation la variable cible doit être qualitative. Les « p » variables explicatives (p ≥ 1) sont de types quelconques (quantitatives ou qualitatives).

Cette distribution inclut également une application RShiny qui exploite le package Mimosa. Lors de l'utilisation de l'application, la préparation des données n'est pas nécessaire, elle sera réalisé par l'application elle même.

En revanche pour utiliser le package indépendemment, il faut réaliser la préparation adéquate :
- imputation des valeurs manquantes, numériques et catégorielles,
- normalisation (centrage et réduction) des données numériques,
- encodage disjonctif des données catégorielles.

La classe de calcul est implementée selon la norme R6. Elle est constitué d'un constructeur, d'une méthode fit pour modéliser les données d'apprentissage, d'une fonction predict qui renvoie la classe prédite pour chaque individu, une fonction predict_proba qui renvoie les probabilités d’appartenance aux classes, une procédure print qui affiche les informations succintes sur le modèle et une procédure summary qui affiche les informations détaillées du modèle.

## Table des Matières
- [Description](#description)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Description de l'application RShiny](#description-de-lapplication-rshiny)
- [Contribution](#contribution)
- [Auteurs](#auteurs)  

## Installation
Pour installer ce projet, suivez ces étapes :

1) Assurez vous d'avoir RStudio installé sur votre machine.

2) Clonez le dépôt :
```bash
git clone https://github.com/AntoineORUEZABALA/RepLogRM2
```

3) Installez les librairies nécessaires au package (si ce n'est pas déjà fait) :
```bash
install.packages('R6')
install.packages('ggplot2')
install.packages('caret')
```

4) Installer le package mimosa que nous avons créé
```bash
install_github("AntoineORUEZABALA/RepLogRM2")
```

## Utilisation
Ce projet étant une application RShiny.

Si vous voulez la lancer :
- Ouvrez le fichier app.R
- Appuyez sur le bouton "Run App"

## Description de l'application RShiny
L'application s'organise en 4 onglets : Aperçu des données, Résumé statistique, Régression Logistique avec GLM/Multinom et Régression Logistique avec Mimosa.  
- Aperçu des données présente les informations du dataset et un tableau interactif qui affiche les données
- Résumé statistique permet d'afficher les statistiques des différentes variables du jeu de données (Minimum, 1er Quartile, Médiane, Moyenne, 3ème Quartile et Maximum).
- Régression Logistique avec GLM/Multinom effectue la régression logistique via la fonction GLM pour des variables binaires ou via la fonction Multinom pour des variables multinomiales.
- Régression Logistique avec Mimosa effectue la régression logistique via la méthode MIMOSA dont on a crée un package.

## Contribution
Les contributions sont les bienvenues ! Pour contribuer :
- Forkez le projet.
- Créez votre branche de fonctionnalité (```git checkout -b feature/NomFonctionnalite```).
- Commitez vos changements (```git commit -m 'Description des modifications'```).
- Poussez à la branche (```git push origin feature/NomFonctionnalite```).
- Ouvrez une Pull Request.

  
## Auteurs
- Antoine ORUEZABALA
- Linh Nhi LE DINH
- Béranger THOMAS
