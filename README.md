# README : Projet R M2 SISE

## Description
Création d’un package proposant la méthode Régression logistique pour la classification supervisée avec la prise en charge de variables prédictives mixtes
Il doit pouvoir être installé directement à partir de GitHub et intègrer un fichier d’aide en anglais aux normes R.

Pour l'implémentation la variable cible doit être qualitative. Les « p » variables explicatives (p ≥ 1) sont de types quelconques (quantitatives ou qualitatives),
on doit soit réaliser la préparation adéquate pour harmoniser le type des données avant de procéder aux calculs soit adapter les calculs de manière à gérer directement les données mixtes.
On doit programmer le cœur de l’algorithme en vous appuyant sur la descente de gradient. En revanche, vous pouvez faire appel à des packages externes pour les calculs annexes, il faut cependant s'assurer de l’exactitude des calculs, la robustesse et la rapidité d’exécution.

La classe de calcul est implementée selon la norme R6. Elle doit être constitué d'un constructeur, d'une méthode fit pour modéliser les données d'apprentissage, d'une fonction predict qui renvoie la classe prédite pour chaque individu, une fonction predict_proba qui renvoie les probabilités d’appartenance aux classes, une procédure print qui affiche les informations succintes sur le modèle et une procédure summary qui affiche les informations détaillées du modèle. Nous pouvons également nous servir une série de propriétés qui peuvent être exploitées par la suite.

Pour finir il nous faut également une application R Shiny de notre package qui permet de sélectionner un dataset, de choisir la variable cible et celles qui sont explicatives et enfin lancer les calculs et nous présenter les résultats obtenus.

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
install.packages('Nom du packages')
```

4) Installer le package mimosa que nous avons crée
```bash
install_github("Linn2d/mimosa")
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
Antoine ORUEZABALA 
Linh Nhi LE DINH
Béranger THOMAS
