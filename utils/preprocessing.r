# Fonctions utilitaires de prétraitement
library(fastDummies)
library(caret)

# Imputation
imputer_numeriques <- function(data, methode = "moyenne") {
    # Imputation des valeurs manquantes
    # Différentes stratégies : moyenne, mediane
    if (methode == "moyenne") {
        for (col in names(data)) {
            if (is.numeric(data[[col]])) {
                data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
            }
        }
    } else if (methode == "mediane") {
        for (col in names(data)) {
            if (is.numeric(data[[col]])) {
                data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
            }
        }
    } else {
        stop("Méthode d'imputation non reconnue")
    }
    return(data)
}

imputer_categorielles <- function(data, methode = "mode") {
    # Imputation des valeurs manquantes
    # Stratégie : mode
    if (methode == "mode") {
        for (col in names(data)) {
            if (is.factor(data[[col]]) || is.character(data[[col]])) {
                mode_val <- names(sort(table(data[[col]], useNA = "no"), decreasing = TRUE))[1]
                data[[col]][is.na(data[[col]])] <- mode_val
            }
        }
    } else {
        stop("Méthode d'imputation non reconnue")
    }
    return(data)
}

# Encodage one hot
encodage_one_hot <- function(data) {
    # Encodage one-hot des variables catégorielles
    # Remove_first_dummy = TRUE pour éviter la colinéarité
    # Remove_selected_columns = TRUE pour supprimer les colonnes originales
    data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    return(data)
}

# Séparation train/test
split_train_test <- function(data, cible, proportion = 0.8) {
    index_train <- createDataPartition(
        y = data[[cible]], # Variable sur laquelle la stratification est effectuée
        p = proportion, # proportion pour l'ensemble d'entraînement
        list = FALSE # Renvoyer une liste ou un vecteur d'indices
    )

    # Création des ensembles d'entraînement et de test
    X_train <- data[index_train, -which(names(data) == cible)]
    X_test <- data[-index_train, -which(names(data) == cible)]
    y_train <- data[[cible]][index_train]
    y_test <- data[[cible]][-index_train]

    return(list(X_train = X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}
