# Fonctions utilitaires de prétraitement
library(fastDummies)

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

# Normalisation
normaliser <- function(data, methode = "z-score") {
    # Normalisation/standardisation des données
    if (methode == "z-score") {
        for (col in names(data)) {
            if (is.numeric(data[[col]])) {
                data[[col]] <- scale(data[[col]])
            }
        }
    } else {
        stop("Méthode de normalisation non reconnue")
    }
    return(data)
}

# Encodage one hot
encodage_one_hot <- function(data) {
    # Encodage one-hot des variables catégorielles
    data <- dummy_cols(data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    return(data)
}
