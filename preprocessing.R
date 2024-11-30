# Fonctions utilitaires de prétraitement
library(caret)

# Imputation des valeurs manquantes pour les variables numériques
imputer_numeriques <- function(data, methode = "moyenne") {
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

# Imputation des valeurs manquantes pour les variables catégoriques
imputer_categorielles <- function(data, methode = "mode") {
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

# Séparation train/test
split_train_test <- function(data, cible, proportion = 0.8) {
  index_train <- createDataPartition(
    y = data[[cible]], 
    p = proportion, 
    list = FALSE 
  )
  
  X_train <- data[index_train, -which(names(data) == cible)]
  X_test <- data[-index_train, -which(names(data) == cible)]
  y_train <- data[[cible]][index_train]
  y_test <- data[[cible]][-index_train]
  
  return(list(X_train = X_train, X_test = X_test, y_train = y_train, y_test = y_test))
}

# Encodage one-hot des variables qualitatives explicatives
encoder_one_hot <- function(data) {
  # Séparer les variables qualitatives et quantitatives
  cat_vars <- sapply(data, function(col) is.factor(col) || is.character(col))
  
  # Appliquer un encodage one-hot aux variables qualitatives
  if (any(cat_vars)) {
    dummy_model <- dummyVars(" ~ .", data = data, fullRank = TRUE)
    data <- as.data.frame(predict(dummy_model, newdata = data))
  }
  
  return(data)
}

# Normalisation/Standardisation des variables quantitatives
normaliser_donnees <- function(data) {
  # Appliquer la normalisation (centrage et réduction)
  preprocess_model <- preProcess(data, method = c("center", "scale"))
  data <- predict(preprocess_model, newdata = data)
  return(data)
}

# Prétraitement global : Imputation, encodage, normalisation
pretraitement_complet <- function(data, cible, imputation_num = "moyenne", imputation_cat = "mode") {
  # Imputer les valeurs manquantes
  data <- imputer_numeriques(data, methode = imputation_num)
  data <- imputer_categorielles(data, methode = imputation_cat)
  
  # Séparer la variable cible et les variables explicatives
  explicatives <- data[, -which(names(data) == cible), drop = FALSE]
  cible_var <- data[[cible]]
  
  # Encodage one-hot pour les variables qualitatives explicatives
  explicatives <- encoder_one_hot(explicatives)
  
  # Normalisation des variables quantitatives explicatives
  explicatives <- normaliser_donnees(explicatives)
  
  # Reconstituer les données avec la cible
  data <- cbind(explicatives, cible_var)
  colnames(data)[ncol(data)] <- cible
  
  return(data)
}
