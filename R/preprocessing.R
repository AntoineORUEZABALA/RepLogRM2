#' @importFrom caret createDataPartition
#' @importFrom caret dummyVars
#' @importFrom caret preProcess
NULL

#' Impute Missing Values for Numerical Variables
#'
#' @param data A dataframe containing the dataset to process.
#' @param methode A character string specifying the imputation method:
#'   "moyenne" (mean) or "mediane" (median). Default is "moyenne".
#'
#' @return A dataframe with missing numerical values imputed.
#' @export
imputer_numeriques <- function(data, methode = "moyenne") {
  if (!methode %in% c("moyenne", "mediane")) {
    stop("Méthode d'imputation invalide. Choisir 'moyenne' ou 'mediane'.")
  }

  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      if (methode == "moyenne") {
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      } else if (methode == "mediane") {
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      }
    }
  }
  return(data)
}

#' Impute Missing Values for Categorical Variables
#'
#' @param data A dataframe containing the dataset to process.
#' @param methode A character string specifying the imputation method.
#'   Currently, only "mode" is supported. Default is "mode".
#' @return A dataframe with missing categorical values imputed.
#' @export
imputer_categorielles <- function(data, methode = "mode") {
  if (methode != "mode") {
    stop("Méthode d'imputation invalide. Seul 'mode' est pris en charge.")
  }

  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      mode_val <- names(sort(table(data[[col]], useNA = "no"), decreasing = TRUE))[1]
      data[[col]][is.na(data[[col]])] <- mode_val
    }
  }
  return(data)
}

#' Split Data into Training and Testing Sets
#'
#' @param data A dataframe containing the dataset to split.
#' @param cible The name of the target variable (as a character string).
#' @param proportion The proportion of data to allocate to the training set.
#'   Default is 0.8 (80% training, 20% testing).
#' @return A list containing the training and testing sets.
#' @export
split_train_test <- function(data, cible, proportion = 0.8) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Le package 'caret' doit être installé pour utiliser cette fonction.")
  }

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

#' One-Hot Encode Categorical Variables
#'
#' @param data A dataframe containing the dataset to process.
#' @return A dataframe where categorical variables are replaced by one-hot encoded columns.
#' @export
encoder_one_hot <- function(data) {
  cat_vars <- sapply(data, function(col) is.factor(col) || is.character(col))

  if (any(cat_vars)) {
    dummy_model <- dummyVars(" ~ .", data = data, fullRank = TRUE)
    data <- as.data.frame(predict(dummy_model, newdata = data))
  }

  return(data)
}

#' Normalize/Standardize Numerical Variables
#'
#' @param data A dataframe containing the dataset to process.
#' @return A dataframe with normalized numerical variables.
#' @export
normaliser_donnees <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  preprocess_model <- preProcess(numeric_data, method = c("center", "scale"))
  data[sapply(data, is.numeric)] <- predict(preprocess_model, newdata = numeric_data)
  return(data)
}

#' Full Preprocessing Pipeline
#'
#' @param data A dataframe containing the dataset to preprocess.
#' @param cible The name of the target variable (as a character string).
#' @param imputation_num The imputation method for numerical variables.
#' @param imputation_cat The imputation method for categorical variables.
#' @return A fully preprocessed dataframe.
#' @export
pretraitement_complet <- function(data, cible, imputation_num = "moyenne", imputation_cat = "mode") {
  data <- imputer_numeriques(data, methode = imputation_num)
  data <- imputer_categorielles(data, methode = imputation_cat)

  explicatives <- data[, -which(names(data) == cible), drop = FALSE]
  cible_var <- data[[cible]]

  explicatives <- encoder_one_hot(explicatives)
  explicatives <- normaliser_donnees(explicatives)

  data <- cbind(explicatives, cible_var)
  colnames(data)[ncol(data)] <- cible

  return(data)
}
