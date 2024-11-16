# Fonction pour normaliser les données numériques
normalize <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    return(rep(0, length(x))) # Retourner des zéros si pas de variation
  }
  # Enlever la moyenne et diviser par l'écart-type
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Fonction pour remplacer les valeurs manquantes par la moyenne/mode
replace_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    mode_val <- names(sort(table(x), decreasing = TRUE))[1]
    x[is.na(x)] <- mode_val
  }
  return(x)
}

# Fonction sigmoïde stabilisée numériquement
sigmoid <- function(x) {
  # Clipper les valeurs extrêmes pour éviter l'overflow
  x <- pmax(pmin(x, 100), -100)
  return(1 / (1 + exp(-x)))
}

# Fonction de coût stabilisée
compute_cost <- function(X, y, theta) {
  m <- nrow(X)
  # Calcul stabilisé de la prédiction
  z <- X %*% theta
  z <- pmax(pmin(z, 100), -100) # Clipper les valeurs
  h <- sigmoid(z)

  # Éviter log(0) et log(1)
  epsilon <- 1e-15
  h <- pmax(pmin(h, 1 - epsilon), epsilon)

  # Calcul du coût avec vérification numérique
  cost <- -(1 / m) * sum(y * log(h) + (1 - y) * log(1 - h))

  # Retourner 0 si le coût est NA ou Inf
  if (is.na(cost) || is.infinite(cost)) {
    return(0)
  }
  return(cost)
}

# Fonction de descente de gradient pour la régression logistique avec optimisation Adam
multinomial_logistic_regression_adam <- function(X, y, learning_rate = 0.01, n_iterations = 1000,
                                                 beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8) {
  m <- nrow(X)
  n <- ncol(X)
  K <- length(unique(y))

  # Initialiser les paramètres theta pour chaque classe
  theta <- matrix(0, nrow = n, ncol = K)
  cost_history <- numeric(n_iterations)

  # Initialisation des moments pour Adam
  m_t <- matrix(0, nrow = n, ncol = K) # Premier moment
  v_t <- matrix(0, nrow = n, ncol = K) # Second moment

  # Convertir y en matrice one-hot
  y_onehot <- matrix(0, nrow = m, ncol = K)
  for (i in 1:m) {
    y_onehot[i, y[i] + 1] <- 1
  }

  for (iter in 1:n_iterations) {
    # Calcul des scores
    scores <- as.matrix(sapply(X, as.numeric)) %*% theta

    # Softmax stable numériquement
    scores_exp <- exp(scores - apply(scores, 1, max))
    probs <- scores_exp / rowSums(scores_exp)

    # Gradient
    gradient <- t(as.matrix(sapply(X, as.numeric))) %*% (probs - y_onehot) / m

    # Mise à jour avec Adam
    m_t <- beta1 * m_t + (1 - beta1) * gradient
    v_t <- beta2 * v_t + (1 - beta2) * gradient^2

    # Correction du biais
    m_hat <- m_t / (1 - beta1^iter)
    v_hat <- v_t / (1 - beta2^iter)

    # Mise à jour des paramètres
    theta <- theta - learning_rate * m_hat / (sqrt(v_hat) + epsilon)

    # Calcul du coût (log loss)
    cost_history[iter] <- -sum(y_onehot * log(pmax(probs, 1e-15))) / m

    if (iter %% 100 == 0) {
      cat("Itération", iter, "- Coût:", cost_history[iter], "\n")
    }
  }

  list(theta = theta, cost_history = cost_history)
}

# Fonction de régression logistique multinomiale avec descente de gradient
multinomial_logistic_regression <- function(X, y, learning_rate = 0.01, n_iterations = 1000) {
  m <- nrow(X)
  n <- ncol(X)
  K <- length(unique(y))

  # Initialiser les paramètres theta pour chaque classe
  theta <- matrix(0, nrow = n, ncol = K)
  cost_history <- numeric(n_iterations)

  # Convertir y en matrice one-hot
  y_onehot <- matrix(0, nrow = m, ncol = K)
  for (i in 1:m) {
    y_onehot[i, y[i] + 1] <- 1
  }

  for (iter in 1:n_iterations) {
    # Calcul des scores
    scores <- as.matrix(sapply(X, as.numeric)) %*% theta

    # Softmax stable numériquement
    scores_exp <- exp(scores - apply(scores, 1, max))
    probs <- scores_exp / rowSums(scores_exp)

    # Gradient
    gradient <- t(as.matrix(sapply(X, as.numeric))) %*% (probs - y_onehot) / m

    # Mise à jour des paramètres
    theta <- theta - learning_rate * gradient

    # Calcul du coût (log loss)
    cost_history[iter] <- -sum(y_onehot * log(pmax(probs, 1e-15))) / m

    if (iter %% 100 == 0) {
      cat("Itération", iter, "- Coût:", cost_history[iter], "\n")
    }
  }

  list(theta = theta, cost_history = cost_history)
}

# Lecture et prétraitement des données
setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "datasets"))
data <- read.csv("gym_members_exercise_tracking.csv", stringsAsFactors = TRUE)
data <- read.csv("small_dataset_ok.csv", stringsAsFactors = TRUE)
cat("Dimensions des données: ", dim(data), "\n")
cat("Colonnes des données: ", colnames(data), "\n")

# Définition de la variable cible
cible <- "Workout_Type"
cible <- "etiquette_dpe"
# Ajouter une vérification de la colonne cible (qu'elle existe bien)
if (!(cible %in% colnames(data))) {
  stop("La variable cible '", cible, "' n'existe pas dans le jeu de données")
}

# Séparation du jeu de données en deux : cible et le reste
# Cible
target_data <- data[[cible]]
# Différentes modalités de la variable cible
target_levels <- unique(target_data)
target_mapping <- setNames(seq_along(target_levels) - 1, target_levels)
# Data de la variable cible
y <- target_mapping[target_data]

# Suppresion de la variable cible
cat("Dimensions des données: ", dim(data), "\n")
data[[cible]] <- NULL
X <- data
cat("Dimensions des données: ", dim(data), "\n")


# Sélection des variables numériques
numeric_cols <- sapply(X, is.numeric)
# Sélection des variables catégorielles
categorical_cols <- !numeric_cols


# Traitement des valeurs manquantes
# data = X car cible a été enlevée plus haut
X <- as.data.frame(lapply(X, replace_missing))

# Normalisation des variables numériques
X[numeric_cols] <- lapply(X[numeric_cols], scale)
print(dim(X))

# Traitement des variables catégorielles = one hot encoding

# Ne faire le one-hot encoding que s'il y a des colonnes catégorielles
if (any(categorical_cols)) {
  library(fastDummies)
  X <- dummy_cols(X, select_columns = names(X)[categorical_cols], remove_first_dummy = TRUE)
}
print(dim(X))

# Application de la descente de gradient
cat("Début de l'entraînement...\n")
cat("Dimensions des données: ", dim(X), "\n")
cat("Niveaux de la variable cible:", target_levels, "\n")

result <- multinomial_logistic_regression_adam(X, y, learning_rate = 0.001, n_iterations = 10000)

# Affichage des résultats
cat("\nCoefficients finaux :\n")
print(result$theta)
cat("\nCoût final :", tail(result$cost_history, 1), "\n")

# Plot de l'évolution du coût
plot(result$cost_history,
  type = "l",
  xlab = "Itération", ylab = "Coût",
  main = "Évolution du coût pendant l'apprentissage"
)
