library(R6)
library(datasets)
library(nnet) 
setwd("C:/Users/aoruezabal/Documents/GitHub/RepLogRM2") 

LogisticRegression <- R6Class(
  "LogisticRegression",
  public = list(
    weights = NULL,
    learning_rate = 0.01,
    max_iter = 1000,
    classification_type = "binary",
    regularization = NULL,
    reg_lambda = 0.01,
    tolerance = 1e-4,
    losses = NULL,
    cible = NULL, 
    data = NULL,
    modalites_cible = NULL, 
    nb_modalites_cible = NULL, 
    modalite_ref_cible = NULL,
    type_regression = NULL,
    
    initialize= function(data, cible, learning_rate = 0.01, max_iter = 1000, classification_type = "binary", regularization = NULL, reg_lambda = 0.01, tolerance = 1e-4) {
      self$data <- data
      self$cible <- cible
      self$learning_rate <- learning_rate
      self$max_iter <- max_iter
      self$classification_type <- classification_type
      self$regularization <- regularization
      self$reg_lambda <- reg_lambda
      self$tolerance <- tolerance
      
      if (!is.data.frame(self$data)) {
        stop("Les données '", self$data, "'' doivent être dans un dataframe")
      }
      if (!is.character(self$cible)) {
        stop("La variable '", self$cible, "'' doit être un vecteur/character")
      }
      if (!self$cible %in% colnames(self$data)) {
        stop("La variable '", self$cible, "'' doit être présente dans le dataframe")
      }
      if (!is.character(self$data[[self$cible]]) && !is.factor(self$data[[self$cible]])) {
        stop("La variable cible doit être de type character ou factor")
      }
      if (length(unique(self$data[[self$cible]])) < 2) {
        stop("La variable cible doit avoir au moins deux modalités")
      }
      self$modalites_cible <- levels(as.factor(self$data[[self$cible]]))
      self$nb_modalites_cible <- length(self$modalites_cible)
      if (self$nb_modalites_cible == 2) {
        self$type_regression <- "Régression logistique binaire"
      } else {
        self$type_regression <- "Régression logistique multinomiale"
      }
      self$data <- self$data[!is.na(self$data[[self$cible]]), ]
    },
    
    sigmoid = function(z) {
      z <- pmin(pmax(z, -500), 500)
      return(1 / (1 + exp(-z)))
    },
    
    softmax = function(z) {
      z <- pmin(pmax(z, -500), 500)
      exp_z <- exp(z)
      return(exp_z / rowSums(exp_z))
    },
    
    forward = function(x) {
      x_with_bias <- cbind(1, x)
      z <- x_with_bias %*% self$weights
      
      if (self$classification_type == "binary") {
        return(self$sigmoid(z))
      } else if (self$classification_type == "softmax") {
        return(self$softmax(z))
      }
    },
    
    log_loss = function(y_true, y_pred) {
      y_pred <- pmin(pmax(y_pred, 1e-15), 1 - 1e-15)
      return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
    },
    
    cross_entropy_loss = function(y_true, y_pred) {
      y_true <- as.integer(y_true)
      y_one_hot <- matrix(0, nrow = nrow(y_pred), ncol = ncol(y_pred))
      y_one_hot[cbind(1:nrow(y_pred), y_true + 1)] <- 1
      y_pred <- pmin(pmax(y_pred, 1e-15), 1 - 1e-15)
      return(-mean(rowSums(y_one_hot * log(y_pred))))
    },
    
    update_weights = function(x, y) {
      if (self$classification_type == "softmax") {
        y <- as.integer(y)
        y_one_hot <- matrix(0, nrow = nrow(x), ncol = ncol(self$weights))
        y_one_hot[cbind(1:nrow(x), y + 1)] <- 1
      }
      y_pred <- self$forward(x)
      if (self$classification_type == "binary") {
        error <- y_pred - y
      } else if (self$classification_type == "softmax") {
        error <- y_pred - y_one_hot
      }
      gradient <- t(cbind(1, x)) %*% error / nrow(x)
      if (self$regularization == "l2") {
        gradient[-1, ] <- gradient[-1, ] + self$reg_lambda * self$weights[-1, ] / nrow(x)
      } else if (self$regularization == "l1") {
        gradient[-1, ] <- gradient[-1, ] + self$reg_lambda * sign(self$weights[-1, ]) / nrow(x)
      }
      self$weights <- self$weights - self$learning_rate * gradient
    },
    
    initialize_weights = function(input_dim, num_classes = 1, seed = 42) {
      set.seed(seed)
      self$weights <- matrix(rnorm((input_dim + 1) * num_classes), nrow = input_dim + 1, ncol = num_classes)
    },
    
    fit = function(X,y) {
      taux_apprentissage <- 0.01
      n_iterations <- 20000
      tolerance <- 1e-6
      
      if (self$nb_modalites_cible == 2) {
        result <- descente_gradient(X, y, taux_apprentissage = taux_apprentissage, n_iterations = n_iterations, tolerance = tolerance)
        
        theta <- result$theta
        cost_history <- result$cost_history
        
        coef_df <- data.frame(
          Variable = colnames(X),
          Coefficient = as.numeric(theta)
        )
        
        coef_df$Abs_Coefficient <- abs(coef_df$Coefficient)
        coef_df <- coef_df[order(-coef_df$Abs_Coefficient), ]
        print("\nCoefficients :")
        print(coef_df[, c("Variable", "Coefficient")])
        
        if (!any(!is.finite(cost_history))) {
          plot(1:length(cost_history), cost_history,
               type = "l",
               col = "blue",
               xlab = "Itération",
               ylab = "Coût",
               main = "Évolution du coût pendant l'apprentissage"
          )
        } else {
          warning("Cost history contient des valeurs non finies, impossibilité de faire le graphique.")
        }
      } else {
        theta <- matrix(0, nrow = ncol(X), ncol = self$nb_modalites_cible)
        thetas_multiclass <- one_vs_rest(X, y, taux_apprentissage, n_iterations, tolerance)
        # thetas_multiclass <- multinomial_logistic_ovr(X, y, max_iter = 1000, learning_rate = 0.01)
        predictions_multiclass <- predict_multiclass(X, thetas_multiclass)
        
        coef_df <- as.data.frame(t(thetas_multiclass))
        names(coef_df) <- colnames(X) 
        rownames(coef_df) <- self$modalites_cible[-1] 
        
        input_dim <- ncol(X)
        num_classes <- if (self$classification_type == "binary") 1 else length(unique(y))
        self$initialize_weights(input_dim, num_classes)
        
        self$losses <- c()
        for (epoch in 1:self$max_iter) {
          for (i in 1:nrow(X)) {
            self$update_weights(X[i, , drop = FALSE], y[i])
          }
          y_pred <- self$forward(X)
          if (self$classification_type == "binary") {
            loss <- self$log_loss(y, y_pred)
          } else if (self$classification_type == "softmax") {
            loss <- self$cross_entropy_loss(y, y_pred)
          }
          self$losses <- c(self$losses, loss)
          cat(sprintf("Epoque %d: Perte = %.4f\n", epoch, loss))
          if (epoch > 1 && abs(self$losses[epoch] - self$losses[epoch - 1]) < self$tolerance) {
            cat(sprintf("Convergé à l'époque %d avec une perte de %.4f\n", epoch, loss))
            break
          }
          if (epoch > 1 && self$losses[epoch] > self$losses[epoch - 1]) {
            self$learning_rate <- self$learning_rate * 0.5
            cat(sprintf("Réduction du taux d'apprentissage à %.6f à l'époque %d\n", self$learning_rate, epoch))
          }
        }
        
        print("\nCoefficients par modalité et variable :")
        print(coef_df)
      }
    },
    
    predict = function(X) {
      if (!exists("weights") || !exists("bias")) {
        stop("Le modèle n'a pas encore été entraîné. Veuillez d'abord utiliser la méthode fit()")
      }
      y_pred_probs <- self$forward(X)
      if (self$classification_type == "binary") {
        y_pred <- as.integer(y_pred_probs >= 0.5)
      } else if (self$classification_type == "softmax") {
        y_pred <- max.col(y_pred_probs) - 1
      }
      return(y_pred)
    },
    
    predict_proba = function(X) {
      if (!exists("weights") || !exists("bias")) {
        stop("Le modèle n'a pas encore été entraîné. Veuillez d'abord utiliser la méthode fit()")
      }
      y_pred_probs <- self$forward(X)
      
      return(y_pred_probs)
    },
    
    print = function() {
      print(paste0("Les données comportent ", nrow(self$data), " observations et ", ncol(self$data), " variables"))
      print(paste0("La variable cible est '", self$cible, "' avec ", self$nb_modalites_cible, " modalités"))
      print(paste0("La modalité de référence de ", self$cible, " est '", self$modalite_ref_cible, "'"))
    },
    
    print_weights = function() {
      intercept <- self$weights[1, ]
      coefficients <- self$weights[-1, , drop = FALSE]
      cat("Intercept(s):\n")
      print(intercept)
      cat("\nCoefficients:\n")
      print(coefficients)
    },
    
    summary = function() {
      accuracy <- mean(predictions == y_test)
      cat(sprintf("\nAccuracy: %.2f%%\n", accuracy * 100))
      
      # Tracer les pertes au fil des époques
      plot(model$losses, type = "l", main = "Loss over epochs", xlab = "Epochs", ylab = "Loss")
      
      # Afficher les poids du modèle
      cat("\nModel Weights:\n")
      model$print_weights()
    }
  ) 
)

# Charger les données
data <- read.csv("datasets/gym_members_exercise_tracking.csv")

# Définir la colonne cible
cible <- "Workout_Type"
feature_cols <- setdiff(names(data), cible)

# Séparer les données en train et test (70% train, 30% test)
set.seed(123)  # Pour reproductibilité
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Identifier les colonnes quantitatives et qualitatives
quantitative_cols <- feature_cols[sapply(data[, feature_cols], is.numeric)]
qualitative_cols <- feature_cols[!sapply(data[, feature_cols], is.numeric)]

# Calculer les statistiques pour l'ensemble d'entraînement
train_means <- sapply(train_data[, quantitative_cols], mean)
train_sds <- sapply(train_data[, quantitative_cols], sd)

# Standardiser l'ensemble d'entraînement
train_data[, quantitative_cols] <- sweep(train_data[, quantitative_cols], 2, train_means, "-")
train_data[, quantitative_cols] <- sweep(train_data[, quantitative_cols], 2, train_sds, "/")

# Appliquer les mêmes transformations à l'ensemble de test
test_data[, quantitative_cols] <- sweep(test_data[, quantitative_cols], 2, train_means, "-")
test_data[, quantitative_cols] <- sweep(test_data[, quantitative_cols], 2, train_sds, "/")

# Encodage one-hot pour les colonnes qualitatives (train et test séparément)
train_encoded_qualitative <- as.data.frame(model.matrix(~ . - 1, data = train_data[, qualitative_cols, drop = FALSE]))
test_encoded_qualitative <- as.data.frame(model.matrix(~ . - 1, data = test_data[, qualitative_cols, drop = FALSE]))

# Aligner les colonnes entre train et test (au cas où il y aurait des différences dans les niveaux de facteurs)
all_levels <- union(names(train_encoded_qualitative), names(test_encoded_qualitative))
train_encoded_qualitative <- train_encoded_qualitative[, all_levels, drop = FALSE]
test_encoded_qualitative <- test_encoded_qualitative[, all_levels, drop = FALSE]
train_encoded_qualitative[is.na(train_encoded_qualitative)] <- 0
test_encoded_qualitative[is.na(test_encoded_qualitative)] <- 0

# Combiner les colonnes transformées (quantitatives + qualitatives)
train_final <- cbind(train_data[, quantitative_cols], train_encoded_qualitative, Workout_Type = train_data[[cible]])
test_final <- cbind(test_data[, quantitative_cols], test_encoded_qualitative, Workout_Type = test_data[[cible]])

# Encodage de la cible en format numérique
train_final$Workout_Type <- as.integer(as.factor(train_final$Workout_Type)) - 1
test_final$Workout_Type <- as.integer(as.factor(test_final$Workout_Type)) - 1

# Vérification des structures
cat("Train set:\n")
str(train_final)
cat("\nTest set:\n")
str(test_final)

# Résumé des tailles
cat(sprintf("\nTaille de l'ensemble d'entraînement : %d\n", nrow(train_final)))
cat(sprintf("Taille de l'ensemble de test : %d\n", nrow(test_final)))
# Extraire les caractéristiques et la cible de l'ensemble d'entraînement
X_train <- as.matrix(train_final[, setdiff(names(train_final), "Workout_Type")])  # Caractéristiques
y_train <- train_final$Workout_Type  # Cible

# Extraire les caractéristiques et la cible de l'ensemble de test
X_test <- as.matrix(test_final[, setdiff(names(test_final), "Workout_Type")])  # Caractéristiques
y_test <- test_final$Workout_Type  # Cible

##############################################

# Initialiser le modèle
model <- LogisticRegression$new(
  data = data,
  cible = cible,
  learning_rate = 0.001,
  max_iter = 500,
  classification_type = "softmax",
  regularization = "l2",
  reg_lambda = 0.01
)

# Entraîner le modèle sur les données d'entraînement
model$fit(X_train, y_train)

# Effectuer des prédictions sur l'ensemble de test
predictions <- model$predict(X_test)

# Afficher les prédictions de probas d'appartenance de classes de la variable cible
probas <- model$predict_proba(y_test)

# Afficher les prédictions et la cible réelle

cat("Probas d'appartenance à \n")
print(probas)
cat("Predictions:\n")
print(predictions)
cat("\nTrue Labels:\n")
print(y_test)

# Calculer et afficher la précision
model$summary()



# ####################
# Fonction pour effectuer le Grid Search
# grid_search <- function(X_train, y_train, X_test, y_test, param_grid) {
#   best_model <- NULL
#   best_accuracy <- 0
#   best_params <- NULL

#   results <- list()

# Boucle à travers toutes les combinaisons de paramètres
#   for (learning_rate in param_grid$learning_rate) {
#     for (max_iter in param_grid$max_iter) {
#       for (regularization in param_grid$regularization) {
#         for (reg_lambda in param_grid$reg_lambda) {
#           # Initialiser le modèle avec les paramètres actuels
#           model <- LogisticRegression$new(
#             learning_rate = learning_rate,
#             max_iter = max_iter,
#             classification_type = "softmax",
#             regularization = regularization,
#             reg_lambda = reg_lambda
#           )

#           # Entraîner le modèle
#           model$fit(X_train, y_train)

#           # Effectuer des prédictions
#           predictions <- model$predict(X_test)

#           # Calculer la précision
#           accuracy <- mean(predictions == y_test)

#           # Stocker les résultats
#           results <- append(results, list(list(
#             learning_rate = learning_rate,
#             max_iter = max_iter,
#             regularization = regularization,
#             reg_lambda = reg_lambda,
#             accuracy = accuracy
#           )))

#           # Vérifier si ce modèle est le meilleur
#           if (accuracy > best_accuracy) {
#             best_accuracy <- accuracy
#             best_model <- model
#             best_params <- list(
#               learning_rate = learning_rate,
#               max_iter = max_iter,
#               regularization = regularization,
#               reg_lambda = reg_lambda
#             )
#           }
#         }
#       }
#     }
#   }

# Retourner les résultats
#   return(list(best_model = best_model, best_params = best_params, best_accuracy = best_accuracy, all_results = results))
# }

# # Définir la grille de paramètres
# param_grid <- list(
#   learning_rate = c(0.01, 0.001, 0.0001),
#   max_iter = c(500, 1000, 2000),
#   regularization = c("l1", "l2"),
#   reg_lambda = c(0.01, 0.1, 1.0)
# )

# Exécuter le Grid Search
# grid_results <- grid_search(X_train, y_train, X_test, y_test, param_grid)

# Résultats du meilleur modèle
# cat("Best Parameters:\n")
# print(grid_results$best_params)
# cat(sprintf("\nBest Accuracy: %.2f%%\n", grid_results$best_accuracy * 100))

# Tracer les pertes du meilleur modèle
# plot(grid_results$best_model$losses, type = "l", main = "Loss over epochs (Best Model)", xlab = "Epochs", ylab = "Loss")

# Afficher les poids du meilleur modèle
# cat("\nBest Model Weights:\n")
# grid_results$best_model$print_weights()



