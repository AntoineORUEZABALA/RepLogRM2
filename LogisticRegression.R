# LogisticRegression.R
library(R6)
library(ggplot2)
library(pROC)
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

    initialize = function(learning_rate = 0.01, max_iter = 1000, classification_type = "binary", regularization = NULL, reg_lambda = 0.01, tolerance = 1e-4) {
      self$learning_rate <- learning_rate
      self$max_iter <- max_iter
      self$classification_type <- classification_type
      self$regularization <- regularization
      self$reg_lambda <- reg_lambda
      self$tolerance <- tolerance
    },

    print = function() {
      cat("Logistic Regression Model\n")
      cat("-------------------------\n")
      cat(sprintf("Type of Classification: %s\n", self$classification_type))
      if (!is.null(self$weights)) {
        cat(sprintf("Number of Classes: %d\n", ncol(self$weights)))
      } else {
        cat("Number of Classes: Not trained yet\n")
      }
      cat(sprintf("Learning Rate: %.4f\n", self$learning_rate))
      cat(sprintf("Max Iterations: %d\n", self$max_iter))
      cat(sprintf("Regularization: %s\n", ifelse(is.null(self$regularization), "None", self$regularization)))
      cat(sprintf("Regularization Lambda: %.4f\n", self$reg_lambda))
      cat(sprintf("Tolerance: %.6f\n", self$tolerance))
    },

    summary = function(class_names = NULL) {
      self$print()
      cat("\nModel Weights (if trained):\n")

      if (!is.null(self$weights)) {
        intercept <- self$weights[1, , drop = FALSE]  # Intercept
        coefficients <- self$weights[-1, , drop = FALSE]  # Coefficients (sans le biais)

        if (self$classification_type == "binary") {
          # Cas binaire : Construire une matrice de poids pour les deux classes
          weights_binary <- cbind(-self$weights, self$weights)  # Classe négative et classe positive
          rownames(weights_binary) <- c("(Intercept)", colnames(X_train))  # Ajouter noms des features

          # Trier les coefficients par valeur absolue
          weights_binary_sorted <- weights_binary[order(-abs(weights_binary[, 2])), , drop = FALSE]

          if (!is.null(class_names) && length(class_names) == 2) {
            colnames(weights_binary_sorted) <- class_names  # Nommer les colonnes pour les deux classes
          } else {
            colnames(weights_binary_sorted) <- c("Class 0", "Class 1")  # Noms par défaut
          }

          cat("\nWeights (Binary Classification, sorted by absolute value):\n")
          print(weights_binary_sorted)

          # Visualisation
          feature_importance <- data.frame(
            Feature = rownames(weights_binary_sorted)[-1],  # Exclure l'intercept
            Importance = abs(weights_binary_sorted[-1, 2])  # Importance pour la classe positive
          )

        } else if (self$classification_type == "softmax") {
          # Cas multinomial : Une colonne par classe
          if (!is.null(class_names)) {
            colnames(self$weights) <- class_names
          }

          intercept <- self$weights[1, ]  # Intercepts pour chaque classe
          coefficients <- self$weights[-1, , drop = FALSE]  # Coefficients sans les intercepts

          # Calcul de l'importance totale (somme des valeurs absolues sur les classes)
          total_importance <- rowMeans(abs(coefficients))

          # Trier les coefficients par importance décroissante
          sorted_indices <- order(-total_importance)
          coefficients_sorted <- coefficients[sorted_indices, , drop = FALSE]

          rownames(coefficients_sorted) <- colnames(X_train)[sorted_indices]  # Noms des features triés

          cat("Intercepts:\n")
          print(intercept)

          cat("\nCoefficients (sorted by absolute value across all classes):\n")
          print(coefficients_sorted)

          # Visualisation
          feature_importance <- data.frame(
            Feature = rownames(coefficients_sorted),
            Importance = total_importance[sorted_indices]
          )
        }

        # Graphique des importances
        p<- ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = "Feature Importance in Logistic Regression",
               x = "Features", y = "Importance") +
          theme_minimal()
        print(p)

      } else {
        cat("Model not trained yet.\n")
      }

      cat("\nTraining Losses:\n")
      if (!is.null(self$losses)) {
        print(self$losses)
        cat(sprintf("Final Loss: %.6f\n", tail(self$losses, 1)))
      } else {
        cat("No training losses available (model not trained).\n")
      }
    }
    ,

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

    fit = function(X_train, y_train) {
      input_dim <- ncol(X_train)
      num_classes <- if (self$classification_type == "binary") 1 else length(unique(y_train))
      self$initialize_weights(input_dim, num_classes)

      self$losses <- c()
      for (epoch in 1:self$max_iter) {
        for (i in 1:nrow(X_train)) {
          self$update_weights(X_train[i, , drop = FALSE], y_train[i])
        }
        y_pred <- self$forward(X_train)
        if (self$classification_type == "binary") {
          loss <- self$log_loss(y_train, y_pred)
        } else if (self$classification_type == "softmax") {
          loss <- self$cross_entropy_loss(y_train, y_pred)
        }
        self$losses <- c(self$losses, loss)
        cat(sprintf("Epoch %d: Loss = %.4f\n", epoch, loss))
        if (epoch > 1 && abs(self$losses[epoch] - self$losses[epoch - 1]) < self$tolerance) {
          cat(sprintf("Converged at epoch %d with loss %.4f\n", epoch, loss))
          break
        }
        if (epoch > 1 && self$losses[epoch] > self$losses[epoch - 1]) {
          self$learning_rate <- self$learning_rate * 0.5
          cat(sprintf("Reducing learning rate to %.6f at epoch %d\n", self$learning_rate, epoch))
        }
      }
    },

    predict = function(X_test) {
      y_pred_probs <- self$forward(X_test)
      if (self$classification_type == "binary") {
        y_pred <- as.integer(y_pred_probs >= 0.5)
      } else if (self$classification_type == "softmax") {
        y_pred <- max.col(y_pred_probs) - 1
      }
      return(y_pred)
    },

    print_weights = function() {
      intercept <- self$weights[1, ]
      coefficients <- self$weights[-1, , drop = FALSE]
      cat("Intercept(s):\n")
      print(intercept)
      cat("\nCoefficients:\n")
      print(coefficients)
    },

    predict_proba = function(X_test) {
      y_pred_probs <- self$forward(X_test)
      return(y_pred_probs)
    },


    precision = function(y_true, y_pred) {
      return(sum(y_true == y_pred) / length(y_true))
    },

    recall = function(y_true, y_pred) {
      return(sum((y_true == 1) & (y_pred == 1)) / sum(y_true == 1))
    },

    f1_score = function(y_true, y_pred) {
      prec <- self$precision(y_true, y_pred)
      rec <- self$recall(y_true, y_pred)
      return(2 * prec * rec / (prec + rec))
    },

    confusion_matrix = function(y_true, y_pred) {
      K <- length(unique(c(y_true, y_pred)))
      cm <- matrix(0, nrow = K, ncol = K)
      for (i in 1:K) {
        for (j in 1:K) {
          cm[i, j] <- sum((y_true == (i - 1)) & (y_pred == (j - 1)))
        }
      }
      return(cm)
    }
  )
)
