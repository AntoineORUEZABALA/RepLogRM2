#' @import ggplot2
#' @import R6


#' @title Logistic Regression R6 Class
#' @description This R6 class implements logistic regression for binary and multinomial classification.
#' It supports L1 and L2 regularization, as well as early stopping based on loss convergence.
#'
#' @field weights A matrix storing the model weights (including intercepts).
#' @field learning_rate A numeric value specifying the learning rate for gradient descent.
#' @field max_iter An integer specifying the maximum number of iterations for training.
#' @field classification_type A character string ("binary" or "softmax") specifying the classification type.
#' @field regularization A character string ("l1", "l2", or NULL) indicating the type of regularization.
#' @field reg_lambda A numeric value specifying the regularization strength.
#' @field tolerance A numeric value specifying the convergence tolerance for early stopping.
#' @field losses A numeric vector storing the loss values for each epoch during training.
#' @field epoch An integer specifying the current epoch during training.
#' @field intercept A numeric value storing the model's intercept.
#' @field coefficients A numeric vector storing the model's coefficients.
#' @field X_test A matrix or data frame representing the test features.
#' @field y_test A vector representing the test labels.
#' @field X_train A matrix or data frame representing the training features.
#' @field y_train A vector representing the training labels.
#'
#' @export
LogisticRegression <- R6::R6Class("LogisticRegression",
  public = list(
    weights = NULL,
    learning_rate = 0.01,
    max_iter = 1000,
    classification_type = "binary",
    regularization = NULL,
    reg_lambda = 0.01,
    tolerance = 1e-4,
    losses = NULL,
    epoch = NULL,
    intercept = NULL,
    coefficients = NULL,
    X_test = NULL,
    y_test = NULL,
    X_train = NULL,
    y_train = NULL,

    #'
    #' @description This method initializes the logistic regression model with specified parameters.
    #'
    #' @param learning_rate A numeric value for the learning rate. Default is 0.01.
    #' @param max_iter An integer specifying the maximum number of iterations for gradient descent. Default is 1000.
    #' @param classification_type A character string: "binary" for binary classification or "softmax" for multinomial classification. Default is "binary".
    #' @param regularization The type of regularization to apply. Options are "l1", "l2", or `NULL` (no regularization). Default is `NULL`.
    #' @param reg_lambda A numeric value for the strength of the regularization. Default is 0.01.
    #' @param tolerance A numeric value for the convergence threshold. Default is 1e-4.
    #' @return An instance of the LogisticRegression class.
    initialize = function(learning_rate = 0.01, max_iter = 1000, classification_type = "binary", regularization = NULL, reg_lambda = 0.01, tolerance = 1e-4) {
      self$learning_rate <- learning_rate
      self$max_iter <- max_iter
      self$classification_type <- classification_type
      self$regularization <- regularization
      self$reg_lambda <- reg_lambda
      self$tolerance <- tolerance
    },

    #' Print Model Information
    #'
    #' This method prints a summary of the logistic regression model's current configuration.
    #' It includes details such as classification type, learning rate, regularization type,
    #' number of classes (if weights are initialized), and convergence tolerance.
    #'
    #' @return None. Prints the information to the console.
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

    #' Summary of the Logistic Regression Model
    #'
    #' This method provides a detailed summary of the logistic regression model, including weights,
    #' feature importance, and training losses. It supports both binary and multinomial classification.
    #' Optionally, a visualization of feature importance and the training loss history can be included.
    #'
    #' @param class_names An optional character vector specifying the names of the classes. If `NULL`,
    #'        default names ("Class 0", "Class 1", etc.) are used.
    #' @param plot_importance A logical value indicating whether to display a bar plot of feature importance.
    #'        Default is `TRUE`.
    #' @param show_losses A logical value indicating whether to display the training loss history.
    #'        Default is `TRUE`.
    #' @return None. Prints the summary and optionally plots the feature importance.
    summary = function(class_names = NULL, plot_importance = TRUE, show_losses = TRUE) {
      self$print()
      cat("\nModel Weights (if trained):\n")

      if (!is.null(self$weights)) {
        intercept <- self$weights[1, , drop = FALSE]  # Intercept
        coefficients <- self$weights[-1, , drop = FALSE]  # Coefficients (sans le biais)

        if (self$classification_type == "binary") {
          # Cas binaire : Construire une matrice de poids pour les deux classes
          weights_binary <- cbind(-self$weights, self$weights)  # Classe négative et classe positive
          rownames(weights_binary) <- c("(Intercept)", colnames(self$X_train))  # Ajouter noms des features

          # Trier les coefficients par valeur absolue
          weights_binary_sorted <- weights_binary[order(-abs(weights_binary[, 2])), , drop = FALSE]

          if (!is.null(class_names) && length(class_names) == 2) {
            colnames(weights_binary_sorted) <- class_names  # Nommer les colonnes pour les deux classes
          } else {
            colnames(weights_binary_sorted) <- c("Class 0", "Class 1")  # Noms par défaut
          }

          cat("\nWeights (Binary Classification, sorted by absolute value):\n")
          print(weights_binary_sorted)

          # Préparer les données pour la visualisation
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

          rownames(coefficients_sorted) <- colnames(self$X_train)[sorted_indices]  # Noms des features triés

          cat("Intercepts:\n")
          print(intercept)

          cat("\nCoefficients (sorted by absolute value across all classes):\n")
          print(coefficients_sorted)

          # Préparer les données pour la visualisation
          feature_importance <- data.frame(
            Feature = rownames(coefficients_sorted),
            Importance = total_importance[sorted_indices]
          )
        }

        # Afficher le graphique des importances si demandé
        if (plot_importance) {
          p <- ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(title = "Feature Importance in Logistic Regression",
                 x = "Features", y = "Importance") +
            theme_minimal()
          print(p)
        }
      } else {
        cat("Model not trained yet.\n")
      }

      # Afficher les pertes d'entraînement si demandé
      if (show_losses) {
        cat("\nTraining Losses:\n")
        if (!is.null(self$losses)) {
          print(self$losses)
          cat(sprintf("Final Loss: %.6f\n", tail(self$losses, 1)))
        } else {
          cat("No training losses available (model not trained).\n")
        }
      }
    }
    ,

    #' Sigmoid Activation Function
    #'
    #' This method applies the sigmoid activation function to the input.
    #' The sigmoid function is defined as \eqn{1 / (1 + exp(-z))}.
    #' It maps any real number to a value between 0 and 1.
    #'
    #' @param z A numeric vector or matrix of inputs.
    #' @return A numeric vector or matrix with the sigmoid function applied to each element.
    sigmoid = function(z) {
      z <- pmin(pmax(z, -500), 500)
      return(1 / (1 + exp(-z)))
    },

    #' Softmax Activation Function
    #'
    #' This method applies the softmax activation function to the input.
    #' The softmax function is defined as:
    #' \deqn{softmax(z) = exp(z) / sum(exp(z))}
    #' It is used to normalize outputs to a probability distribution across multiple classes.
    #'
    #' @param z A numeric matrix where each row represents the unnormalized logits for a single sample.
    #' @return A numeric matrix where each row contains the normalized probabilities for each class.
    softmax = function(z) {
      z <- pmin(pmax(z, -500), 500)
      exp_z <- exp(z)
      return(exp_z / rowSums(exp_z))
    },

    #' Forward Pass
    #'
    #' This method performs a forward pass through the logistic regression model.
    #' It computes the logits and applies the appropriate activation function
    #' (sigmoid for binary classification, softmax for multinomial classification).
    #'
    #' @param x A numeric matrix of input features. Each row represents a sample, and each column represents a feature.
    #' @return A numeric vector (for binary classification) or a numeric matrix (for multinomial classification)
    #'         containing the predicted probabilities.
    forward = function(x) {
      x_with_bias <- cbind(1, x)
      z <- x_with_bias %*% self$weights

      if (self$classification_type == "binary") {
        return(self$sigmoid(z))
      } else if (self$classification_type == "softmax") {
        return(self$softmax(z))
      }
    },

    #' Logarithmic Loss Function
    #'
    #' This method computes the log loss (logarithmic loss) for binary classification.
    #' Log loss measures the performance of a classification model whose output is a probability
    #' value between 0 and 1. A lower log loss indicates a better model.
    #'
    #' The formula for log loss is:
    #' \deqn{-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))}
    #'
    #' @param y_true A numeric vector of true binary labels (0 or 1).
    #' @param y_pred A numeric vector of predicted probabilities (values between 0 and 1).
    #' @return A numeric value representing the log loss.
    log_loss = function(y_true, y_pred) {
      y_pred <- pmin(pmax(y_pred, 1e-15), 1 - 1e-15)
      return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
    },

    #' Cross-Entropy Loss Function
    #'
    #' This method computes the cross-entropy loss for multinomial classification.
    #' Cross-entropy loss measures the performance of a classification model whose output is a probability
    #' distribution across multiple classes.
    #'
    #' The formula for cross-entropy loss is:
    #' \deqn{-mean(sum(y_true_one_hot * log(y_pred), axis=1))}
    #'
    #' @param y_true A numeric vector of true class labels (integer values starting from 0).
    #' @param y_pred A numeric matrix of predicted probabilities where each row represents the
    #'        predicted probability distribution for a sample, and each column corresponds to a class.
    #' @return A numeric value representing the cross-entropy loss.
    cross_entropy_loss = function(y_true, y_pred) {
      y_true <- as.integer(y_true)
      y_one_hot <- matrix(0, nrow = nrow(y_pred), ncol = ncol(y_pred))
      y_one_hot[cbind(1:nrow(y_pred), y_true + 1)] <- 1
      y_pred <- pmin(pmax(y_pred, 1e-15), 1 - 1e-15)
      return(-mean(rowSums(y_one_hot * log(y_pred))))
    },

    #' Update Weights Using Gradient Descent
    #'
    #' This method updates the weights of the logistic regression model using gradient descent.
    #' The gradients are computed based on the predicted probabilities and true labels, and
    #' regularization L1 or L2 is applied if specified.
    #'
    #' @param x A numeric matrix of input features. Each row represents a sample, and each column represents a feature.
    #' @param y A numeric vector of true labels. For binary classification, labels should be 0 or 1.
    #'        For multinomial classification, labels should be integers starting from 0.
    #' @return None. The weights are updated in place.
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

    #' Initialize Model Weights
    #'
    #' This method initializes the weights of the logistic regression model. The weights are
    #' randomly generated from a normal distribution.
    #'
    #' @param input_dim An integer specifying the number of features in the input data.
    #' @param num_classes An integer specifying the number of classes. Default is 1 for binary classification.
    #' @param seed An integer for setting the random seed to ensure reproducibility. Default is 42.
    #' @return None. The weights are initialized in place.
    initialize_weights = function(input_dim, num_classes = 1, seed = 42) {
      set.seed(seed)
      self$weights <- matrix(rnorm((input_dim + 1) * num_classes), nrow = input_dim + 1, ncol = num_classes)
    },

    #' Train the Logistic Regression Model
    #'
    #' This method trains the logistic regression model using gradient descent on the provided training data.
    #' It initializes the model weights, computes the loss for each epoch, and updates the weights
    #' iteratively to minimize the loss. The training stops when the loss converges or the maximum
    #' number of iterations is reached.
    #'
    #' @param X_train A numeric matrix of features for the training set. Each row represents a sample,
    #'        and each column represents a feature.
    #' @param y_train A numeric vector of target labels for the training set. For binary classification,
    #'        labels should be 0 or 1. For multinomial classification, labels should be integers starting from 0.
    #' @return None. The model is trained in place, and the weights and training losses are updated.
    #' print(model$losses)
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
      # Assign intercept and coefficients after training
      self$intercept <- self$weights[1, , drop = FALSE]
      self$coefficients <- self$weights[-1, , drop = FALSE]
    },

    #' Predict Class Labels
    #'
    #' This method predicts class labels for the given test set based on the trained logistic regression model.
    #' For binary classification, it thresholds the predicted probabilities at 0.5. For multinomial classification,
    #' it selects the class with the highest predicted probability.
    #'
    #' @param X_test A numeric matrix of features for the test set. Each row represents a sample, and each column represents a feature.
    #' @return A numeric vector of predicted class labels. For binary classification, the labels are 0 or 1.
    #'         For multinomial classification, the labels are integers starting from 0.
    predict = function(X_test) {
      y_pred_probs <- self$forward(X_test)
      if (self$classification_type == "binary") {
        y_pred <- as.integer(y_pred_probs >= 0.5)
      } else if (self$classification_type == "softmax") {
        y_pred <- max.col(y_pred_probs) - 1
      }
      return(y_pred)
    },

    #' Print Model Weights
    #'
    #' This method prints the weights of the logistic regression model, including the intercept(s)
    #' and coefficients. The weights are displayed in a human-readable format.
    #'
    #' @return None. This method prints the weights to the console.
    print_weights = function() {
      intercept <- self$weights[1, ]
      coefficients <- self$weights[-1, , drop = FALSE]
      cat("Intercept(s):\n")
      print(intercept)
      cat("\nCoefficients:\n")
      print(coefficients)
    },

    #' Predict Class Probabilities
    #'
    #' This method predicts the probabilities of class membership for the given test set
    #' based on the trained logistic regression model. For binary classification, it returns
    #' probabilities for the positive class. For multinomial classification, it returns
    #' the full probability distribution across all classes.
    #'
    #' @param X_test A numeric matrix of features for the test set. Each row represents a sample,
    #'        and each column represents a feature.
    #' @return A numeric matrix of predicted probabilities. For binary classification, it returns
    #'         a column vector where each element is the probability of the positive class.
    #'         For multinomial classification, it returns a matrix where each row represents
    #'         the probability distribution across all classes for a sample.
    predict_proba = function(X_test) {
      y_pred_probs <- self$forward(X_test)
      return(y_pred_probs)
    },


    #' Calculate Precision
    #'
    #' This method calculates the precision, which is the ratio of correctly predicted positive observations
    #' to the total predicted positive observations.
    #'
    #' Precision is defined as:
    #' \deqn{Precision = \frac{True Positives}{True Positives + False Positives}}
    #'
    #' @param y_true A numeric vector of true labels.
    #' @param y_pred A numeric vector of predicted labels.
    #' @return A numeric value representing the precision.
    precision = function(y_true, y_pred) {
      return(sum(y_true == y_pred) / length(y_true))
    },

    #' Calculate Recall
    #'
    #' This method calculates the recall (or sensitivity), which is the ratio of correctly predicted positive observations
    #' to all actual positive observations.
    #'
    #' Recall is defined as:
    #' \deqn{Recall = \frac{True Positives}{True Positives + False Negatives}}
    #'
    #' @param y_true A numeric vector of true labels.
    #' @param y_pred A numeric vector of predicted labels.
    #' @return A numeric value representing the recall.
    recall = function(y_true, y_pred) {
      return(sum((y_true == 1) & (y_pred == 1)) / sum(y_true == 1))
    },

    #' Calculate F1-Score
    #'
    #' This method calculates the F1-score, which is the harmonic mean of precision and recall.
    #' It provides a balance between precision and recall.
    #'
    #' F1-score is defined as:
    #' \deqn{F1 = 2 \times \frac{Precision \times Recall}{Precision + Recall}}
    #'
    #' @param y_true A numeric vector of true labels.
    #' @param y_pred A numeric vector of predicted labels.
    #' @return A numeric value representing the F1-score.
    f1_score = function(y_true, y_pred) {
      prec <- self$precision(y_true, y_pred)
      rec <- self$recall(y_true, y_pred)
      return(2 * prec * rec / (prec + rec))
    },

    #' Generate Confusion Matrix
    #'
    #' This method generates a confusion matrix to evaluate the performance of a classification model.
    #' The confusion matrix shows the number of correct and incorrect predictions for each class.
    #'
    #' @param y_true A numeric vector of true labels.
    #' @param y_pred A numeric vector of predicted labels.
    #' @return A matrix where the rows represent the true classes and the columns represent the predicted classes.
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
