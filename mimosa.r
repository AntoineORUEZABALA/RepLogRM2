library(R6)

LogisticRegressionMultinomial <- R6Class("LogisticRegressionMultinomial",
    public = list(
        # Champs
        theta = NULL,
        cost_history = NULL,
        target_levels = NULL,
        target_mapping = NULL,

        # Initialisation
        initialize = function() {
            # Constructeur vide
        },

        # Méthodes de prétraitement
        normalize = function(x) {
            if (sd(x, na.rm = TRUE) == 0) {
                return(rep(0, length(x)))
            }
            return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
        },
        replace_missing = function(x) {
            if (is.numeric(x)) {
                x[is.na(x)] <- mean(x, na.rm = TRUE)
            } else {
                mode_val <- names(sort(table(x), decreasing = TRUE))[1]
                x[is.na(x)] <- mode_val
            }
            return(x)
        },
        sigmoid = function(x) {
            x <- pmax(pmin(x, 100), -100)
            return(1 / (1 + exp(-x)))
        },
        preprocess_data = function(data, target_col) {
            # Vérification de la colonne cible
            if (!(target_col %in% colnames(data))) {
                stop("La variable cible '", target_col, "' n'existe pas dans le jeu de données")
            }

            # Préparation de la cible
            target_data <- data[[target_col]]
            self$target_levels <- unique(target_data)
            self$target_mapping <- setNames(seq_along(self$target_levels) - 1, self$target_levels)
            y <- self$target_mapping[target_data]

            # Préparation des features
            X <- data[, !colnames(data) %in% target_col, drop = FALSE]

            # Traitement des colonnes
            numeric_cols <- sapply(X, is.numeric)
            categorical_cols <- !numeric_cols

            # Remplacement des valeurs manquantes
            X <- as.data.frame(lapply(X, self$replace_missing))

            # Normalisation
            X[numeric_cols] <- lapply(X[numeric_cols], scale)

            # One-hot encoding
            if (any(categorical_cols)) {
                X <- dummy_cols(X, select_columns = names(X)[categorical_cols], remove_first_dummy = TRUE)
            }

            return(list(X = X, y = y))
        },
        fit = function(X, y, learning_rate = 0.01, n_iterations = 1000,
                       beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8) {
            m <- nrow(X)
            n <- ncol(X)
            K <- length(unique(y))

            # Initialisation
            self$theta <- matrix(0, nrow = n, ncol = K)
            self$cost_history <- numeric(n_iterations)
            m_t <- matrix(0, nrow = n, ncol = K)
            v_t <- matrix(0, nrow = n, ncol = K)

            # One-hot encoding de y
            y_onehot <- matrix(0, nrow = m, ncol = K)
            for (i in 1:m) {
                y_onehot[i, y[i] + 1] <- 1
            }

            # Boucle d'apprentissage
            for (iter in 1:n_iterations) {
                scores <- as.matrix(sapply(X, as.numeric)) %*% self$theta
                scores_exp <- exp(scores - apply(scores, 1, max))
                probs <- scores_exp / rowSums(scores_exp)

                gradient <- t(as.matrix(sapply(X, as.numeric))) %*% (probs - y_onehot) / m

                m_t <- beta1 * m_t + (1 - beta1) * gradient
                v_t <- beta2 * v_t + (1 - beta2) * gradient^2

                m_hat <- m_t / (1 - beta1^iter)
                v_hat <- v_t / (1 - beta2^iter)

                self$theta <- self$theta - learning_rate * m_hat / (sqrt(v_hat) + epsilon)
                self$cost_history[iter] <- -sum(y_onehot * log(pmax(probs, 1e-15))) / m

                if (iter %% 100 == 0) {
                    cat("Itération", iter, "- Coût:", self$cost_history[iter], "\n")
                }
            }

            invisible(self)
        },
        predict = function(X) {
            scores <- as.matrix(sapply(X, as.numeric)) %*% self$theta
            scores_exp <- exp(scores - apply(scores, 1, max))
            probs <- scores_exp / rowSums(scores_exp)
            predictions <- max.col(probs) - 1
            return(self$target_levels[predictions + 1])
        },
        plot_cost_history = function() {
            plot(self$cost_history,
                type = "l",
                xlab = "Itération", ylab = "Coût",
                main = "Évolution du coût pendant l'apprentissage"
            )
        }
    )
)
