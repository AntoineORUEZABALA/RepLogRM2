# Fonction sigmoïde
sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
}

# Fonction pour calculer le softmax
softmax <- function(x) {
    exp_x <- exp(x - max(x))
    exp_x / sum(exp_x)
}

# Fonction d'hypothèse
hypothese <- function(X, theta) {
    z <- X %*% theta
    return(sigmoid(z))
}

# Fonction de coût pour la régression logistique avec cross entropy
cout <- function(X, y, theta) {
    m <- length(y)
    h <- hypothese(X, theta)
    # Ajout d'un petit epsilon pour éviter les NaN
    epsilon <- 1e-15
    cost <- (-1 / m) * sum(y * log(h + epsilon) + (1 - y) * log(1 - h + epsilon))
    return(cost)
}

# Descente de gradient pour la régression logistique
descente_gradient <- function(X, y, taux_apprentissage, n_iterations, tolerance) {
    cost_history <- numeric(n_iterations) # Vecteur pour stocker l'historique du coût

    m <- length(y)
    theta <- rep(0, ncol(X))

    for (i in 1:n_iterations) {
        h <- hypothese(X, theta)
        gradient <- 1 / m * t(X) %*% (h - y)
        theta <- theta - taux_apprentissage * gradient
        cost_history[i] <- cout(X, y, theta) # Enregistrement du coût

        # Vérifier la tolérance pour arrêter les itérations
        if (i > 2 && abs(cost_history[i] - cost_history[i - 1]) < tolerance) {
            cost_history <- cost_history[1:i]
            break
        }
    }

    return(list(theta = theta, cost_history = cost_history))
}

# One vs Rest
one_vs_rest <- function(X, y, taux_apprentissage, n_iterations, tolerance) {
    n <- ncol(X)
    K <- length(unique(y))

    theta <- matrix(0, nrow = n, ncol = K)

    for (k in 1:K) {
        y_k <- as.matrix(y[, k]) # Sélection de la classe k
        theta_result <- descente_gradient(X, y_k, rep(0, n), taux_apprentissage, n_iterations, tolerance)
        theta[, k] <- theta_result$theta
    }
    return(theta)
}

# One vs One
one_vs_one <- function(X, y, taux_apprentissage, n_iterations, tolerance) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
    theta_list <- list()
    for (i in 1:(K - 1)) {
        for (j in (i + 1):K) {
            y_ij <- y[y == i | y == j]
            X_ij <- X[y == i | y == j, ]
            theta_init <- rep(0, n)
            theta_result <- descente_gradient(X_ij, y_ij, theta_init, taux_apprentissage, n_iterations, tolerance)
            theta_list[[paste(i, j, sep = "_")]] <- theta_result$theta
        }
    }
    return(theta_list)
}

# Multinomial Logistic Regression
multinomial_logistic_regression <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
    print(n)
    print(K)

    theta <- matrix(0, nrow = n, ncol = K)
    for (iter in 1:n_iterations) {
        scores <- as.matrix(sapply(X, as.numeric)) %*% theta
        probs <- softmax(scores)
        gradient <- t(as.matrix(sapply(X, as.numeric))) %*% (probs - y) / m
        theta <- theta - learning_rate * gradient
    }
    return(theta)
}

# Fonction de prédiction
predict_simple <- function(X, theta) {
    return(apply(X, 1, function(x) which.max(hypothese(x, theta))))
}

# Prédiction multiclasse
predict_multiclass <- function(X, thetas) {
    probabilities <- sigmoid(X %*% thetas) # Probabilités pour k-1 classes
    probabilities <- cbind(probabilities, 1 - rowSums(probabilities)) # Ajout de la classe manquante
    return(max.col(probabilities)) # Classe avec la probabilité maximale
}

# Fonction de précision
precision <- function(y_true, y_pred) {
    return(sum(y_true == y_pred) / length(y_true))
}

# Fonction de rappel
recall <- function(y_true, y_pred) {
    return(sum(y_true == y_pred) / sum(y_true == 1))
}

# Fonction F1-score
f1_score <- function(y_true, y_pred) {
    return(2 * precision(y_true, y_pred) * recall(y_true, y_pred) / (precision(y_true, y_pred) + recall(y_true, y_pred)))
}

# Matrice de confusion
confusion_matrix <- function(y_true, y_pred) {
    K <- length(unique(y_true))
    cm <- matrix(0, nrow = K, ncol = K)
    for (i in 1:K) {
        for (j in 1:K) {
            cm[i, j] <- sum(y_true == i & y_pred == j)
        }
    }
    return(cm)
}
