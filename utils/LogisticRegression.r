# Fonction sigmoïde
sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
}

# Fonction d'hypothèse
hypothesis <- function(X, theta) {
    z <- as.matrix(X) %*% theta
    return(sigmoid(z))
}

# Fonction de coût pour la régression logistique binaire
cost <- function(X, y, theta) {
    m <- nrow(X)
    h <- hypothesis(X, theta)
    cost <- (-1 / m) * sum(y * log(h) + (1 - y) * log(1 - h))
    return(cost)
}

# Fonction de gradient pour la régression logistique
gradient <- function(X, y, theta) {
    m <- nrow(X)
    return(1 / m * t(X) %*% (hypothesis(X, theta) - y))
}

# Descente de gradient pour la régression logistique
gradient_descent <- function(X, y, theta, learning_rate, n_iterations) {
    for (i in 1:n_iterations) {
        theta <- theta - learning_rate * gradient(X, y, theta)
        if (i %% 100 == 0) {
            cat("Itération", i, "- Coût:", cost(X, y, theta), "\n")
        }
    }
    return(theta)
}

# One vs Rest
one_vs_rest <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
    theta <- matrix(0, nrow = n, ncol = K)
    for (k in 1:K) {
        y_k <- y == k
        theta[, k] <- gradient_descent(X, y_k, theta[, k], learning_rate, n_iterations)
    }
    return(theta)
}

# One vs One
one_vs_one <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
    theta <- matrix(0, nrow = n, ncol = K)
    for (i in 1:(K - 1)) {
        for (j in (i + 1):K) {
            y_ij <- (y == i) | (y == j)
            theta[, i] <- gradient_descent(X, y_ij, theta[, i], learning_rate, n_iterations)
        }
    }
    return(theta)
}

# Descente de gradient Softmax
softmax <- function(x) {
    exp_x <- exp(x)
    return(exp_x / sum(exp_x))
}

# Multinomial Logistic Regression
multinomial_logistic_regression <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
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
predict <- function(X, theta) {
    return(apply(X, 1, function(x) which.max(hypothesis(x, theta))))
}

# Prédiction multiclasse
predict_multiclass <- function(X, thetas) {
    # transformer X et thetas en matrice
    X <- as.matrix(X)
    thetas <- as.matrix(thetas)
    probabilities <- t(apply(thetas, 1, function(theta) hypothesis(X, theta)))
    return(max.col(probabilities) - 1) # -1 pour correspondre à l'indexation Python
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
