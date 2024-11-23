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

# Fonction de coût pour la régression logistique
cout <- function(X, y, theta) {
    m <- nrow(X)
    h <- hypothese(X, theta)
    # Ajout d'un petit epsilon pour éviter les NaN
    epsilon <- 1e-15
    cost <- (-1 / m) * sum(y * log(h + epsilon) + (1 - y) * log(1 - h + epsilon))
    return(cost)
}

# Fonction de gradient pour la régression logistique
gradient <- function(X, y, theta) {
    m <- nrow(X)
    return(1 / m * t(X) %*% (hypothese(X, theta) - y))
}

# Descente de gradient pour la régression logistique
gradient_descent <- function(X, y, theta, learning_rate, n_iterations) {
    # Nombre de pas à prendre en compte pour l'historique
    pas_historique <- 1000
    historique_cout <- numeric()
    for (i in 1:n_iterations) {
        theta <- theta - learning_rate * gradient(X, y, theta)
        if (i %% (n_iterations / pas_historique) == 0) {
            cost <- cout(X, y, theta)
            # cat("Itération", i, "- Coût:", cost, "\n")
            historique_cout <- c(historique_cout, cost)
        }
    }
    return(list(theta = theta, historique_cout = historique_cout))
}

# One vs Rest
one_vs_rest <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- ncol(y) # k-1 classes (3 dans ce cas)
    theta <- matrix(0, nrow = n, ncol = K)

    for (k in 1:K) {
        y_k <- as.matrix(y[, k]) # Sélection de la classe k
        theta_result <- gradient_descent(X, y_k, rep(0, n), learning_rate, n_iterations)
        theta[, k] <- theta_result$theta
    }
    return(theta)
}

# One vs One
one_vs_one <- function(X, y, learning_rate, n_iterations) {
    m <- nrow(X)
    n <- ncol(X)
    K <- length(unique(y))
    theta_list <- list()
    for (i in 1:(K - 1)) {
        for (j in (i + 1):K) {
            y_ij <- y[y == i | y == j]
            X_ij <- X[y == i | y == j, ]
            theta_init <- rep(0, n)
            theta_result <- gradient_descent(X_ij, y_ij, theta_init, learning_rate, n_iterations)
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
