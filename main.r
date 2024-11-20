rm(list = ls())
library(R6)
source("utils/utils.r")
source("utils/preprocessing.r")
source("utils/LogisticRegression.r")


# Définition de la classe R6 LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",

    # Attributs publics
    public = list(

        # Constructeur
        initialize = function() {
            # Vide
        },

        ########################################
        # Méthode fit ##########################
        ########################################
        fit = function(X, y) {
            # Vérifications ##########################
            # X doit être un data.frame
            check_type(X, "data.frame")

            # y doit être un vecteur ou un character
            check_type(y, "character")

            # La cible doit être présente dans les données
            if (!y %in% colnames(X)) {
                stop("La variable ", y, " doit être présente dans le dataframe")
            }

            # Les données de la cible doivent être de type catégorielle (character ou factor)
            if (!is.character(X[[y]]) && !is.factor(X[[y]])) {
                stop("La variable cible doit être de type character ou factor")
            }

            # Il faut au moins deux modalités pour la variable cible
            if (length(unique(X[[y]])) < 2) {
                stop("La variable cible doit avoir au moins deux modalités")
            }

            # Séparation en deux jeux ##########################
            # Extraction de la variable cible
            target_data <- X[[y]]

            # On enlève la cible des variables explicatives
            X[[y]] <- NULL

            # Pré-traitement ##########################
            # Variables explicatives
            X <- private$preprocess_data(X)

            # Variable cible
            # création d'une matrice one hot
            matrice_y_onehot <- as.matrix(encodage_one_hot(target_data))

            # Régression ##########################
            # Entraînement du modèle
            taux_apprentissage <- 0.01
            num_iters <- 10000
            num_classes <- length(unique(y))
            theta <- rep(0, ncol(X))

            # Convertir X et theta en matrice pour pouvoir la multiplier avec theta
            # dans les fonctions de coût et de gradient
            X <- as.matrix(X)
            theta <- as.matrix(theta)

            # Cas modalité binaire
            if (num_classes == 2) {
                # Cas binaire
                result <- gradient_descent(X, y, theta, taux_apprentissage, num_iters)
                theta <- result$theta
                J_history <- result$J_history

                # Création du dataframe avec les coefficients
                coef_df <- data.frame(
                    Variable = colnames(X),
                    Coefficient = theta
                )

                # Tri par valeur absolue des coefficients
                coef_df$Abs_Coefficient <- abs(coef_df$Coefficient)
                coef_df <- coef_df[order(-coef_df$Abs_Coefficient), ]

                print("\nCoefficients for each variable:")
                print(coef_df[, c("Variable", "Coefficient")])
            } else {
                # Cas modalités > 2
                thetas_multiclass <- one_vs_rest(X, y, taux_apprentissage, num_iters)
                predictions_multiclass <- predict_multiclass(X, thetas_multiclass)

                # Création du dataframe avec les coefficients
                coef_df <- as.data.frame(thetas_multiclass)
                names(coef_df) <- colnames(X)
                rownames(coef_df) <- levels(factor(df[[cible]]))

                print("\nCoefficients par modalité et variable :")
                print(coef_df)
            }
        },

        ########################################
        # Fonction predict #####################
        ########################################
        predict = function(X) {
            # Vérifier d'abord si le modèle a déjà fait son apprentissage
            if (!exists("weights") || !exists("bias")) {
                stop("Le modèle n'a pas encore été entraîné. Veuillez d'abord utiliser la méthode fit()")
            }
            print("Hello, je suis la fonction predict ! je prends en entrée un df X compatible avec celui présenté à fit(), et qui renvoie la classe prédite pour chaque individu.")
        },

        ########################################
        # Fonction predict_proba ###############
        ########################################
        predict_proba = function(X) {
            # Vérifier d'abord si le modèle a déjà fait son apprentissage
            if (!exists("weights") || !exists("bias")) {
                stop("Le modèle n'a pas encore été entraîné. Veuillez d'abord utiliser la méthode fit()")
            }
            print("Hello, je suis la fonction predict_proba ! Je renvoie la probabilité d'appartenance aux classes")
        },

        ########################################
        # Procédure print ######################
        ########################################
        print = function() {
            print("Hello, je suis la procédure print ! je renvoie des infos succintes sur le modèle")
        },

        ########################################
        # Procédure summary ####################
        ########################################
        summary = function() {
            print("Hello, je suis la procédure summary ! j'affiche des infos détaillées")
        }
    ),

    # Attributs privés
    private = list(

        # Fonction de prétraitement des données
        preprocess_data = function(data) {
            # Séparation en variables numériques et catégorielles
            numeric_cols <- sapply(data, is.numeric)
            categorical_cols <- !numeric_cols

            # Traitement des valeurs manquantes
            data[numeric_cols] <- imputer_numeriques(data[numeric_cols])
            data[categorical_cols] <- imputer_categorielles(data[categorical_cols])

            # Normalisation des variables numériques
            data[numeric_cols] <- lapply(data[numeric_cols], normaliser)

            # One-hot encoding des variables catégorielles
            if (any(categorical_cols)) {
                data <- encodage_one_hot(data[categorical_cols])
            }
            return(data)
        }
    )
)

# Chargement des données depuis les fichiers csv train et test
# Définir le répertoire par défaut comme celui où se situe le programme R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("datasets/gym_members_exercise_tracking.csv")
cible <- "Workout_Type"
# cible <- "Gender"

# utilisation de LogisticRegression
LogisticRegression1 <- LogisticRegression$new()
# Afficher les informations renvoyées par la méthode fit
LogisticRegression1$fit(data, cible)
# LogisticRegression1$fit(data, cible)
# LogisticRegression1$predict()
# LogisticRegression1$predict_proba()
# LogisticRegression1$summary()
