rm(list = ls())
library(R6)
source("utils/utils.r")
source("utils/preprocessing.r")
source("utils/LogisticRegression.r")


# Définition de la classe R6 LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",

    # Attributs publics
    public = list(
        modalites_cible = NULL,
        nb_modalites_cible = NULL,
        cible = NULL,
        data = NULL,
        modalite_ref = NULL,

        # Constructeur
        initialize = function(data, cible) {
            self$data <- data
            self$cible <- cible
            # Vérifications ##########################
            # X doit être un data.frame
            check_type(self$data, "data.frame")

            # y doit être un vecteur ou un character
            check_type(self$cible, "character")

            # La cible doit être présente dans les données
            if (!self$cible %in% colnames(self$data)) {
                stop("La variable ", self$cible, " doit être présente dans le dataframe")
            }

            # Les données de la cible doivent être de type catégorielle (character ou factor)
            if (!is.character(self$data[[self$cible]]) && !is.factor(self$data[[self$cible]])) {
                stop("La variable cible doit être de type character ou factor")
            }

            # Il faut au moins deux modalités pour la variable cible
            if (length(unique(self$data[[self$cible]])) < 2) {
                stop("La variable cible doit avoir au moins deux modalités")
            }

            # Séparation en deux jeux ##########################
            # Extraction de la variable cible
            private$y <- self$data[[self$cible]]
            # Modalités de la cible
            self$modalites_cible <- levels(as.factor(private$y))
            self$nb_modalites_cible <- length(unique(self$data[[self$cible]]))

            # On enlève la cible des variables explicatives
            self$data[[self$cible]] <- NULL

            # Pré-traitement ##########################
            # Variables explicatives
            private$X <- private$preprocess_data(self$data)

            # Variable cible
            # création d'une matrice one hot
            private$y <- encodage_one_hot(private$y)
            # Déduire de la matrice one hot la colonne correspondant à la modalité de référence
            self$modalite_ref <- colnames(private$y)[which.max(colSums(private$y))]
            # Enlever les caractères .data_ de la variable self$modalite_ref
            self$modalite_ref <- gsub(".data_", "", self$modalite_ref)
        },

        ########################################
        # Méthode fit ##########################
        ########################################
        fit = function() {
            # Régression ##########################
            # Entraînement du modèle
            taux_apprentissage <- 0.00001
            num_iters <- 10000
            # theta <- rep(0, ncol(private$X))
            theta <- matrix(0, nrow = ncol(private$X), ncol = ncol(private$y))

            # Convertir X, y et theta en matrice pour pouvoir la multiplier avec theta
            # dans les fonctions de coût et de gradient
            X <- as.matrix(private$X)
            y <- as.matrix(private$y)

            # Cas modalité binaire
            if (self$nb_modalites_cible == 2) {
                # Cas binaire
                print("Binary classification")
                result <- gradient_descent(X, y, theta, taux_apprentissage, num_iters)

                # Récupération des résultats
                theta <- result$theta
                cost_history <- result$cost_history

                # Création du dataframe avec les coefficients
                coef_df <- data.frame(
                    Variable = colnames(X),
                    Coefficient = as.numeric(theta)
                )

                # Tri par valeur absolue et affichage des coefficients
                coef_df$Abs_Coefficient <- abs(coef_df$Coefficient)
                coef_df <- coef_df[order(-coef_df$Abs_Coefficient), ]
                print("\nCoefficients for each variable:")
                print(coef_df[, c("Variable", "Coefficient")])

                # Affichage d'un graphique montrant l'évolution du coût
                plot(cost_history, type = "l", xlab = "Iteration", ylab = "Cost", main = "Cost evolution during training")
            } else {
                # Cas modalités > 2
                thetas_multiclass <- one_vs_rest(X, y, taux_apprentissage, num_iters)
                predictions_multiclass <- predict_multiclass(X, thetas_multiclass)

                # Création du dataframe avec les coefficients Linh Nhi
                coef_df <- as.data.frame(t(thetas_multiclass))
                names(coef_df) <- colnames(X) # Les noms des variables explicatives
                rownames(coef_df) <- self$modalites_cible[-1] # Les noms des classes automatiquement récupérés

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
            print(paste0("Les données comportent ", nrow(self$data), " observations et ", ncol(self$data), " variables"))
            print(paste0("La variable cible est '", self$cible, "' avec ", self$nb_modalites_cible, " modalités"))
            print(paste0("La modalité de référence est '", self$modalite_ref, "'"))
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

        # Variables privées
        X = NULL,
        y = NULL,

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
                data <- encodage_one_hot(data)
            }
            return(data)
        }
    )
)

# Chargement des données depuis les fichiers csv train et test
# Définir le répertoire par défaut comme celui où se situe le programme R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("datasets/gym_members_exercise_tracking.csv")
# data <- read.csv("datasets/ricco.csv")
cible <- "Workout_Type"
# cible <- "Gender"
# cible <- "type"


# utilisation de LogisticRegression
# Instanciation de la classe LogisticRegression
LogisticRegression1 <- LogisticRegression$new(data, cible)

# Affichage d'une variable publique
print(LogisticRegression1$nb_modalites_cible)

# Affichage d'un résumé grâce à la méthode print
LogisticRegression1$print()

# Modélisation
# LogisticRegression1$fit()

# LogisticRegression1$predict()
# LogisticRegression1$predict_proba()
# LogisticRegression1$summary()
