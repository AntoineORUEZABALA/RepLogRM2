# Suppression des variables de l'environnement
rm(list = ls())


# Définir le répertoire par défaut comme celui où se situe le programme R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Importation des librairies
library(R6)
library(caret)
source("utils/utils.r")
source("utils/preprocessing.r")
source("utils/LogisticRegression.r")


# Définition de la classe R6 LogisticRegression
LogisticRegression <- R6Class("LogisticRegression",

    # Attributs publics
    public = list(
        modalites_cible = NULL, # La liste des modalités de la variable cible
        nb_modalites_cible = NULL, # le nombre de ces modalités
        modalite_ref_cible = NULL, # La modalité de référence, qui n'apparaît pas dans la matrice one-hot
        cible = NULL, # La variable cible
        data = NULL, # le jeu de données complet
        type_regression = NULL, # Type de régression, binaire ou multinomiale


        ########################################
        # Constructeur #########################
        ########################################
        initialize = function(data, cible) {
            self$data <- data
            self$cible <- cible


            # Vérifications ##############################################
            # X doit être un data.frame
            check_type(self$data, "data.frame")

            # y doit être un vecteur ou un character
            check_type(self$cible, "character")

            # La cible doit être présente dans les données
            if (!self$cible %in% colnames(self$data)) {
                stop("La variable '", self$cible, "'' doit être présente dans le dataframe")
            }

            # Les données de la cible doivent être de type catégorielle (character ou factor)
            if (!is.character(self$data[[self$cible]]) && !is.factor(self$data[[self$cible]])) {
                stop("La variable cible doit être de type character ou factor")
            }

            # Il faut au moins deux modalités pour la variable cible
            if (length(unique(self$data[[self$cible]])) < 2) {
                stop("La variable cible doit avoir au moins deux modalités")
            }
            # Fin des Vérifications ######################################


            # Extraction des modalités de la cible #######################
            # Les modalités
            self$modalites_cible <- levels(as.factor(self$data[[self$cible]]))
            # leur nombre
            self$nb_modalites_cible <- length(self$modalites_cible)
            # D'où le type de régression logistique
            if (self$nb_modalites_cible == 2) {
                self$type_regression <- "Régression logistique binaire"
            } else {
                self$type_regression <- "Régression logistique multinomiale"
            }
            # Fin de extraction des modalités de la cible #################


            # Suppression des lignes où la cible est absente #############
            self$data <- self$data[!is.na(self$data[[self$cible]]), ]
            # Fin de suppression des lignes où la cible est absente ######


            # Imputation des valeurs manquantes ##########################
            # On suppose que les données manquantes sont aléatoires,
            # on réalise donc l'opération avant la séparation train/test
            self$data <- private$auto_impute(self$data)
            # Fin imputation des valeurs manquantes ######################

            # Séparation train/test ######################################
            splits <- split_train_test(self$data, self$cible, proportion = 0.8)
            private$X_train <- splits$X_train
            private$X_test <- splits$X_test
            private$y_train <- splits$y_train
            private$y_test <- splits$y_test
            # Fin de la séparation train/test ############################


            # Pré-traitement pour X ######################################
            # Normalisation/standardisation des données
            # Création du modèle de prétraitement sur private$X_train
            preprocess_params <- preProcess(private$X_train, method = c("center", "scale"))
            # Application du modèle sur private$X_train et private$X_test
            private$X_train <- predict(preprocess_params, private$X_train)
            private$X_test <- predict(preprocess_params, private$X_test)

            # One-hot encoding des variables catégorielles
            # Création d'un modèle pour les dummy variables
            dummy_model <- dummyVars(" ~ .", data = private$X_train, fullRank = TRUE)
            private$X_train <- predict(dummy_model, private$X_train)
            private$X_test <- predict(dummy_model, private$X_test)

            # Conversion en data.frame
            private$X_train <- as.data.frame(private$X_train)
            private$X_test <- as.data.frame(private$X_test)

            # Conversion en matrix
            private$X_train <- as.matrix(private$X_train)
            private$X_test <- as.matrix(private$X_test)

            # Extraction de la modalité de référence
            self$modalite_ref_cible <- self$modalites_cible[1]
            # Fin de pré-traitement pour X ################################

            # Pré-traitement pour y ######################################
            # Encodage en matrice de classes
            dummy_model_y <- dummyVars(" ~ .", data = data.frame(y = private$y_train), fullRank = TRUE)
            private$y_train <- as.matrix(predict(dummy_model_y, data.frame(y = private$y_train)))
            private$y_test <- as.matrix(predict(dummy_model_y, data.frame(y = private$y_test)))
            # Fin de pré-traitement pour y ################################
        },

        ########################################
        # Méthode fit ##########################
        ########################################
        fit = function() {
            # Régression ##########################
            # Entraînement du modèle
            taux_apprentissage <- 0.01
            n_iterations <- 20000
            tolerance <- 1e-6

            theta <- matrix(0, nrow = ncol(private$X_train), ncol = ncol(private$y_train))

            if (self$nb_modalites_cible == 2) {
                # Cas modalité binaire
                result <- gradient_descent(private$X_train, private$y_train, theta, taux_apprentissage, n_iterations, tolerance)

                # Récupération des résultats
                theta <- result$theta
                cost_history <- result$cost_history

                # Création du dataframe avec les coefficients
                coef_df <- data.frame(
                    Variable = colnames(private$X_train),
                    Coefficient = as.numeric(theta)
                )

                # Tri par valeur absolue et affichage des coefficients
                coef_df$Abs_Coefficient <- abs(coef_df$Coefficient)
                coef_df <- coef_df[order(-coef_df$Abs_Coefficient), ]
                print("\nCoefficients :")
                print(coef_df[, c("Variable", "Coefficient")])

                # Affichage de l'évolution du coût
                if (!any(!is.finite(cost_history))) {
                    plot(1:length(cost_history), cost_history,
                        type = "l",
                        col = "blue",
                        xlab = "Itération",
                        ylab = "Coût",
                        main = "Évolution du coût pendant l'apprentissage"
                    )
                } else {
                    warning("Cost history contains non-finite values, unable to plot.")
                }
            } else {
                # Cas modalités > 2
                thetas_multiclass <- one_vs_rest(private$X_train, private$y_train, taux_apprentissage, n_iterations, tolerance)
                predictions_multiclass <- predict_multiclass(private$X_train, thetas_multiclass)

                # Création du dataframe avec les coefficients
                coef_df <- as.data.frame(t(thetas_multiclass))
                names(coef_df) <- colnames(private$X_train) # Les noms des variables explicatives
                rownames(coef_df) <- self$modalites_cible[-1] # Les noms des classes automatiquement récupérés

                print("\nCoefficients par modalité et variable :")
                print(coef_df)
            }
        },

        ########################################
        # Fonction predict #####################
        ########################################
        prediction = function(X) {
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
            print(paste0("La modalité de référence de ", self$cible, " est '", self$modalite_ref_cible, "'"))
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
        X_train = NULL,
        X_test = NULL,
        y_train = NULL,
        y_test = NULL,

        # Fonction d'imputation des données
        auto_impute = function(data) {
            # Séparation en variables numériques et catégorielles
            numeric_cols <- sapply(data, is.numeric)
            categorical_cols <- !numeric_cols

            # Traitement des valeurs manquantes
            data[numeric_cols] <- imputer_numeriques(data[numeric_cols])
            data[categorical_cols] <- imputer_categorielles(data[categorical_cols])

            return(data)
        },

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

            return(data)
        }
    )
)


# Chargement des données depuis les fichiers csv train et test
data <- read.csv("datasets/gym_members_exercise_tracking.csv")
# data <- read.csv("datasets/ricco.csv")
cible <- "Workout_Type"
cible <- "Gender"
# cible <- "type"


# utilisation de LogisticRegression
# Instanciation de la classe LogisticRegression
LogisticRegression1 <- LogisticRegression$new(data = data, cible = cible)


# Affichage d'une variable publique
# print(LogisticRegression1$nb_modalites_cible)


# Affichage d'un résumé grâce à la méthode print
LogisticRegression1$print()


# Modélisation
LogisticRegression1$fit()


# LogisticRegression1$predict()
# LogisticRegression1$predict_proba()
# LogisticRegression1$summary()
