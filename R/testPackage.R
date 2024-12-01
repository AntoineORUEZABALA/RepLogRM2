# Suppression des variables de l'environnement
rm(list = ls())

devtools::install_github("AntoineORUEZABALA/RepLogRM2@package")

setwd("D:/GitHub/RepLogRM2/R")
# library(mimosa)
library(R6)
library(datasets)
library(caret)
library(ggplot2)
source("LogisticRegression.R")

data <- read.csv("gym_members_exercise_tracking.csv")
cible <- "Workout_Type"
# Sauvegarde des noms des classes d'origine
class_names <- factor(unique(data[[cible]]))

# Encodage de la variable cible
data[[cible]] <- as.integer(as.factor(data[[cible]]))

# Prétraitement global des données (imputation, encodage des explicatives, normalisation)
data_prep <- pretraitement_complet(data, cible = cible)

# Séparation train/test
splits <- split_train_test(data_prep, cible = cible, proportion = 0.8)
X_train <- as.matrix(splits$X_train)
y_train <- splits$y_train
X_test <- as.matrix(splits$X_test)
y_test <- splits$y_test

# Réindexer les classes de y_train et y_test
y_train <- as.integer(as.factor(y_train)) - 1
y_test <- as.integer(as.factor(y_test)) - 1


model <- LogisticRegression$new(
  learning_rate = 0.001,         # Taux d'apprentissage
  max_iter = 10000,              # Nombre maximum d'itérations pour la descente de gradient
  classification_type = "softmax",  # Classification multinomiale (softmax)
  regularization = "l2",        # L2 pour la régularisation Ridge
  reg_lambda = 0.01,            # Hyperparamètre de régularisation

)

model$fit(X_train, y_train)

# Prédictions
predictions <- model$predict(X_test)

# Calculer la précision
accuracy <- mean(predictions == y_test)
cat(sprintf("Précision du modèle : %.2f%%\n", accuracy * 100))

# Matrice de confusion avec noms des classes
conf_matrix <- model$confusion_matrix(y_test, predictions)
rownames(conf_matrix) <- class_names
colnames(conf_matrix) <- class_names
cat("Matrice de confusion :\n")
print(conf_matrix)

# Probabilités d'appartenance avec noms des classes
probabilities <- model$predict_proba(X_test)

if (model$classification_type == "binary") {
  # Pour la classification binaire, ajouter une colonne pour la classe négative
  probabilities <- cbind(1 - probabilities, probabilities)  # Ajouter probabilité de classe négative
  colnames(probabilities) <- class_names  # Assigner les noms des classes
} else if (model$classification_type == "softmax") {
  # Pour softmax, assigner directement les noms des classes
  colnames(probabilities) <- class_names
}

# Afficher les probabilités avec noms des classes
cat("Probabilités d'appartenance (avec noms des classes) :\n")
print(probabilities)
# Courbe de la perte
ggplot(data.frame(epoch = 1:length(model$losses), loss = model$losses), aes(x = epoch, y = loss)) +
  geom_line() +
  labs(title = "Convergence de la perte", x = "Époque", y = "Perte")

print(model)
model$summary(class_names = class_names)
