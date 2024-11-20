import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# Sigmoid function
def sigmoid(z):
    return 1 / (1 + np.exp(-z))


def hypothesis(X, theta):
    z = np.dot(X, theta)
    return sigmoid(z)


def cost_function(X, y, theta):
    m = len(y)
    h = hypothesis(X, theta)
    cost = (-1 / m) * np.sum(y * np.log(h) + (1 - y) * np.log(1 - h))
    return cost


def gradient_descent(X, y, theta, alpha, num_iters):
    m = len(y)
    J_history = []

    for _ in range(num_iters):
        h = hypothesis(X, theta)
        gradient = (1 / m) * np.dot(X.T, (h - y))
        theta -= alpha * gradient
        J_history.append(cost_function(X, y, theta))

    return theta, J_history


def one_vs_rest(X, y, num_classes, alpha, num_iters):
    n_features = X.shape[1]
    thetas = np.zeros((num_classes, n_features))

    for i in range(num_classes):
        y_binary = (y == i).astype(int)
        theta = np.zeros(n_features)
        theta, _ = gradient_descent(X, y_binary, theta, alpha, num_iters)
        thetas[i] = theta

    return thetas


def predict_multiclass(X, thetas):
    probabilities = np.array([hypothesis(X, theta) for theta in thetas])
    return np.argmax(probabilities, axis=0)


# Lecture des données
# Préciser le répertoire où se trouve le fichier : dans datasets par rapport au répertoire courant
df = pd.read_csv("datasets/gym_members_exercise_tracking.csv")
# df = pd.read_csv("gym_members_exercise_tracking.csv")
cible = "Gender"
X = df.drop(cible, axis=1)

# Séparation des variables numériques et catégorielles
num_vars = X.select_dtypes(include="number")
cat_vars = X.select_dtypes(include="object")

# Imputation des valeurs manquantes numériques uniquement sur les colonnes numériques
X.fillna(num_vars.mean(), inplace=True)
# Imputation des valeurs manquantes catégorielles uniquement sur les variables catégorielles
X.fillna(cat_vars.mode().iloc[0], inplace=True)
# Normalisation des variables numériques uniquement sur les colonnes numériques
X[num_vars.columns] = (num_vars - num_vars.mean()) / num_vars.std()
# Encodage des variables catégorielles, uniquement sur les variables catégorielles
X = pd.get_dummies(X, columns=cat_vars.columns, dtype=int)

y = df[cible]
# Encodage des variables catégorielles, uniquement sur les variables catégorielles
y = pd.factorize(y)[0]  # Convert categories to numerical labels

# Initialize parameters
theta = np.zeros(X.shape[1])

# Train the model
alpha = 0.01
num_iters = 1000
num_classes = len(np.unique(y))

if num_classes == 1:
    print("Il faut au moins 2 classes pour faire de la classification")
elif num_classes == 2:
    # Cas binaire
    theta, J_history = gradient_descent(X, y, theta, alpha, num_iters)
    # Plot cost vs iterations
    # plt.plot(J_history)
    # plt.xlabel("Iterations")
    # plt.ylabel("Cost")
    # plt.title("Cost vs. Iterations")
    # plt.show()

    # Create DataFrame with coefficients and variable names
    coef_df = pd.DataFrame({"Variable": X.columns, "Coefficient": theta})

    # Sort by absolute coefficient value for better visualization
    coef_df["Abs_Coefficient"] = abs(coef_df["Coefficient"])
    coef_df = coef_df.sort_values("Abs_Coefficient", ascending=False)

    print("\nCoefficients for each variable:")
    print(coef_df[["Variable", "Coefficient"]])
else:
    # Cas multiclasse
    thetas_multiclass = one_vs_rest(X, y, num_classes, alpha, num_iters)
    predictions_multiclass = predict_multiclass(X, thetas_multiclass)

    # Créer un DataFrame avec les coefficients
    coef_df = pd.DataFrame(
        thetas_multiclass,
        columns=X.columns,  # Noms des variables
        index=pd.get_dummies(
            df[cible]
        ).columns,  # Noms des modalités de la variable cible
    )

    # Afficher le DataFrame
    print("\nCoefficients par modalité et variable :")
    print(coef_df)
