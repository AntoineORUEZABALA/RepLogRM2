# MIMOSA : A Mixed Input Multinomial Optimization for Statistical Analysis

## Table of Contents

- [Description](#description)
- [Installation](#installation)
- [Documentation](#documentation)
- [Dataset](#dataset)
- [Shiny Application](#shiny-application)
- [Usage of the package](#usage-of-the-package)
- [Contribution](#contribution)
- [Authors](#authors)  


## Description

MIMOSA (for "Mixed Inputs Multinomial Optimization for Statistical Analysis") provides a custom implementation of logistic regression models, supporting both binary and multinomial classification tasks. The package handles explanatory variables of mixed types, including both quantitative and qualitative variables. It includes preprocessing utilities for missing value imputation, one-hot encoding of categorical variables, and normalization of numerical variables. Designed for flexibility and ease of use, MIMOSA is suitable for a wide range of logistic regression applications.

## Installation

Make sure you have R (>= 4.0.0) installed. The package relies on the following dependencies:

- R6
- caret
- ggplot2
- pmml

If needed, you can install using the following commands :
```bash
install.packages("R6")
install.packages("caret")
install.packages("ggplot2")
install.packages("pmml") 
```
To install the package, please use the following code lines:
```bash
library(devtools)
install_github("AntoineORUEZABALA/RepLogRM2")
```
And then to import it :
```bash
library(mimosa)
```
## Documentation
In case of any difficulties using one of the function, you can refer to the documentation of the function with these two possibilities:
```bash
help(LogisticRegression)

?LogisticRegression
```
## Dataset
Five datasets are included within the package in order to test the different models. The link refers to the source of the dataset :

- "iris" - [lien](https://www.kaggle.com/datasets/arshid/iris-flower-dataset) : This classic dataset contains measurements of flowers from three Iris species, ideal for classification tasks.
- breast_cancer - [lien](https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data) : Data on breast tumors used to predict whether they are benign or malignant.
- titanic  - [lien](https://www.kaggle.com/c/titanic/data) : Passenger data from the Titanic, commonly used to predict survival rates based on various features.
- depression  - [lien](https://www.kaggle.com/datasets/arashnic/the-depression-dataset) : Dataset containing survey data to analyze mental health and depression symptoms.
- gym_members_exercise_tracking  - [lien](https://www.kaggle.com/datasets/valakhorasani/gym-members-exercise-dataset) : This dataset provides a detailed overview of gym members' exercise routines, physical attributes, and fitness metrics. It can be used to predict various outcomes such as Workout_Type or Gender.<br><br>

To import a dataset, use the function :
```bash
data("depression")
```

To get more informations about the dataset, use :
```bash
?depression
```
## Usage of the package 
Here is an example of how to use the package with the gym_members_exercise_tracking dataset, demonstrating a use case where the target variable is multiclass (not binary):


```bash

# Load the dataset
data("gym_members_exercise_tracking")
data <- gym_members_exercise_tracking

# Encode the target variable
target <- "Workout_Type"
class_names <- factor(unique(data[[target]]))  # Save class names
data[[target]] <- as.integer(as.factor(data[[target]]))  # Encode target

# Preprocess the data (imputation, encoding, normalization)
data_prep <- pretraitement_complet(data, cible = target)

# Split the data into training and testing subsets
splits <- split_train_test(data_prep, cible = target, proportion = 0.8)
X_train <- as.matrix(splits$X_train)
y_train <- as.integer(as.factor(splits$y_train)) - 1  # Re-index classes
X_test <- as.matrix(splits$X_test)
y_test <- as.integer(as.factor(splits$y_test)) - 1  # Re-index classes

# Train a logistic regression model
model <- LogisticRegression$new(
  learning_rate = 0.001,
  max_iter = 10000,
  classification_type = "softmax",
  regularization = "l2",
  reg_lambda = 0.01
)

model$fit(X_train, y_train)

# Make predictions
predictions <- model$predict(X_test)
```
To see some results :
```bash
# Calculate accuracy
accuracy <- mean(predictions == y_test)
cat(sprintf("Model Accuracy: %.2f%%\n", accuracy * 100))
```
```bash
Model Accuracy : 26.94%
```

```bash
# Display confusion matrix
conf_matrix <- model$confusion_matrix(y_test, predictions)
rownames(conf_matrix) <- class_names
colnames(conf_matrix) <- class_names
cat("Confusion Matrix:\n")
print(conf_matrix)
```
```bash
         Yoga HIIT Cardio Strength
Yoga       17    6     23        9
HIIT       14    6     21        7
Cardio      8   10     18        7
Strength   14    4     18       11

```

```bash
# Display class probabilities
probabilities <- model$predict_proba(X_test)
if (model$classification_type == "binary") {
  probabilities <- cbind(1 - probabilities, probabilities)
  colnames(probabilities) <- class_names
} else if (model$classification_type == "softmax") {
  colnames(probabilities) <- class_names
}
cat("Class Probabilities:\n")
print(head(probabilities, 15)) #Display the first 15 class membership probabilities with class names
```
```bash
        Yoga      HIIT    Cardio  Strength
6  0.2105855 0.2852488 0.2462032 0.2579624
7  0.1485123 0.2120205 0.4604935 0.1789737
24 0.4593221 0.2084412 0.1538710 0.1783657
42 0.2735298 0.2001044 0.2912302 0.2351356
43 0.2839803 0.1973860 0.3280369 0.1905968
45 0.2278500 0.2493743 0.3977008 0.1250749
52 0.2282965 0.2492882 0.2524866 0.2699287
53 0.2906739 0.2624612 0.2663905 0.1804744
60 0.3275733 0.1907479 0.2042270 0.2774519
61 0.3159349 0.2243855 0.1759911 0.2836885
63 0.2653401 0.2793836 0.2229950 0.2322812
67 0.2012261 0.2814643 0.3188840 0.1984255
77 0.2017565 0.2524061 0.3085243 0.2373131
80 0.2517758 0.2563596 0.2136808 0.2781838
82 0.2370455 0.2294231 0.1679197 0.3656117
```
You can easily visualize the loss convergence to see how the model's loss decreases over the training epochs. Use the following code snippet to generate a plot :
```bash
# Visualize loss convergence
ggplot(data.frame(epoch = 1:length(model$losses), loss = model$losses), aes(x = epoch, y = loss)) +
  geom_line() +
  labs(title = "Loss Convergence", x = "Epoch", y = "Loss")
```
![loss](https://github.com/user-attachments/assets/69e438c9-225e-43c2-a7a1-68666f60c112)

The summary() function provides a detailed overview of the logistic regression model, including information such as the type of classification, regularization parameters, model weights, coefficients, and training losses. This is especially useful for understanding the model's behavior and evaluating its parameters.

```bash
# Model summary
model$summary(class_names = class_names)
```
```bash
Logistic Regression Model
-------------------------
Type of Classification: softmax
Number of Classes: 4
Learning Rate: 0.0010
Max Iterations: 10000
Regularization: l2
Regularization Lambda: 0.0100
Tolerance: 0.000100

Model Weights :
Intercepts:
     Yoga      HIIT    Cardio  Strength 
0.7715114 0.6516200 0.7543156 0.7177300 

Coefficients (sorted by absolute value across all classes):
                                     Yoga        HIIT       Cardio
Calories_Burned              -0.350948520  0.58351216  0.782075442
Experience_Level             -0.335499131 -0.53478498 -0.506651827
Height_m                      0.229183036  0.54006915  0.212687152
Weight_kg                    -0.143277467 -0.63482804 -0.144091266
BMI                          -0.331186791  0.14040580 -0.476644678
Session_Duration_hours        0.584448462 -0.07234845 -0.163426881
Water_Intake_liters           0.440909699  0.34678473  0.122458243
Workout_Frequency_days_week_ -0.372727233 -0.27433852 -0.197350908
Avg_BPM                      -0.039311737 -0.34839303 -0.324947658
Max_BPM                      -0.246272346 -0.19596549 -0.288745634
Fat_Percentage                0.192074871  0.09144325  0.284770430
GenderMale                   -0.008039238 -0.20011318  0.170731459
Age                          -0.314061660 -0.02270484 -0.005560016
Resting_BPM                   0.053611008  0.10562877  0.057142234
                                 Strength
Calories_Burned              -0.066538705
Experience_Level             -0.336148341
Height_m                      0.414765755
Weight_kg                    -0.456591785
BMI                          -0.231463800
Session_Duration_hours        0.359022016
Water_Intake_liters           0.226392496
Workout_Frequency_days_week_ -0.271030226
Avg_BPM                      -0.177217758
Max_BPM                      -0.144247679
Fat_Percentage                0.088650400
GenderMale                    0.127409801
Age                          -0.120262218
Resting_BPM                   0.005467021

Training Losses:
 [1] 4.053194 3.589355 3.197198 2.868449 2.595659 2.373039 2.194729 2.053758
 [9] 1.942670 1.854677 1.784293 1.727346 1.680717 1.642071 1.609661 1.582182
[17] 1.558652 1.538332 1.520661 1.505204 1.491619 1.479633 1.469024 1.459608
[25] 1.451230 1.443760 1.437085 1.431110 1.425752 1.420937 1.416603 1.412696
[33] 1.409166 1.405973 1.403079 1.400451 1.398061 1.395884 1.393898 1.392083
[41] 1.390422 1.388899 1.387500 1.386214 1.385029 1.383936 1.382927 1.381993
[49] 1.381127 1.380324 1.379578 1.378884 1.378237 1.377633 1.377069 1.376541
[57] 1.376047 1.375583 1.375147 1.374737 1.374351 1.373986 1.373643 1.373318
[65] 1.373010 1.372718 1.372442 1.372179 1.371929 1.371691 1.371464 1.371247
[73] 1.371041 1.370843 1.370654 1.370472 1.370298 1.370131 1.369971 1.369816
[81] 1.369668 1.369525 1.369386 1.369253 1.369124 1.369000 1.368880 1.368763
[89] 1.368650 1.368541 1.368435 1.368332 1.368232
Final Loss: 1.368232
```
By default, the summary() function displays a plot of the most important variables based on their absolute values, allowing you to quickly identify which features have the greatest impact on the model's predictions.

![featuresImportance](https://github.com/user-attachments/assets/9591c462-eaad-474d-8228-a05c7a6fd7b4)
```

## Shiny Application

We have also developed a **Shiny application** to interact with our package.

### How to Launch the Application:
- Open the file `Mimosa - RShiny.R` on RStudio.
- Click the **"Run App"** button.

### Description of the Shiny Application:
The application is organized into 4 tabs:
1. **Data Overview**: Displays information about the dataset and provides an interactive table to view the data.
2. **Statistical Summary**: Shows the summary statistics for different variables in the dataset (Minimum, 1st Quartile, Median, Mean, 3rd Quartile, and Maximum).
3. **Logistic Regression with GLM/Multinom**: Performs logistic regression using the GLM function for binary variables or the Multinom function for multinomial variables.
4. **Logistic Regression with Mimosa**: Performs logistic regression using the MIMOSA method from our custom package.


## Contribution
Contributions are welcome! To contribute:
- Fork the project.
- Create your feature branch (git checkout -b feature/FeatureName)..
- Commit your changes (git commit -m 'Description of changes').
- Push to the branch (git push origin feature/FeatureName).
- Open a Pull Request.


## Authors
- Antoine ORUEZABALA
- Linh Nhi LE DINH
- Béranger THOMAS
