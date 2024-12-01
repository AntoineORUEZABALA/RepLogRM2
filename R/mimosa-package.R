#' Mimosa: A Logistic Regression and Preprocessing Package
#'
#' **Mimosa** is a package designed for logistic regression and comprehensive data preprocessing.
#' It includes tools for binary and multinomial logistic regression, as well as a full pipeline
#' for handling missing data, encoding categorical variables, and normalizing numerical variables.
#'
#' @section Key Features:
#' - **Logistic Regression**: Train and evaluate binary or multinomial logistic regression models
#'   with L1/L2 regularization and gradient descent.
#' - **Data Preprocessing**: Functions for imputing missing values, one-hot encoding categorical variables,
#'   and normalizing numerical data.
#' - **Preprocessing Pipeline**: An all-in-one function for preparing datasets for machine learning.
#'
#' @section Core Functions:
#' - **Logistic Regression Class**:
#'   - `LogisticRegression`: An R6 class for creating and training logistic regression models.
#'   - Methods include `fit`, `predict`, `predict_proba`, and performance metrics such as `precision`,
#'     `recall`, `f1_score`.
#'
#' - **Data Preprocessing**:
#'   - `imputer_numeriques`: Impute missing values for numerical variables using mean or median.
#'   - `imputer_categorielles`: Impute missing values for categorical variables using the mode.
#'   - `split_train_test`: Split datasets into training and testing sets.
#'   - `encoder_one_hot`: Perform one-hot encoding for categorical variables.
#'   - `normaliser_donnees`: Normalize numerical variables.
#'   - `pretraitement_complet`: Perform complete preprocessing, including imputation, encoding, and normalization.
#'
#' @examples
#' library(mimosa)
#'
#' # Preprocessing example
#' data <- data.frame(
#'   Age = c(25, 30, NA, 40),
#'   Gender = c("M", "F", "M", NA),
#'   Salary = c(50000, 60000, 55000, NA),
#'   Target = c(1, 0, 1, 0)
#' )
#'
#' processed_data <- pretraitement_complet(data, cible = "Target")
#' print(processed_data)
#'
#' # Logistic Regression example
#' model <- LogisticRegression$new(learning_rate = 0.01, max_iter = 500)
#' model$fit(processed_data[, -4], processed_data$Target)
#' predictions <- model$predict(processed_data[, -4])
#' print(predictions)
#'
#' @keywords internal
#' @name mimosa
"_PACKAGE"
