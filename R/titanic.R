#' Titanic Dataset
#'
#' This dataset contains information about passengers aboard the Titanic. It includes demographic
#' and socio-economic data as well as survival status. It is commonly used for classification
#' tasks in machine learning.
#'
#' @format A data frame with X rows and Y columns (replace with actual dimensions):
#' \describe{
#'   \item{PassengerId}{Unique identifier for each passenger (integer).}
#'   \item{Survived}{Survival status: \code{0} = did not survive, \code{1} = survived (integer).}
#'   \item{Pclass}{Passenger class: \code{1} = 1st, \code{2} = 2nd, \code{3} = 3rd (integer).}
#'   \item{Name}{Full name of the passenger (character).}
#'   \item{Sex}{Gender of the passenger (\code{"male"} or \code{"female"}) (factor).}
#'   \item{Age}{Age of the passenger in years (numeric). May contain \code{NA} for missing values.}
#'   \item{SibSp}{Number of siblings or spouses aboard (integer).}
#'   \item{Parch}{Number of parents or children aboard (integer).}
#'   \item{Ticket}{Ticket number (character).}
#'   \item{Fare}{Fare paid for the ticket in pounds (numeric).}
#'   \item{Cabin}{Cabin number (character). May contain \code{NA} for missing values.}
#'   \item{Embarked}{Port of embarkation: \code{"C"} = Cherbourg, \code{"Q"} = Queenstown,
#'   \code{"S"} = Southampton (factor).}
#' }
#'
#' @details
#' The Titanic dataset is widely used for binary classification problems, where the goal
#' is to predict the survival status of passengers based on their demographic and socio-economic data.
#'
#' @source
#' This dataset is available from Kaggle: \url{https://www.kaggle.com/c/titanic}.
"titanic"
