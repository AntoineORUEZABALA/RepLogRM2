% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{imputer_categorielles}
\alias{imputer_categorielles}
\title{Impute Missing Values for Categorical Variables}
\usage{
imputer_categorielles(data, methode = "mode")
}
\arguments{
\item{data}{A dataframe containing the dataset to process.}

\item{methode}{A character string specifying the imputation method.
Currently, only "mode" is supported. Default is "mode".}
}
\value{
A dataframe with missing categorical values imputed.
}
\description{
Impute Missing Values for Categorical Variables
}
