#' Depression Dataset
#'
#' This dataset contains information about individuals and various factors potentially associated with depression.
#' It includes demographic data, academic and work pressures, lifestyle habits, and mental health indicators.
#'
#' @format A data frame with 27,901 rows and 18 columns:
#' \describe{
#'   \item{id}{Unique identifier for each individual (integer).}
#'   \item{Gender}{Gender of the individual (character).}
#'   \item{Age}{Age of the individual in years (numeric). Ranges from 18 to 59.}
#'   \item{City}{City of residence (character).}
#'   \item{Profession}{Profession of the individual (character).}
#'   \item{Academic.Pressure}{Level of academic pressure experienced (numeric, Likert scale from 0 to 5).}
#'   \item{Work.Pressure}{Level of work pressure experienced (numeric, Likert scale from 0 to 5).}
#'   \item{CGPA}{Cumulative Grade Point Average (numeric, ranges from 0 to 10).}
#'   \item{Study.Satisfaction}{Satisfaction with study life (numeric, Likert scale from 0 to 5).}
#'   \item{Job.Satisfaction}{Satisfaction with job life (numeric, Likert scale from 0 to 4).}
#'   \item{Sleep.Duration}{Average sleep duration in hours per day (character).}
#'   \item{Dietary.Habits}{Description of dietary habits (character).}
#'   \item{Degree}{Highest degree achieved (character).}
#'   \item{Have.you.ever.had.suicidal.thoughts..}{Whether the individual has experienced suicidal thoughts (\code{"Yes"} or \code{"No"}) (character).}
#'   \item{Work.Study.Hours}{Total hours spent on work and study per day (numeric, ranges from 0 to 12).}
#'   \item{Financial.Stress}{Level of financial stress experienced (numeric, Likert scale from 1 to 5). May contain \code{NA} values.}
#'   \item{Family.History.of.Mental.Illness}{Whether the individual has a family history of mental illness (\code{"Yes"} or \code{"No"}) (character).}
#'   \item{Depression}{Depression status of the individual (\code{0} = no depression, \code{1} = depression) (numeric).}
#' }
#'
#' @details
#' This dataset is used to analyze the prevalence and factors associated with depression, including lifestyle,
#' academic, and work-related pressures. It also includes family history and other indicators of mental health.
#'
#' @source
#' Simulated dataset for educational and illustrative purposes.
"depression"
