#' Gym Members Exercise Tracking Dataset
#'
#' This dataset provides a detailed overview of gym members' exercise routines, physical attributes,
#' and fitness metrics. It includes demographic data, workout performance indicators, and physiological metrics
#' for 973 gym members. The dataset is suitable for studying exercise habits, modeling fitness progression,
#' and analyzing health trends.
#'
#' @format A data frame with 973 rows and 15 columns:
#' \describe{
#'   \item{Age}{Age of the gym member (numeric).}
#'   \item{Gender}{Gender of the gym member (\code{"Male"} or \code{"Female"}) (factor).}
#'   \item{Weight_kg}{Member’s weight in kilograms (numeric).}
#'   \item{Height_m}{Member’s height in meters (numeric).}
#'   \item{Max_BPM}{Maximum heart rate (beats per minute) during workout sessions (numeric).}
#'   \item{Avg_BPM}{Average heart rate during workout sessions (numeric).}
#'   \item{Resting_BPM}{Heart rate at rest before workout (numeric).}
#'   \item{Session_Duration_hours}{Duration of each workout session in hours (numeric).}
#'   \item{Calories_Burned}{Total calories burned during each session (numeric).}
#'   \item{Workout_Type}{Type of workout performed (\code{"Cardio"}, \code{"Strength"}, \code{"Yoga"}, \code{"HIIT"}) (factor).}
#'   \item{Fat_Percentage}{Body fat percentage of the member (numeric).}
#'   \item{Water_Intake_liters}{Daily water intake during workouts in liters (numeric).}
#'   \item{Workout_Frequency}{Number of workout sessions per week (numeric).}
#'   \item{Experience_Level}{Level of experience: \code{1} = Beginner, \code{2} = Intermediate, \code{3} = Expert (numeric).}
#'   \item{BMI}{Body Mass Index, calculated from height and weight (numeric).}
#' }
#'
#' @details
#' This dataset is ideal for data scientists, health researchers, and fitness enthusiasts interested in studying:
#' - Exercise habits and their impact on fitness metrics.
#' - Relationships between demographic and physiological data.
#' - Athlete progression and health trends.
#'
#' @source
#' Dataset sourced from Kaggle: \url{https://www.kaggle.com/datasets/valakhorasani/gym-members-exercise-dataset}
"gym_members_exercise_tracking"
