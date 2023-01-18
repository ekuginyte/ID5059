# Set working directory
setwd("~/Documents/GitHub/ID5059")

# Load libraries
library(utils)

# Read in data
wine <- read.csv("winequality-white.csv", sep = ";") 

# Check head
head(wine)

quality_categories = sort(c('low', 'normal', 'high'))
quality_to_category <- function(quality) {
  return (ifelse(quality < 6, 'low', ifelse(quality == 6, 'normal', 'high')))
}
classification_accuracy <- function(confusion_matrix, number_of_test_cases) {
  # Sum the elements on the diagonal, which are the correct classifications.
  
  correct_predictions <- 0
  for (j in 1 : nrow(confusion_matrix)) {
    correct_predictions <- correct_predictions + confusion_matrix[j, j]
  }
  return (sprintf("%1.f%%", (correct_predictions / number_of_test_cases) *␣ ↪100))
}

index_of_highest_value <- function(vector) {
  index = 1
  highest = vector[index]
  for (i in 2 : length(vector)) {
    if (vector[i] > highest) {
      index = i
      highest = vector[i]
    }
  }
  return (index)
}

highest_probability_label = function(probability_of_high, probability_of_low,␣ ↪probability_of_normal) {
  return (quality_categories[index_of_highest_value(c(probability_of_high,␣ ↪probability_of_low, probability_of_normal))])
}

scale_to_zero_one <- function(x) { (x - min(x)) / (max(x) - min(x)) }


