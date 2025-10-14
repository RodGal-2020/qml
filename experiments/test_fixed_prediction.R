# Test script to verify the fixed prediction function
# This should work without the matrix multiplication error

# Clear environment
rm(list = ls())

# Load libraries
library(dplyr)
library(magrittr) 
library(purrr)
library(tibble)
library(rlang)

# Load all source files
cat("Loading source files...\n")
source("R/dm_utils.R")
source("R/data_preprocessing.R")  
source("R/quantum_matrix.R")
source("R/kernel_functions.R")
source("R/coordinate_transforms.R")
source("R/dm_classifier.R")

# Load test data
data(iris)

# Create filtered dataset (binary classification as in original issue)
filtered_data <- iris %>% 
  filter(Species != "setosa") %>%
  mutate(Species = droplevels(Species))

cat("=== TESTING FIXED IMPLEMENTATION ===\n")
cat("Filtered data dimensions:", dim(filtered_data), "\n")
cat("Classes:", levels(filtered_data$Species), "\n")

# Test 1: Fit the model
cat("\n1. Testing model fitting...\n")
tryCatch({
  model <- dm_fit(Species ~ ., data = filtered_data, n_breaks = 3)
  cat("✓ Model fitted successfully\n")
  cat("Model classes:", model$classes, "\n")
  cat("Preprocessing info stored:", !is.null(model$preprocessing), "\n")
}, error = function(e) {
  cat("✗ Error in model fitting:\n")
  cat(conditionMessage(e), "\n")
  stop("Cannot continue without fitted model")
})

# Test 2: Make predictions on same data (should work)
cat("\n2. Testing predictions on training data...\n")
tryCatch({
  train_predictions <- predict(model, newdata = filtered_data[1:5, ])
  cat("✓ Training data predictions successful\n")
  cat("Predicted classes:", as.character(train_predictions), "\n")
}, error = function(e) {
  cat("✗ Error in training data predictions:\n")
  cat(conditionMessage(e), "\n")
})

# Test 3: Make predictions on new data (original failing case)
cat("\n3. Testing predictions on new data (original failing case)...\n")
tryCatch({
  new_predictions <- predict(model, newdata = iris[1:10, ])
  cat("✓ New data predictions successful\n")
  cat("Predicted classes:", as.character(new_predictions), "\n")
}, error = function(e) {
  cat("✗ Error in new data predictions:\n")
  cat(conditionMessage(e), "\n")
  cat("This indicates the fix didn't work completely\n")
})

# Test 4: Test probability predictions
cat("\n4. Testing probability predictions...\n")
tryCatch({
  prob_predictions <- predict(model, newdata = iris[1:5, ], type = "prob")
  cat("✓ Probability predictions successful\n")
  cat("Probability prediction structure:\n")
  print(prob_predictions)
}, error = function(e) {
  cat("✗ Error in probability predictions:\n")
  cat(conditionMessage(e), "\n")
})

# Test 5: Test edge cases
cat("\n5. Testing edge cases...\n")

# Empty data
tryCatch({
  empty_pred <- predict(model, newdata = filtered_data[integer(0), ])
  cat("✓ Empty data handled correctly\n")
}, error = function(e) {
  cat("✗ Error with empty data:\n")
  cat(conditionMessage(e), "\n")
})

# Single observation
tryCatch({
  single_pred <- predict(model, newdata = iris[1, ])
  cat("✓ Single observation handled correctly:", as.character(single_pred), "\n")
}, error = function(e) {
  cat("✗ Error with single observation:\n")
  cat(conditionMessage(e), "\n")
})

cat("\n=== TEST SUMMARY ===\n")
cat("If all tests show ✓, the fix is successful!\n")
cat("If any show ✗, there are still issues to resolve.\n")
