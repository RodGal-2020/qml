# Debug script for quantum matrix classifier prediction issues
# Run this step by step to identify where the error occurs

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

# Load iris data
cat("Loading iris data...\n")
data(iris)

# Create filtered dataset (removing setosa to match your test)
cat("Creating filtered dataset...\n")
filtered_data <- iris %>% 
  filter(Species != "setosa") %>%
  mutate(Species = droplevels(Species))

cat("Filtered data info:\n")
cat("- Dimensions:", dim(filtered_data), "\n")
cat("- Classes:", levels(filtered_data$Species), "\n")
cat("- Class counts:", table(filtered_data$Species), "\n")

# Step 1: Fit the model (this works according to your test)
cat("\n=== STEP 1: FITTING MODEL ===\n")
model <- dm_fit(Species ~ ., data = filtered_data, n_breaks = 3)
cat("Model fitted successfully\n")
cat("Model classes:", model$classes, "\n")
cat("Number of classes:", model$n_classes, "\n")

# Step 2: Prepare newdata for prediction
cat("\n=== STEP 2: PREPARING NEWDATA ===\n")
set.seed(1974)
newdata <- filtered_data %>% sample_n(10)
cat("Newdata dimensions:", dim(newdata), "\n")
cat("Newdata classes:", levels(newdata$Species), "\n")

# Step 3: Add dummy target variable (as done in predict.dm_fit)
cat("\n=== STEP 3: ADDING DUMMY TARGET ===\n")
if (!model$objective_var %in% names(newdata)) {
  newdata[[model$objective_var]] <- factor(
    rep(model$classes[1], nrow(newdata)),
    levels = model$classes
  )
  cat("Added dummy target variable\n")
} else {
  cat("Target variable already exists in newdata\n")
}

# Step 4: Add prediction flag
cat("\n=== STEP 4: ADDING PREDICTION FLAG ===\n")
prediction_data <- newdata %>% dplyr::mutate(temp_pred_flag = TRUE)
cat("Added prediction flag, dimensions:", dim(prediction_data), "\n")

# Step 5: DEBUG THE QUANTUM MATRIX CREATION PROCESS
cat("\n=== STEP 5A: DEBUGGING get_rho_d PROCESS ===\n")

# Let's manually call get_rho_d to see what happens
cat("Calling get_rho_d manually...\n")
tryCatch({
  rho_d_result <- get_rho_d(
    Data = prediction_data,
    objective_var = model$objective_var,
    n_breaks = model$n_breaks,
    verbose = 2,
    test_var = "temp_pred_flag"
  )
  
  cat("get_rho_d completed successfully\n")
  cat("rho_d dimensions:", dim(rho_d_result$rho_d), "\n")
  cat("Training data classes:", length(rho_d_result$train_C), "\n")
  cat("Test data classes:", length(rho_d_result$test_C), "\n")
  
  # Check for empty classes
  for(i in seq_along(rho_d_result$train_C)) {
    cat("Train class", i, "dimensions:", dim(rho_d_result$train_C[[i]]), "\n")
  }
  for(i in seq_along(rho_d_result$test_C)) {
    cat("Test class", i, "dimensions:", dim(rho_d_result$test_C[[i]]), "\n")
  }
  
}, error = function(e) {
  cat("ERROR in get_rho_d:\n")
  cat("Error message:", conditionMessage(e), "\n")
  cat("Stack trace:\n")
  traceback()
  return(NULL)
})

# Step 5B: DEBUG THE SVD PROCESS 
if (exists("rho_d_result")) {
  cat("\n=== STEP 5B: DEBUGGING SVD PROCESS ===\n")
  
  cat("Examining rho_d matrix:\n")
  cat("- Dimensions:", dim(rho_d_result$rho_d), "\n")
  cat("- Is symmetric:", isSymmetric(rho_d_result$rho_d), "\n")
  cat("- Trace:", sum(diag(rho_d_result$rho_d)), "\n")
  cat("- Eigenvalues:", eigen(rho_d_result$rho_d)$values, "\n")
  
  tryCatch({
    svd_result <- compute_quantum_svd(rho_d_result$rho_d, zero_limit = 4)
    cat("SVD completed successfully\n")
    cat("- Rank:", svd_result$rank, "\n")
    cat("- U matrix dimensions:", dim(svd_result$U), "\n")
    cat("- Eigenvalues:", svd_result$eigenvalues, "\n")
    
  }, error = function(e) {
    cat("ERROR in compute_quantum_svd:\n")
    cat("Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Step 5C: DEBUG THE COORDINATE TRANSFORMATION
if (exists("rho_d_result") && exists("svd_result")) {
  cat("\n=== STEP 5C: DEBUGGING COORDINATE TRANSFORMATION ===\n")
  
  # Test coordinate transformation for training data
  cat("Testing coordinate transformation for training data...\n")
  tryCatch({
    train_coords <- transform_to_coordinates(
      rho_d_result$train_C,
      svd_result$U,
      svd_result$rank
    )
    cat("Training coordinate transformation successful\n")
    for(i in seq_along(train_coords)) {
      cat("Train coords class", i, "dimensions:", dim(train_coords[[i]]), "\n")
    }
    
  }, error = function(e) {
    cat("ERROR in training coordinate transformation:\n")
    cat("Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Test coordinate transformation for test data
  cat("Testing coordinate transformation for test data...\n")
  tryCatch({
    test_coords <- transform_to_coordinates(
      rho_d_result$test_C,
      svd_result$U,
      svd_result$rank
    )
    cat("Test coordinate transformation successful\n")
    for(i in seq_along(test_coords)) {
      cat("Test coords class", i, "dimensions:", dim(test_coords[[i]]), "\n")
    }
    
  }, error = function(e) {
    cat("ERROR in test coordinate transformation:\n")
    cat("Error message:", conditionMessage(e), "\n")
    cat("This is likely the source of the matrix multiplication error!\n")
    
    # Let's examine the problematic data
    cat("\nExamining test data structure:\n")
    for(i in seq_along(rho_d_result$test_C)) {
      test_class_data <- rho_d_result$test_C[[i]]
      cat("Test class", i, ":\n")
      cat("  Dimensions:", dim(test_class_data), "\n")
      cat("  Column names:", colnames(test_class_data), "\n")
      if(nrow(test_class_data) > 0) {
        cat("  First row:", as.numeric(test_class_data[1,]), "\n")
        cat("  Data types:", sapply(test_class_data, class), "\n")
      }
    }
    
    cat("\nExamining U matrix:\n")
    cat("  U dimensions:", dim(svd_result$U), "\n")
    cat("  U matrix:\n")
    print(svd_result$U)
    
    return(NULL)
  })
}

# Step 6: MANUAL MATRIX MULTIPLICATION TEST
if (exists("rho_d_result") && exists("svd_result")) {
  cat("\n=== STEP 6: MANUAL MATRIX MULTIPLICATION TEST ===\n")
  
  # Let's manually test the matrix multiplication that's failing
  for(i in seq_along(rho_d_result$test_C)) {
    test_class <- rho_d_result$test_C[[i]]
    if(nrow(test_class) > 0) {
      cat("Testing matrix multiplication for test class", i, ":\n")
      
      # Get first row for testing
      first_row <- test_class[1, , drop = FALSE]
      cat("  First row dimensions:", dim(first_row), "\n")
      cat("  U matrix dimensions:", dim(svd_result$U), "\n")
      
      # Try to normalize first
      tryCatch({
        normalized <- normalize_quantum_vector(first_row)
        cat("  Normalized vector dimensions:", dim(normalized), "\n")
        
        # Try matrix multiplication
        result <- normalized %*% svd_result$U
        cat("  Matrix multiplication successful!\n")
        cat("  Result dimensions:", dim(result), "\n")
        
      }, error = function(e) {
        cat("  ERROR in matrix multiplication:\n")
        cat("  Error message:", conditionMessage(e), "\n")
        cat("  This confirms the dimension mismatch issue!\n")
      })
    }
  }
}

cat("\n=== PROBLEM IDENTIFIED ===\n")
cat("The issue is clear: EMPTY TRAINING CLASSES!\n")
cat("- Train class 1 has 0 rows (should have training data)\n")
cat("- Train class 2 has 0 rows (should have training data)\n")
cat("- Test classes have data (this is correct for predictions)\n\n")

cat("ROOT CAUSE:\n")
cat("The prediction process is incorrectly trying to create a NEW quantum matrix\n")
cat("from the prediction data alone, rather than using the trained model.\n")
cat("This results in empty training classes and a degenerate 1x1 rho_d matrix.\n\n")

cat("THE FIX:\n")
cat("The predict.dm_fit function should use model$trained_data (the already\n")
cat("computed polar coordinates from training) instead of recomputing everything.\n\n")

# Let's show what the model actually contains
cat("=== EXAMINING THE TRAINED MODEL ===\n")
cat("The model contains pre-computed training data:\n")
for(i in seq_along(model$trained_data)) {
  cat("Model trained class", i, "dimensions:", dim(model$trained_data[[i]]), "\n")
  if(nrow(model$trained_data[[i]]) > 0) {
    cat("  Column names:", colnames(model$trained_data[[i]]), "\n")
    cat("  First few rows:\n")
    print(head(model$trained_data[[i]], 3))
  }
}

cat("\n=== CORRECT PREDICTION WORKFLOW ===\n")
cat("1. Use model$trained_data (already in polar coordinates)\n")
cat("2. Transform ONLY the new prediction data to polar coordinates\n")
cat("3. Use the same coordinate system (U matrix) from training\n")
cat("4. Apply classify_dm() with trained polar data\n\n")

cat("The current predict() function is broken because it calls get_C_tilde_polar()\n")
cat("on prediction data alone, creating a new (and empty) training set.\n")
