#' @title Data Preprocessing Functions for Quantum Matrix Classification
#' @description Functions for data preparation, validation, and integration with rsample
#' 
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import tibble
#' @importFrom rsample initial_split training testing

#' @title Prepare Split Data for Quantum Matrix Processing
#'
#' @description Converts rsample split objects into the format expected by quantum matrix functions.
#' This function bridges the gap between tidymodels rsample splits and the internal data structure
#' needed for quantum matrix classification.
#'
#' `r lifecycle::badge("stable")`
#'
#' @param split An rsample split object created with rsample::initial_split()
#' @param objective_var Character string naming the objective/target variable
#'
#' @return A data frame with training and test data combined, including a logical "test" column
#'
#' @examples
#' # Using rsample for stratified splitting
#' library(rsample)
#' split <- initial_split(iris, prop = 0.7, strata = Species)
#' prepared_data <- prepare_split_data(split, "Species")
#' table(prepared_data$test, prepared_data$Species)  # Check balance
#'
#' @export
prepare_split_data <- function(split, objective_var) {
  # Extract training and testing data from rsample split
  train_data <- rsample::training(split) %>%
    dplyr::mutate(test = FALSE)
  
  test_data <- rsample::testing(split) %>%
    dplyr::mutate(test = TRUE)
  
  # Combine and return
  dplyr::bind_rows(train_data, test_data) %>%
    dplyr::relocate(test, .after = dplyr::last_col())
}

#' @title Validate Input Data for Quantum Matrix Classification
#' 
#' @description Validates that the input data meets requirements for the quantum matrix classifier
#' 
#' @param data Input data frame or tibble
#' @param objective_var Name of the objective variable
#' @param n_breaks Number of breaks for discretization
#' 
#' @return TRUE if validation passes, otherwise throws an error
#' 
#' @keywords internal
validate_dm_input <- function(data, objective_var, n_breaks = 3) {
  # Check data exists
  if (missing(data) || is.null(data)) {
    stop("Input data is required")
  }
  
  # Check objective variable exists
  if (!objective_var %in% names(data)) {
    stop(paste("Objective variable", objective_var, "not found in data"))
  }
  
  # Check for minimum number of observations
  if (nrow(data) < 10) {
    warning("Very few observations (<10). Results may be unreliable.")
  }
  
  # Check n_breaks is reasonable
  if (n_breaks < 2) {
    stop("n_breaks must be at least 2")
  }
  
  # Check for missing values in objective variable
  if (any(is.na(data[[objective_var]]))) {
    stop("Missing values found in objective variable")
  }
  
  # Enforce binary classification (current package limitation)
  n_levels <- length(levels(as.factor(data[[objective_var]])))
  if (n_levels != 2) {
    stop("Binary classification required: objective variable must have exactly 2 classes")
  }

  # Check minimum observations per class (only for classes that actually appear in data)
  class_counts <- table(data[[objective_var]])
  active_classes <- class_counts[class_counts > 0]  # Only classes with observations
  
  if (length(active_classes) < 2) {
    stop("Dataset must contain at least 2 classes with observations")
  }
  
  if (any(active_classes < 2)) {
    stop("Each active class must have at least 2 observations")
  }
  
  return(TRUE)
}

#' @title Prepare Data for Quantum Matrix Processing
#' 
#' @description Converts and preprocesses data for quantum matrix operations
#' 
#' @param data Input data frame
#' @param objective_var Name of objective variable
#' @param n_breaks Number of breaks for continuous variables
#' @param test_var Name of test indicator variable
#' 
#' @return List with processed data components
#' 
#' @keywords internal
prepare_dm_data <- function(data, objective_var, n_breaks = 3, test_var = "test") {
  # Validate input
  validate_dm_input(data, objective_var, n_breaks)
  
  # Convert to tibble and ensure objective is factor
  data <- as_tibble(data)
  data[[objective_var]] <- as.factor(data[[objective_var]])
  
  # Store original data reference
  original_data <- data
  
  # Get number of classes
  n_classes <- length(levels(data[[objective_var]]))
  
  return(list(
    data = data,
    original_data = original_data,
    n_classes = n_classes,
    class_levels = levels(data[[objective_var]])
  ))
}
