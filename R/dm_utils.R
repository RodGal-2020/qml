#' @title Utility Functions for Quantum Matrix Classification
#' @description Common helper functions, constants, and configurations
#' 
#' @import dplyr
#' @import magrittr

#' @title Default Configuration for Quantum Matrix Classifier
#' @export
DM_DEFAULTS <- list(
  n_breaks = 3, verbose = 0, test_var = "test", zero_limit = 4,
  h_window = 0.1, bandwidth = 0.1, min_observations_per_class = 2,
  eigenvalue_tolerance = 1e-15, trace_tolerance = 1e-10
)

#' @title Check Package Dependencies
#' @return TRUE if all dependencies are available, otherwise throws error
#' @keywords internal
check_dependencies <- function() {
  required_packages <- c("dplyr", "magrittr", "purrr", "tibble", "fasano.franceschini.test")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not available:", paste(missing_packages, collapse = ", ")))
  }
  return(TRUE)
}

#' @title Format Error Messages
#' @param message Main error message
#' @param context Additional context (optional)
#' @param suggestion Suggested solution (optional)
#' @return Formatted error message
#' @keywords internal
format_error <- function(message, context = NULL, suggestion = NULL) {
  formatted_msg <- paste("Quantum Matrix Classifier Error:", message)
  if (!is.null(context)) formatted_msg <- paste(formatted_msg, "\nContext:", context)
  if (!is.null(suggestion)) formatted_msg <- paste(formatted_msg, "\nSuggestion:", suggestion)
  return(formatted_msg)
}

#' @title Format Warning Messages
#' @param message Main warning message
#' @param context Additional context (optional)
#' @return Formatted warning message
#' @keywords internal
format_warning <- function(message, context = NULL) {
  formatted_msg <- paste("Quantum Matrix Classifier Warning:", message)
  if (!is.null(context)) formatted_msg <- paste(formatted_msg, "\nContext:", context)
  return(formatted_msg)
}

#' @title Safe Division
#' @param numerator Numerator value
#' @param denominator Denominator value
#' @param replacement Value to return if denominator is zero
#' @return Result of division or replacement value
#' @keywords internal
safe_divide <- function(numerator, denominator, replacement = 0) {
  if (abs(denominator) < DM_DEFAULTS$eigenvalue_tolerance) return(replacement)
  return(numerator / denominator)
}

#' @title Round to Significant Digits
#' @param x Numeric value or vector
#' @param digits Number of significant digits
#' @return Rounded value
#' @keywords internal
round_precision <- function(x, digits = DM_DEFAULTS$zero_limit) round(x, digits)

#' @title Check Numeric Tolerance
#' @param x Numeric value to check
#' @param tolerance Tolerance level
#' @return Logical indicating if value is within tolerance of zero
#' @keywords internal
is_effectively_zero <- function(x, tolerance = DM_DEFAULTS$eigenvalue_tolerance) abs(x) < tolerance

#' @title Validate Probability Vector
#' @param probs Numeric vector of probabilities
#' @param tolerance Tolerance for sum being 1
#' @return TRUE if valid, otherwise throws error
#' @keywords internal
validate_probabilities <- function(probs, tolerance = DM_DEFAULTS$trace_tolerance) {
  if (any(probs < 0)) stop(format_error("Probabilities cannot be negative"))
  if (abs(sum(probs) - 1) > tolerance) {
    warning(format_warning(paste("Probabilities do not sum to 1 (sum =", round(sum(probs), 6), ")")))
  }
  return(TRUE)
}

#' @title Create Model Summary Statistics
#' @param data Training data by class
#' @param model_params Model parameters
#' @return List of summary statistics
#' @keywords internal
create_model_summary <- function(data, model_params) {
  class_stats <- data %>%
    purrr::map(~ {
      if (nrow(.x) == 0) return(list(n_obs = 0, r_mean = NA, r_sd = NA, phi_mean = NA, phi_sd = NA))
      list(n_obs = nrow(.x), r_mean = mean(.x$r, na.rm = TRUE), r_sd = sd(.x$r, na.rm = TRUE),
           phi_mean = mean(.x$phi_1, na.rm = TRUE), phi_sd = sd(.x$phi_1, na.rm = TRUE))
    })
  
  list(class_statistics = class_stats, total_observations = sum(purrr::map_int(data, nrow)),
       n_classes = length(data), model_parameters = model_params)
}
