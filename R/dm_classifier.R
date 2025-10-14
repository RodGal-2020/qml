#' @title Quantum Matrix Classifier - Main Interface
#' @description Core functions for quantum machine learning classification
#'
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import tibble

# Internal function: Classify a single observation
classify_dm <- function(x, D_tilde) {
  class_probs <- seq_along(D_tilde) %>%
    purrr::map_dbl(~ f_hat_h(D_tilde, x, clase = .x))

  if (all(class_probs == 0)) {
    warning(
      "Zero probabilities detected, using uniform distribution",
      call. = FALSE
    )
    class_probs <- rep(1 / length(D_tilde), length(D_tilde))
  } else {
    class_probs <- class_probs / sum(class_probs)
  }

  list(class = which.max(class_probs), probs = class_probs)
}

#' @title Quantum Matrix Method for Classification - Model Fitting
#'
#' @description Fits a quantum matrix classifier using the DM model.
#' Returns a fitted model object that can be used with predict().
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param formula A formula specifying the model (e.g., y ~ .)
#' @param data A data frame containing the training data
#' @param n_breaks Number of breaks for discretizing continuous variables
#' @param verbose Verbosity level (0 = silent, 1+ = more verbose)
#' @param ... Additional parameters
#'
#' @return An object of class "dm_fit" containing the fitted model
#'
#' @examples
#' # Fit a quantum matrix classifier
#' model <- dm_fit(Species ~ ., data = iris)
#' predictions <- predict(model, iris)
#'
#' @export
dm_fit <- function(formula, data, n_breaks = 3, verbose = 0, ...) {
  # Validate inputs
  check_dependencies()
  if (missing(formula)) {
    stop(format_error("formula is required"))
  }

  # Extract target variable from formula
  objective_var <- all.vars(formula)[1]
  validate_dm_input(data, objective_var, n_breaks)

  # Prepare data: convert target to factor and add training flag
  training_data <- data %>%
    dplyr::mutate(
      !!rlang::sym(objective_var) := as.factor(!!rlang::sym(objective_var)) %>%
        droplevels(),
      temp_fit_flag = FALSE
    )

  # Transform to polar coordinates (core quantum matrix workflow)
  polar_data <- get_C_tilde_polar(
    training_data,
    objective_var,
    n_breaks,
    verbose,
    "temp_fit_flag"
  )

  # Store preprocessing information for prediction phase (tidymodels pattern)
  # We need to capture the SVD transformation and preprocessing parameters
  preprocessing_info <- get_preprocessing_params(
    training_data,
    objective_var,
    n_breaks,
    verbose,
    "temp_fit_flag"
  )

  # Build and return fitted model object
  list(
    formula = formula,
    trained_data = polar_data$train,
    objective_var = objective_var,
    classes = levels(training_data[[objective_var]]),
    n_classes = length(levels(training_data[[objective_var]])),
    n_breaks = n_breaks,
    # Store preprocessing info for predictions (tidymodels pattern)
    preprocessing = preprocessing_info,
    call = match.call(),
    training_summary = list(n_obs = nrow(data), n_features = ncol(data) - 1),
    model_summary = create_model_summary(
      polar_data$train,
      list(n_breaks = n_breaks, verbose = verbose, formula = formula)
    ),
    fit_time = Sys.time()
  ) %>%
    magrittr::set_class("dm_fit")
}

#' @title Predict method for quantum matrix classifier
#'
#' @description Make predictions using a fitted quantum matrix classifier.
#' Follows tidymodels conventions: uses stored model parameters without refitting.
#'
#' @param object A fitted model object of class "dm_fit"
#' @param newdata A data frame containing new observations to predict
#' @param type Type of prediction: "class" for predicted classes, "prob" for class probabilities
#' @param return_coords Logical. If TRUE, returns polar coordinates (r, phi) along with predictions
#' @param ... Additional arguments (currently unused)
#'
#' @return For type="class": factor vector of predicted classes.
#'         For type="prob": tibble with class probabilities (columns = classes).
#'         If return_coords=TRUE: list with predictions and polar coordinates
#'
#' @examples
#' model <- dm_fit(Species ~ ., data = iris)
#' predict(model, iris)
#' predict(model, iris, type = "prob")
#' # Get predictions with polar coordinates for visualization
#' result <- predict(model, iris, return_coords = TRUE)
#' result$predictions  # Standard predictions
#' result$coords       # Polar coordinates (r, phi_1)
#'
#' @export
predict.dm_fit <- function(object, newdata, type = c("class", "prob"), return_coords = FALSE, ...) {
  type <- match.arg(type)
  
  # Validate inputs (tidymodels pattern)
  if (missing(newdata)) {
    stop(format_error("newdata is required"))
  }
  if (nrow(newdata) == 0) {
    return(empty_prediction_result(type, object$classes))
  }

  # Transform newdata using STORED preprocessing parameters (key tidymodels principle)
  tryCatch({
    transformed_newdata <- transform_newdata_for_prediction(
      newdata, 
      object$preprocessing, 
      object$objective_var
    )
  }, error = function(e) {
    stop(format_error(
      "Failed to transform new data using stored preprocessing parameters",
      context = conditionMessage(e),
      suggestion = "Check that newdata has the same structure as training data"
    ))
  })

  # Generate predictions for each observation
  n_obs <- nrow(newdata)
  predictions <- vector("list", n_obs)
  
  # Collect all observations across classes (since data might be split artificially)
  all_observations <- tibble::tibble()
  for (class_idx in seq_along(transformed_newdata)) {
    class_data <- transformed_newdata[[class_idx]]
    if (nrow(class_data) > 0) {
      class_data$original_row_id <- seq_len(nrow(class_data))
      all_observations <- dplyr::bind_rows(all_observations, class_data)
    }
  }
  
  # Make predictions using stored trained data (tidymodels pattern)
  if (nrow(all_observations) > 0) {
    prediction_results <- all_observations %>%
      dplyr::mutate(
        pred_result = purrr::pmap(
          dplyr::select(., r, phi_1),
          ~ classify_dm(c(.x, .y), object$trained_data)
        ),
        .pred_class = purrr::map_dbl(pred_result, ~ .x$class),
        .pred_probs = purrr::map(pred_result, ~ .x$probs)
      )
    
    # Store polar coordinates for optional return
    polar_coords <- all_observations %>%
      dplyr::select(r, phi_1)
    
  } else {
    return(empty_prediction_result(type, object$classes, return_coords))
  }

  # Format predictions according to type
  if (type == "class") {
    predictions <- factor(
      prediction_results$.pred_class,
      levels = seq_along(object$classes),
      labels = object$classes
    )
  } else {
    # Return tibble with standardized column names (tidymodels pattern)
    prob_matrix <- prediction_results$.pred_probs %>%
      do.call(rbind, .)
    
    predictions <- tibble::as_tibble(prob_matrix)
    colnames(predictions) <- paste0(".pred_", object$classes)
  }
  
  # Return coordinates along with predictions if requested
  if (return_coords) {
    return(list(
      predictions = predictions,
      coords = polar_coords
    ))
  } else {
    return(predictions)
  }
}

#' @title Create Empty Prediction Result
#' @param type Prediction type
#' @param classes Model classes
#' @param return_coords Whether to include coordinates
#' @return Empty result in correct format
#' @keywords internal
empty_prediction_result <- function(type, classes, return_coords = FALSE) {
  if (type == "class") {
    predictions <- factor(character(0), levels = classes)
  } else {
    predictions <- tibble::tibble()
    for (class_name in classes) {
      predictions[[paste0(".pred_", class_name)]] <- numeric(0)
    }
  }
  
  if (return_coords) {
    return(list(
      predictions = predictions,
      coords = tibble::tibble(r = numeric(0), phi_1 = numeric(0))
    ))
  } else {
    return(predictions)
  }
}

#' @title Print method for quantum matrix classifier
#'
#' @description Print a summary of the fitted quantum matrix classifier
#'
#' @param x A fitted model object of class "dm_fit"
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.dm_fit <- function(x, ...) {
  cat("Quantum Matrix Classifier (dm_fit)\n")
  cat("===================================\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Formula:", deparse(x$formula), "\n")
  cat("Number of observations:", x$training_summary$n_obs, "\n")
  cat("Number of features:", x$training_summary$n_features, "\n")
  cat("Number of classes:", x$n_classes, "\n")
  cat("Classes:", paste(x$classes, collapse = ", "), "\n")
  cat("Number of breaks:", x$n_breaks, "\n")
  cat("Fitted on:", format(x$fit_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

  cat("Training data summary:\n")
  for (i in seq_along(x$trained_data)) {
    cat(
      "  Class",
      i,
      "(",
      x$classes[i],
      "):",
      nrow(x$trained_data[[i]]),
      "observations\n"
    )
  }

  cat("\nUse predict() to make predictions on new data.\n")
  cat("Use summary() for more detailed information.\n")
}

#' @title Summary method for quantum matrix classifier
#'
#' @description Provide a detailed summary of the fitted quantum matrix classifier
#'
#' @param object A fitted model object of class "dm_fit"
#' @param ... Additional arguments (currently unused)
#'
#' @export
summary.dm_fit <- function(object, ...) {
  cat("Quantum Matrix Classifier Summary\n")
  cat("=================================\n\n")

  print(object)

  cat("\nDetailed training data statistics:\n")
  for (i in seq_along(object$trained_data)) {
    class_data <- object$trained_data[[i]]
    cat("\nClass", i, "(", object$classes[i], "):\n")
    cat("  Observations:", nrow(class_data), "\n")
    if (nrow(class_data) > 0) {
      cat(
        "  r statistics   -> Mean:",
        round(mean(class_data$r), 4),
        "SD:",
        round(sd(class_data$r), 4),
        "\n"
      )
      cat(
        "  phi_1 statistics -> Mean:",
        round(mean(class_data$phi_1, na.rm = TRUE), 4),
        "SD:",
        round(sd(class_data$phi_1, na.rm = TRUE), 4),
        "\n"
      )
    }
  }

  cat("\nModel Configuration:\n")
  cat("  Number of breaks:", object$n_breaks, "\n")
  cat(
    "  Processing completed at:",
    format(object$fit_time, "%Y-%m-%d %H:%M:%S"),
    "\n"
  )
}

# Legacy function for backward compatibility - DEPRECATED
#' @title Legacy Quantum Matrix Classification Function
#' @description This function is deprecated. Use dm_fit() and predict() instead.
#' @param ... All parameters (ignored)
#' @return Error message directing to new interface
#' @keywords internal
dm <- function(...) {
  stop(
    "dm() is deprecated and removed. Use the modern interface:\n",
    "  model <- dm_fit(formula, data, n_breaks, verbose)\n",
    "  predictions <- predict(model, newdata, type)\n",
    "See ?dm_fit for more information.",
    call. = FALSE
  )
}
