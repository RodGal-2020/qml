#' @title Coordinate Transformation Functions for Quantum Matrix Classification
#' @description Functions for transforming data to polar coordinates and other coordinate systems
#'
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import tibble
#' @import rlang
#' @importFrom fasano.franceschini.test fasano.franceschini.test

#' @title Get Preprocessing Parameters for Predictions
#'
#' @description Extracts and stores preprocessing parameters needed for making predictions.
#' This follows tidymodels pattern of separating fit and predict phases.
#'
#' @param Data Training data 
#' @param objective_var Name of objective variable
#' @param n_breaks Number of breaks for discretization
#' @param verbose Verbosity level
#' @param test_var Name of test indicator variable
#' @param zero_limit Precision limit for SVD operations
#'
#' @return List with preprocessing parameters for predictions
#'
#' @keywords internal
get_preprocessing_params <- function(
  Data,
  objective_var = NULL,
  n_breaks = 3,
  verbose = 0,
  test_var = "test",
  zero_limit = 4
) {
  # Get quantum matrix info from training data only
  rho_d_info <- get_rho_d(Data, objective_var, n_breaks, verbose, test_var)
  
  # Compute SVD transformation matrix
  svd_info <- compute_quantum_svd(rho_d_info$rho_d, zero_limit)
  
  # Store all necessary parameters for prediction
  list(
    U_matrix = svd_info$U,
    rank = svd_info$rank,
    rho_d = rho_d_info$rho_d,
    n_breaks = n_breaks,
    objective_var = objective_var,
    zero_limit = zero_limit,
    svd_eigenvalues = svd_info$eigenvalues
  )
}

#' @title Transform Data to Polar Coordinates
#'
#' @description Main workflow function that transforms quantum matrix data to polar coordinates:
#' 1. Creates quantum density matrix using get_rho_d()
#' 2. Performs SVD decomposition
#' 3. Projects data onto eigenvector basis
#' 4. Converts to polar coordinate system
#' 5. Validates distribution differences between classes
#'
#' @param Data Input data with test/train indicators
#' @param objective_var Name of objective variable
#' @param n_breaks Number of breaks for discretization
#' @param verbose Verbosity level (0 = silent, >2 = detailed output)
#' @param test_var Name of test indicator variable
#' @param zero_limit Precision limit for SVD operations
#'
#' @return List with training and test data in polar coordinates
#'
#' @export
get_C_tilde_polar <- function(
  Data,
  objective_var = NULL,
  n_breaks = 3,
  verbose = 0,
  test_var = "test",
  zero_limit = 4
) {
  # Step 1: Create quantum density matrix representation
  rho_d_info <- get_rho_d(Data, objective_var, n_breaks, verbose, test_var)

  # Step 2: Perform SVD decomposition for coordinate transformation
  svd_info <- compute_quantum_svd(rho_d_info$rho_d, zero_limit)

  if (verbose > 2) {
    cat("Eigenvector matrix U:\n")
    svd_info$U %>% round(2) %>% print()
  }

  # Step 3: Check rank constraint (currently limited to rank ≤ 2)
  if (svd_info$rank > 2) {
    stop(
      "Quantum matrix rank exceeds 2. Currently only binary classification is supported."
    )
  }

  # Step 4: Transform training and test data to new coordinate system
  train_coords <- transform_to_coordinates(
    rho_d_info$train_C,
    svd_info$U,
    svd_info$rank
  )
  test_coords <- transform_to_coordinates(
    rho_d_info$test_C,
    svd_info$U,
    svd_info$rank
  )

  if (verbose > 2) {
    cat("Training data in new coordinates:\n")
    print(train_coords)
  }

  # Step 5: Convert to polar coordinates
  train_polar <- convert_to_polar_coordinates(train_coords)
  test_polar <- convert_to_polar_coordinates(test_coords)

  if (verbose > 2) {
    cat("Training data in polar coordinates:\n")
    print(train_polar)
  }

  # Step 6: Validate distribution differences between classes
  dist_test <- test_distribution_differences(train_polar)

  if (!dist_test$significant) {
    warning(dist_test$message, call. = FALSE)
  }

  return(list(train = train_polar, test = test_polar))
}

#' @title Transform New Data for Prediction
#'
#' @description Transforms new prediction data using stored preprocessing parameters.
#' This follows tidymodels pattern - uses stored transformation without refitting.
#'
#' @param newdata New data to transform
#' @param preprocessing_info Stored preprocessing parameters from fitted model
#' @param objective_var Name of objective variable
#'
#' @return Data transformed to polar coordinates for prediction
#'
#' @keywords internal
transform_newdata_for_prediction <- function(newdata, preprocessing_info, objective_var) {
  # Add dummy objective variable if missing (standard tidymodels pattern)
  if (!objective_var %in% names(newdata)) {
    # Use first class as dummy (will be ignored in coordinate transformation)
    newdata[[objective_var]] <- factor(rep("dummy", nrow(newdata)))
  }
  
  # Ensure objective is factor with proper levels (prevent droplevels issues)
  newdata[[objective_var]] <- as.factor(newdata[[objective_var]])
  
  # Apply same preprocessing as training data
  # Step 1: Discretize continuous features
  processed_data <- discretize_continuous_features(newdata, preprocessing_info$n_breaks)
  
  # Step 2: Add prediction flag (all observations are for prediction)
  processed_data <- processed_data %>% dplyr::mutate(temp_pred_flag = TRUE)
  
  # Step 3: Encode logical variables
  processed_data <- encode_logical_variables(processed_data, "temp_pred_flag")
  
  # Step 4: Encode categorical variables  
  processed_data <- encode_categorical_variables(processed_data, objective_var, "temp_pred_flag")
  
  # Step 5: Encode target variable (dummy encoding)
  processed_data <- encode_target_variable(processed_data, objective_var)
  
  # Step 6: Create quantum matrix format
  processed_data <- create_quantum_matrix_format(processed_data, "temp_pred_flag")
  
  # Step 7: Extract prediction data (all marked as test since temp_pred_flag = TRUE)
  n_classes <- length(unique(processed_data[[paste0(objective_var, "_class_1")]]))
  if (any(grepl(paste0(objective_var, "_class_2"), names(processed_data)))) {
    n_classes <- sum(grepl(paste0(objective_var, "_class_"), names(processed_data)))
  }
  
  # Separate by class (even though it's artificial for prediction)
  class_data <- separate_data_by_class(processed_data, objective_var, n_classes)
  
  # Get test data (since temp_pred_flag = TRUE, all data will be "test")
  test_data <- class_data %>%
    purrr::map(
      ~ .x %>%
        dplyr::filter(temp_pred_flag == TRUE) %>%
        dplyr::select(-temp_pred_flag)
    )
  
  # Transform using STORED U matrix (key tidymodels principle)
  transformed_coords <- transform_to_coordinates(
    test_data,
    preprocessing_info$U_matrix,
    preprocessing_info$rank
  )
  
  # Convert to polar coordinates
  polar_coords <- convert_to_polar_coordinates(transformed_coords)
  
  return(polar_coords)
}

#' @title Transform Data to Coordinate System
#'
#' @description Applies coordinate transformation using SVD eigenvectors
#'
#' @param data_list List of class-specific data
#' @param U Matrix of eigenvectors
#' @param rank_rho Rank of the quantum matrix
#'
#' @return List of transformed data
#'
#' @keywords internal
transform_to_coordinates <- function(data_list, U, rank_rho) {
  data_list %>%
    purrr::map(
      ~ {
        # Handle empty data frames
        if (nrow(.x) == 0 || ncol(.x) == 0) {
          result_matrix <- matrix(nrow = 0, ncol = rank_rho)
          colnames(result_matrix) <- paste0("Coord_", seq_len(rank_rho))
          return(result_matrix)
        }

        # Initialize result matrix with proper column names
        xp <- matrix(NA, nrow = 0, ncol = rank_rho)
        colnames(xp) <- paste0("Coord_", seq_len(rank_rho))

        # Process each row
        for (r in seq_len(nrow(.x))) {
          # Extract row and handle potential issues
          row_data <- .x[r, , drop = FALSE]

          # Skip if row contains only NA values
          if (all(is.na(row_data))) {
            next
          }

          # Normalize and transform
          x_tilde <- normalize_quantum_vector(row_data)
          u_coords <- get_u_coordinates(x_tilde, U)

          # Ensure u_coords has proper dimensions and names
          if (is.vector(u_coords)) {
            u_coords <- matrix(u_coords, nrow = 1)
          }
          if (ncol(u_coords) != rank_rho) {
            warning("Coordinate dimension mismatch", call. = FALSE)
            next
          }

          xp <- rbind(xp, u_coords)
        }

        # Ensure final matrix has proper column names
        colnames(xp) <- paste0("Coord_", seq_len(rank_rho))
        return(xp)
      }
    )
}

#' @title Normalize Quantum Vector
#'
#' @description Normalizes a quantum state vector to unit length
#'
#' @param X Input vector (single row matrix)
#'
#' @return Normalized vector with unit norm
#'
#' @keywords internal
normalize_quantum_vector <- function(X) {
  X_matrix <- X %>% as.matrix()

  # Handle empty or invalid input
  if (nrow(X_matrix) == 0 || ncol(X_matrix) == 0) {
    warning("Empty input matrix, returning as is", call. = FALSE)
    return(X_matrix)
  }

  # Compute squared norm (inner product with transpose)
  norm_squared <- X_matrix %*% t(X_matrix) %>% diag() %>% magrittr::extract(1)

  # Handle NA, NaN, or near-zero vectors
  if (
    is.na(norm_squared) || is.nan(norm_squared) || abs(norm_squared) < 1e-10
  ) {
    warning(
      "Invalid or zero vector norm, returning original vector",
      call. = FALSE
    )
    return(X_matrix)
  }

  # Return normalized vector
  X_matrix / sqrt(norm_squared)
}

#' @title Get Coordinates in Eigenvector Basis
#'
#' @description Projects normalized vector onto eigenvector basis from SVD
#'
#' @param X_tilde Normalized quantum state vector (row matrix)
#' @param U Matrix of eigenvectors from SVD (columns = eigenvectors)
#'
#' @return Coordinates in the eigenvector basis
#'
#' @keywords internal
get_u_coordinates <- function(X_tilde, U) {
  X_tilde %*% U
}

#' @title Convert to Polar Coordinates
#'
#' @description Converts Cartesian coordinates to polar coordinates
#'
#' @param data_list List of coordinate data
#'
#' @return List of data in polar coordinates
#'
#' @keywords internal
convert_to_polar_coordinates <- function(data_list) {
  data_list %>%
    purrr::map(
      ~ {
        # Handle empty matrices
        if (nrow(.x) == 0 || ncol(.x) == 0) {
          return(tibble::tibble(r = numeric(0), phi_1 = numeric(0)))
        }

        # Ensure the matrix has proper column names before converting to tibble
        if (is.null(colnames(.x))) {
          colnames(.x) <- paste0("V_", seq_len(ncol(.x)))
        }

        .x %>%
          tibble::as_tibble() %>%
          magrittr::set_names(paste0("V_", seq_len(ncol(.)))) %>% # Ensure consistent naming
          dplyr::mutate(
            r = sqrt(V_1^2 + V_2^2),
            phi_1 = atan(V_2 / V_1)
          ) %>%
          dplyr::select(r, phi_1)
      }
    )
}

#' @title Test for Distribution Differences
#'
#' @description Uses Fasano-Franceschini test to check if class distributions differ
#'
#' @param polar_data List of polar coordinate data by class
#'
#' @return List with test results
#'
#' @keywords internal
test_distribution_differences <- function(polar_data) {
  if (length(polar_data) < 2) {
    return(list(
      significant = TRUE,
      message = "Only one class present"
    ))
  }

  # Test first two classes (binary classification assumption)
  ff.test <- fasano.franceschini.test::fasano.franceschini.test(
    polar_data[[1]],
    polar_data[[2]]
  )

  if (ff.test$p.value < 0.05) {
    return(list(
      significant = TRUE,
      p_value = ff.test$p.value,
      message = paste(
        "Distributions are significantly different (p =",
        round(ff.test$p.value, 4),
        ")"
      )
    ))
  } else {
    return(list(
      significant = FALSE,
      p_value = ff.test$p.value,
      message = paste(
        "No hay evidencia significativa de diferencia entre las distribuciones (p =",
        round(ff.test$p.value, 4),
        "). Posiblemente nada funcione"
      )
    ))
  }
}
