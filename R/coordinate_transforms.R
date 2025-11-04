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
    zero_limit = 4,
    bandwidth = DM_DEFAULTS$bandwidth
) {
    # Compute and store preprocessing artifacts (breaks and factor levels)
    Data <- tibble::as_tibble(Data)
    Data[[objective_var]] <- as.factor(Data[[objective_var]])
    class_levels <- levels(Data[[objective_var]])
    n_classes <- length(class_levels)

    # Determine explicit breaks per numeric column and levels per factor
    breaks_list <- compute_breaks_list(Data, n_breaks)
    cat_levels <- purrr::imap(Data, function(col, nm) {
      if (is.factor(col) && nm != objective_var && nm != test_var) levels(col) else NULL
    })

    # Build rho_d using explicit artifacts to ensure consistency
    rho_d_info <- get_rho_d(
      Data %>% dplyr::mutate(!!rlang::sym(test_var) := FALSE),
      objective_var, n_breaks, verbose, test_var,
      breaks_list = breaks_list, cat_levels = cat_levels
    )
  
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
      svd_eigenvalues = svd_info$eigenvalues,
      breaks_list = breaks_list,
      cat_levels = cat_levels,
      class_levels = class_levels,
      n_classes = n_classes,
      test_var = test_var,
      bandwidth = bandwidth
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
    warning("Rank > 2 detected. Projecting onto first 2 components.")
  }
  rank_used <- min(2, svd_info$rank)
  U_used <- svd_info$U[, 1:rank_used, drop = FALSE]

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
#' @param use_polar Logical; if TRUE (default) return polar coords (r, phi_1). If FALSE, return Cartesian coords (Coord_1, Coord_2)
#'
#' @return Data transformed to polar coordinates for prediction
#'
#' @keywords internal
transform_newdata_for_prediction <- function(newdata, preprocessing_info, objective_var, use_polar = TRUE) {
  # Add dummy objective variable if missing (standard tidymodels pattern)
  if (!objective_var %in% names(newdata)) {
    newdata[[objective_var]] <- factor(rep(preprocessing_info$class_levels[1], nrow(newdata)),
                                       levels = preprocessing_info$class_levels)
  }

  # Ensure objective is factor with proper levels (prevent droplevels issues)
  newdata[[objective_var]] <- factor(newdata[[objective_var]], levels = preprocessing_info$class_levels)

  # Apply the SAME preprocessing artifacts as training time
  # Step 1: Discretize continuous features using stored breaks
  processed_data <- discretize_continuous_features(newdata, preprocessing_info$n_breaks, preprocessing_info$breaks_list)

  # Step 2: Add prediction flag and row id (preserve original order)
  processed_data <- processed_data %>%
    dplyr::mutate(temp_pred_flag = TRUE, .row_id = dplyr::row_number())

  # Step 3: Encode logical vars (preserve temp_pred_flag and .row_id)
  preserved_cols <- c("temp_pred_flag", ".row_id")
  processed_data <- encode_logical_variables(processed_data, preserved_cols)

  # Step 4: Encode categoricals with stored levels (preserve temp_pred_flag and .row_id)
  processed_data <- encode_categorical_variables(processed_data, objective_var, preserved_cols, levels_list = preprocessing_info$cat_levels)

  # Step 5: Encode target (dummy)
  processed_data <- encode_target_variable(processed_data, objective_var)

  # Step 6: Create quantum matrix format (preserve both preserved_cols)
  processed_data <- create_quantum_matrix_format(processed_data, preserved_cols)

  # Step 7: Separate by class using training-time number of classes
  n_classes <- preprocessing_info$n_classes
  class_data <- separate_data_by_class(processed_data, objective_var, n_classes)

  # Capture row ids for final reassembly in original order
  row_id_split <- class_data %>%
    purrr::map(~ .x %>% dplyr::filter(temp_pred_flag == TRUE) %>% dplyr::pull(.row_id))

  # Extract prediction matrices per class (drop flags and ids)
  test_data <- class_data %>%
    purrr::map(~ .x %>%
                 dplyr::filter(temp_pred_flag == TRUE) %>%
                 dplyr::select(-temp_pred_flag, -.row_id))

  # Step 8: Transform using stored U matrix
  transformed_coords <- transform_to_coordinates(
    test_data,
    preprocessing_info$U_matrix,
    preprocessing_info$rank
  )

  # Step 9: Return in the requested coordinate system
  rank_used <- min(2, preprocessing_info$rank)
  if (use_polar) {
    polar_coords <- convert_to_polar_coordinates(transformed_coords)
    polar_with_id <- purrr::map2(polar_coords, row_id_split, ~ dplyr::mutate(.x, row_id = .y))
    combined <- dplyr::bind_rows(polar_with_id) %>% dplyr::arrange(row_id) %>% dplyr::select(r, phi_1)
    return(combined)
  } else {
    # Build Cartesian coordinate tibble (first 2 coordinates)
    cart_with_id <- purrr::map2(
      transformed_coords,
      row_id_split,
      ~ {
        mat <- .x
        if (ncol(mat) < rank_used) {
          # pad with NAs if needed
          mat <- cbind(mat, matrix(NA_real_, nrow = nrow(mat), ncol = rank_used - ncol(mat)))
        }
        tibble::as_tibble(mat[, seq_len(rank_used), drop = FALSE]) %>%
          magrittr::set_names(paste0("Coord_", seq_len(rank_used))) %>%
          dplyr::mutate(row_id = .y)
      }
    )
    combined <- dplyr::bind_rows(cart_with_id) %>% dplyr::arrange(row_id) %>% dplyr::select(dplyr::all_of(paste0("Coord_", seq_len(rank_used))))
    # Ensure two coordinates exist (pad with zeros if necessary)
    if (rank_used < 2) {
      if (!"Coord_1" %in% names(combined)) combined$Coord_1 <- 0
      if (!"Coord_2" %in% names(combined)) combined$Coord_2 <- 0
      combined <- combined %>% dplyr::select(Coord_1, Coord_2)
    }
    return(combined)
  }
}

#' @title Transform Data to Coordinate System
#'
#' @description Projects each observation onto the SVD eigenvector basis and
#' returns the first rank_rho coordinates.
#'
#' @param data_list List of class-specific numeric matrices/data.frames
#' @param U Matrix of eigenvectors (from SVD)
#' @param rank_rho Rank of rho_d (number of coordinates to keep)
#'
#' @return List of matrices with columns Coord_1..Coord_rank
#' @keywords internal
transform_to_coordinates <- function(data_list, U, rank_rho) {
  purrr::map(
    data_list,
    ~ {
      # Handle empty entries gracefully
      if (is.null(.x) || nrow(.x) == 0 || ncol(.x) == 0) {
        result_matrix <- matrix(nrow = 0, ncol = rank_rho)
        colnames(result_matrix) <- paste0("Coord_", seq_len(rank_rho))
        return(result_matrix)
      }

      X <- as.matrix(.x)
      xp <- matrix(NA_real_, nrow = nrow(X), ncol = rank_rho)

      for (r in seq_len(nrow(X))) {
        row_data <- X[r, , drop = FALSE]
        if (all(is.na(row_data))) next

        x_tilde <- normalize_quantum_vector(row_data)
        u_coords <- get_u_coordinates(x_tilde, U)
        if (is.vector(u_coords)) u_coords <- matrix(u_coords, nrow = 1)

        xp[r, ] <- u_coords[, seq_len(rank_rho), drop = FALSE]
      }

      colnames(xp) <- paste0("Coord_", seq_len(rank_rho))
      xp
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

        res <- .x %>%
          tibble::as_tibble() %>%
          magrittr::set_names(paste0("V_", seq_len(ncol(.)))) %>% # Ensure consistent naming
          dplyr::mutate(
            r = sqrt(V_1^2 + V_2^2),
            phi_1 = atan2(V_2, V_1)
          ) %>%
          dplyr::select(r, phi_1)
        # Normalize angle to [0, 2*pi)
        res$phi_1 <- ifelse(res$phi_1 < 0, res$phi_1 + 2 * pi, res$phi_1)
        res
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
