#' @title Quantum Matrix Operations for Classification
#' @description Functions for creating and manipulating quantum density matrices
#'
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @import tibble
#' @importFrom tidyr unnest_wider

#' @title Discretize Continuous Features
#'
#' @description Converts continuous variables to factors using cut() function
#'
#' @param data Input data frame
#' @param n_breaks Number of breaks for discretization
#'
#' @return Data frame with continuous variables discretized
#'
#' @keywords internal
discretize_continuous_features <- function(data, n_breaks = 3, breaks_list = NULL) {
  data <- tibble::as_tibble(data)
  # Build breaks_list if not provided
  if (is.null(breaks_list)) {
    breaks_list <- purrr::imap(data, function(col, nm) {
      if (is.numeric(col)) {
        rng <- range(col, na.rm = TRUE)
        if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
          # Degenerate range: create small epsilon range
          rng <- c(ifelse(is.finite(rng[1]), rng[1], 0), ifelse(is.finite(rng[2]), rng[2], 1))
          rng[2] <- rng[1] + 1e-6
        }
        seq(rng[1], rng[2], length.out = n_breaks + 1)
      } else {
        NULL
      }
    })
  }

  # Apply discretization using explicit breaks
  data %>%
    purrr::imap(~ {
      if (is.numeric(.x) && !is.null(breaks_list[[.y]])) {
        cut(.x, breaks = breaks_list[[.y]], include.lowest = TRUE, right = TRUE)
      } else {
        .x
      }
    }) %>%
    tibble::as_tibble()
}

#' @title Encode Logical Variables
#'
#' @description Converts logical variables to one-hot encoding
#'
#' @param data Input data frame
#' @param test_var Name of test indicator variable to preserve
#'
#' @return Data frame with logical variables encoded
#'
#' @keywords internal
encode_logical_variables <- function(data, test_var) {
  # Expand logical columns into one-hot while preserving all other columns
  expanded <- purrr::imap(data, function(col, nm) {
    if (nm %in% test_var) return(col)
    if (is.logical(col)) {
      purrr::map(col, function(value) {
        template <- rep(0, 2)
        if (!is.na(value)) template[as.numeric(value) + 1] <- 1
        names(template) <- c("FALSE", "TRUE")
        template
      })
    } else {
      col
    }
  })
  expanded %>% tibble::as_tibble()
}

#' @title Encode Categorical Variables
#'
#' @description Converts factor variables to one-hot encoding
#'
#' @param data Input data frame
#' @param objective_var Name of objective variable
#' @param test_var Name of test indicator variable
#'
#' @return Data frame with factor variables encoded
#'
#' @keywords internal
encode_categorical_variables <- function(data, objective_var, test_var, levels_list = NULL) {
  objective_data <- data %>%
    dplyr::select(dplyr::all_of(objective_var), dplyr::all_of(test_var))

  non_target <- data %>% dplyr::select(-dplyr::all_of(objective_var), -dplyr::all_of(test_var))

  encoded_cols <- purrr::imap(non_target, function(col, nm) {
    if (is.factor(col)) {
      # Enforce training-time levels if provided
      if (!is.null(levels_list) && !is.null(levels_list[[nm]])) {
        col <- factor(col, levels = levels_list[[nm]])
      }
      level_names <- levels(col)
      n_levels <- length(level_names)
      res <- purrr::map(col, function(value) {
        template <- rep(0, n_levels)
        if (!is.na(value) && as.numeric(value) <= n_levels) {
          template[as.numeric(value)] <- 1
        }
        names(template) <- if (length(level_names) == n_levels) level_names else paste0("level_", 1:n_levels)
        template
      })
      return(res)
    } else {
      return(col)
    }
  })

  processed_data <- encoded_cols %>% tibble::as_tibble() %>% dplyr::bind_cols(objective_data)
  return(processed_data)
}

#' @title Encode Target Variable
#'
#' @description Converts objective variable to one-hot class indicators
#'
#' @param data Input data frame
#' @param objective_var Name of objective variable
#'
#' @return Data frame with objective variable encoded as class indicators
#'
#' @keywords internal
encode_target_variable <- function(data, objective_var) {
  n_classes <- length(levels(as.factor(data[[objective_var]])))
  class_template <- rep(0, n_classes)

  data[[objective_var]] %<>%
    purrr::map(function(class_value) {
      template <- class_template
      template[as.numeric(class_value)] <- 1
      template %>% magrittr::set_names(paste0("class_", 1:n_classes))
    })
    
  return(data)
}

#' @title Compute breaks list for numeric columns
#' @keywords internal
compute_breaks_list <- function(data, n_breaks = 3) {
  data <- tibble::as_tibble(data)
  purrr::imap(data, function(col, nm) {
    if (is.numeric(col)) {
      rng <- range(col, na.rm = TRUE)
      if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
        rng <- c(ifelse(is.finite(rng[1]), rng[1], 0), ifelse(is.finite(rng[2]), rng[2], 1))
        rng[2] <- rng[1] + 1e-6
      }
      seq(rng[1], rng[2], length.out = n_breaks + 1)
    } else {
      NULL
    }
  })
}

#' @title Create Quantum Matrix Representation
#'
#' @description Converts encoded data to final quantum matrix format
#'
#' @param data Input data with encoded variables
#' @param test_var Name of test indicator variable
#'
#' @return Data frame in quantum matrix format
#'
#' @keywords internal
create_quantum_matrix_format <- function(data, test_var) {
  test_data <- data[test_var]
  
  quantum_data <- data %>%
    dplyr::select(-dplyr::all_of(test_var)) %>%
    tidyr::unnest_wider(
      dplyr::everything(),
      names_sep = "_",
      names_repair = "unique"
    ) %>%
    dplyr::bind_cols(test_data, .name_repair = "unique")
    
  return(quantum_data)
}

#' @title Separate Data by Class
#'
#' @description Splits quantum matrix data by class labels
#'
#' @param data Quantum matrix formatted data
#' @param objective_var Name of objective variable
#' @param n_classes Number of classes
#'
#' @return List of data frames, one per class
#'
#' @keywords internal
separate_data_by_class <- function(data, objective_var, n_classes) {
  class_indicators <- paste0(objective_var, "_class_", 1:n_classes)
  class_data <- vector("list", n_classes)

  for (i in seq_len(n_classes)) {
    class_data[[i]] <- data %>%
      dplyr::filter(!!rlang::sym(class_indicators[i]) == 1) %>%
      dplyr::select(-dplyr::starts_with(objective_var))
  }
  
  return(class_data)
}

#' @title Split Train and Test Data
#'
#' @description Separates class data into training and test sets
#'
#' @param class_data List of data frames by class
#' @param test_var Name of test indicator variable
#'
#' @return List with train and test data components
#'
#' @keywords internal
split_train_test_data <- function(class_data, test_var) {
  train_data <- class_data %>%
    purrr::map(
      ~ .x %>%
        dplyr::filter(!!rlang::sym(test_var) == 0) %>%
        dplyr::select(-dplyr::all_of(test_var))
    )

  test_data <- class_data %>%
    purrr::map(
      ~ .x %>%
        dplyr::filter(!!rlang::sym(test_var) == 1) %>%
        dplyr::select(-dplyr::all_of(test_var))
    )
    
  return(list(train = train_data, test = test_data))
}

#' @title Compute Quantum Density Matrix
#'
#' @description Creates the quantum density matrix (rho_d) from training data
#'
#' @param train_data List of training data by class
#'
#' @return Quantum density matrix with proper normalization
#'
#' @keywords internal
compute_quantum_density_matrix <- function(train_data) {
  # Compute feature matrix from training data
  feature_matrix <- train_data %>%
    purrr::map(
      ~ {
        if (nrow(.x) == 0 || ncol(.x) == 0) {
          return(matrix(0, nrow = 1, ncol = 1))
        } else {
          col_sums <- .x %>% colSums(na.rm = TRUE)
          return(col_sums %>% as.matrix())
        }
      }
    )

  # Ensure consistent dimensions
  max_rows <- max(sapply(feature_matrix, nrow), 0)
  if (max_rows == 0) {
    feature_matrix <- matrix(0, nrow = 1, ncol = length(train_data))
  } else {
    feature_matrix <- feature_matrix %>%
      purrr::map(
        ~ {
          if (nrow(.x) < max_rows) {
            rbind(.x, matrix(0, nrow = max_rows - nrow(.x), ncol = ncol(.x)))
          } else {
            .x
          }
        }
      ) %>%
      do.call(cbind, .)
  }

  # Create quantum matrix and density matrix
  quantum_matrix <- sqrt(feature_matrix)
  outer_product <- quantum_matrix %*% t(quantum_matrix)
  trace_value <- compute_trace(outer_product)
  
  # Handle zero trace case
  if (abs(trace_value) < 1e-15) {
    warning("Trace of outer product is zero or near zero. Using regularization.")
    rho_d <- outer_product + diag(nrow(outer_product)) * 1e-10
    rho_d <- rho_d / compute_trace(rho_d)
  } else {
    rho_d <- outer_product / trace_value
  }
  
  return(rho_d)
}

#' @title Create Quantum Density Matrix from Data
#'
#' @description Main function that orchestrates the complete quantum matrix creation process
#'
#' @param Data Input data with test/train indicator
#' @param objective_var Name of objective variable
#' @param n_breaks Number of breaks for discretization
#' @param verbose Verbosity level (0 = silent, >2 = detailed output)
#' @param test_var Name of test indicator variable
#'
#' @return List containing rho_d matrix, training data, and test data
#'
#' @export
get_rho_d <- function(
  Data,
  objective_var = NULL,
  n_breaks = 3,
  verbose = 0,
  test_var = "test",
  breaks_list = NULL,
  cat_levels = NULL
) {
  # Step 1: Prepare and validate data
  Data <- Data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(!!rlang::sym(objective_var) := as.factor(!!rlang::sym(objective_var)))
  # Determine number of classes from objective variable levels (before encoding)
  n_classes <- length(levels(Data[[objective_var]]))

  # Step 2: Discretize continuous features
  Data <- discretize_continuous_features(Data, n_breaks, breaks_list)
  
  # Step 3: Encode logical variables
  Data <- encode_logical_variables(Data, test_var)
  
  # Step 4: Encode categorical variables
  Data <- encode_categorical_variables(Data, objective_var, test_var, cat_levels)
  
  # Step 5: Encode target variable
  Data <- encode_target_variable(Data, objective_var)
  
  # Step 6: Create quantum matrix format
  Data <- create_quantum_matrix_format(Data, test_var)
  
  if (verbose > 2) {
    cat("Quantum matrix data structure:\n")
    Data %>% dplyr::glimpse()
  }
  
  # Step 7: Separate data by class (use factor levels count from training data)
  
  class_data <- separate_data_by_class(Data, objective_var, n_classes)
  
  # Step 8: Split train and test data
  train_test_split <- split_train_test_data(class_data, test_var)
  
  # Step 9: Compute quantum density matrix
  rho_d <- compute_quantum_density_matrix(train_test_split$train)
  
  # Step 10: Validate quantum properties
  validate_quantum_matrix(rho_d)

  return(list(
    rho_d = rho_d,
    train_C = train_test_split$train,
    test_C = train_test_split$test
  ))
}

#' @title Compute Matrix Trace
#'
#' @description Calculates the trace (sum of diagonal elements) of a matrix
#'
#' @param mat Input matrix
#'
#' @return Numeric value of the trace
#'
#' @keywords internal
compute_trace <- function(mat) {
  mat %>% diag() %>% sum()
}

#' @title Validate Quantum Density Matrix Properties
#'
#' @description Checks if a matrix satisfies quantum density matrix properties
#'
#' @param rho_d Input matrix to validate
#'
#' @return TRUE if valid, otherwise throws warning
#'
#' @keywords internal
validate_quantum_matrix <- function(rho_d) {
  # This matrix is called a density matrix and it is a measure of quantum probability (also called non-classical probability).
  # That's because it has the following properties:
  eigenvalues <- eigen(rho_d)$values

  is_positive_semidefinite <- all(eigenvalues >= 0 | eigenvalues < 1e-15)
  has_unit_trace <- abs(compute_trace(rho_d) - 1) < 1e-10
  is_hermitian <- all(abs(rho_d - t(rho_d)) < 1e-15)

  if (!(is_positive_semidefinite && has_unit_trace && is_hermitian)) {
    warning(
      "La matriz rho_d no cumple con las propiedades de una matriz de densidad cuántica",
      call. = FALSE
    )
  }

  return(TRUE)
}

#' @title Perform SVD on Quantum Density Matrix
#'
#' @description Computes singular value decomposition for the quantum density matrix
#'
#' @param rho_d Quantum density matrix
#' @param zero_limit Precision limit for determining non-zero eigenvalues
#'
#' @return List with SVD components and rank information
#'
#' @keywords internal
compute_quantum_svd <- function(rho_d, zero_limit = 4) {
  # Obtenemos el rango de rho_d
  lambda <- rho_d %>% eigen() %>% .$values %>% sort() %>% round(zero_limit)
  rank_rho <- sum(lambda != 0)

  # SVD con rho_d
  svd_rho_d <- svd(rho_d)
  n_nonzero <- svd_rho_d$d %>%
    round(zero_limit) %>%
    equals(0) %>%
    not() %>%
    sum()

  if (n_nonzero != rank_rho) {
    stop("El rango de rho_d no coincide con el número de autovalores no nulos")
  }

  U <- svd_rho_d$u[, 1:rank_rho] # Los r autovectores de rho_d, por columnas

  return(list(
    U = U,
    rank = rank_rho,
    eigenvalues = lambda,
    svd = svd_rho_d
  ))
}
