#' @title Kernel Functions for Quantum Matrix Classification
#' @description Mathematical kernel functions and probability estimation for quantum classification
#' 
#' @import dplyr
#' @import magrittr
#' @import purrr

#' @title Quantum Kernel Function
#' @param ... Coordinate values to evaluate kernel at
#' @param r Kernel parameter (default = 2)
#' @return Kernel value at the specified coordinates
#' @export
K <- function(..., r = 2) {
  x <- c(...)
  sxx <- sum(x^2)
  if (sxx < 1) return(1 / 2 * (r + 2) * (1 - sxx)) else return(0)
}

#' @title Probability Density Estimation
#' @param D_tilde Training data in polar coordinates (list by class)
#' @param x Point to evaluate density at (vector of coordinates)
#' @param clase Class index to estimate density for
#' @param h_window Bandwidth parameter for kernel (default = 0.1)
#' @return Estimated probability density value
#' @export
f_hat_h <- function(D_tilde, x, clase, h_window = 0.1) {
  n_total <- D_tilde %>% dplyr::bind_rows() %>% nrow()
  
  D_tilde[[clase]] %>%
    dplyr::mutate(
      v_1 = (x[1] - r) / h_window,
      v_2 = (x[2] - phi_1) / h_window
    ) %>%
    dplyr::mutate(kernel_value = purrr::pmap_dbl(dplyr::select(., v_1, v_2), K)) %>%
    dplyr::pull(kernel_value) %>%
    sum() %>%
    magrittr::divide_by(n_total * h_window^2)
}

#' @title Advanced Kernel Density Estimation
#' @param training_data Training data in polar coordinates
#' @param query_point Point to evaluate density at
#' @param class_index Class to estimate density for
#' @param bandwidth Bandwidth parameter (default = 0.1)
#' @param kernel_type Type of kernel
#' @return Estimated probability density
#' @keywords internal
estimate_class_density <- function(training_data, query_point, class_index, bandwidth = 0.1, kernel_type = "quantum") {
  if (class_index > length(training_data)) stop("Class index exceeds number of available classes")
  if (length(query_point) != 2) stop("Query point must have exactly 2 coordinates")
  
  class_data <- training_data[[class_index]]
  if (nrow(class_data) == 0) return(0)
  
  total_n <- training_data %>% purrr::map_int(nrow) %>% sum()
  if (total_n == 0) return(0)
  
  kernel_sum <- class_data %>%
    dplyr::mutate(
      v_1 = (query_point[1] - r) / bandwidth,
      v_2 = (query_point[2] - phi_1) / bandwidth
    ) %>%
    dplyr::mutate(kernel_value = purrr::pmap_dbl(dplyr::select(., v_1, v_2), ~ K(.x, .y))) %>%
    dplyr::pull(kernel_value) %>%
    sum()
  
  return(kernel_sum / (total_n * bandwidth^2))
}

#' @title Batch Density Estimation
#' @param training_data Training data in polar coordinates
#' @param query_points Matrix of query points
#' @param class_index Class to estimate density for
#' @param bandwidth Bandwidth parameter
#' @return Vector of density estimates
#' @keywords internal
batch_estimate_density <- function(training_data, query_points, class_index, bandwidth = 0.1) {
  if (is.vector(query_points)) query_points <- matrix(query_points, nrow = 1)
  apply(query_points, 1, function(point) estimate_class_density(training_data, point, class_index, bandwidth))
}
