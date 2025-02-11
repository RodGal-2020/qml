#' as_qmatrix
#'
#' @title Transform a vector with context into a qvector, made out of 1s and 0s.
#'
#' @description For each value
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .data A `tibble` containing the context of the vector $v$
#'
#' @return A qmatrix, $V'$, or the derivative $X$, which is $V'$ but summing across the rows.
#'
#' @examples
#'
#' as_qmatrix(tibble(iris))
#'
#' @export
as_qmatrix = function(.data) {
  # .data = tibble(iris)

  # Factors
  recode_factor = function(v) {
    # v = .data$Species
    fs = levels(v)
    n_fs = length(fs)
    coded_v = rep(0, n_fs)


    v %>%
      # Now we get the position of each value of v in fs using map functions
      purrr::map_dbl(function(x) {
        which(x == fs)
      }) %>%
      tibble::tibble() %>%
      dplyr::rename(pos = ".") # %>%
      # Now we turn each row value i into a 1 into a tibble made out of n_fs 0s
      # purrr::map(function(i) {
      #   rep(0, n_fs) %>%
      #     replace(i, 1)
      # })
  }
}
