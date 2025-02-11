#' @title Print a table as LaTeX
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param my_param Yep, it's a parameter.
#'
#' @return It returns...
#'
#' @examples
#' print("TODO:")
#'
#' @export
tib2latex <- function(tbl, caption = "Table", label = "tab:table") {
  tbl %>%
    knitr::kable(format = "latex", booktabs = TRUE, caption = caption) %>%
    cat()
}
