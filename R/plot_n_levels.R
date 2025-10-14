#' @title Visualize Levels of Categorical Variables
#' @description Bar plot showing number of levels for each factor variable
#' @param dataset A data frame containing factor variables
#' @param lan Language for labels ("en" or "es")
#' @return A ggplot object
#' @export
plot_n_levels <- function(dataset, lan = "en") {
  my_plot <- dataset %>%
    dplyr::select(dplyr::where(is.factor)) %>%
    purrr::map(~ .x %>% levels() %>% length()) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_levels") %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(variable, n_levels), y = n_levels)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip()

  if (lan == "en") {
    my_plot + ggplot2::labs(title = "Number of Levels in Categorical Variables",
                           subtitle = "Recommended: ≤10 levels per variable",
                           x = "Variable", y = "Number of Levels")
  } else if (lan == "es") {
    my_plot + ggplot2::labs(title = "Número de niveles en variables categóricas",
                           subtitle = "Recomendado: ≤10 niveles por variable",
                           x = "Variable", y = "Número de niveles")
  } else {
    rlang::abort("Language not supported. Use 'en' or 'es'")
  }
}
