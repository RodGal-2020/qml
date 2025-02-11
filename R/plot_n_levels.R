#' plot_n_levels
#'
#' Visualize the number of levels in n variables within a dataset.
#'
#' @title Visualize Levels of n Variables
#'
#' @description
#' This function takes a dataset as input and generates a bar plot showing the number of levels for each n variable (factor) in the dataset.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param dataset A data frame or tibble containing the dataset. The function will look for factor variables in this dataset.
#'
#' @return A ggplot object showing the number of levels for each categorical variable.
#'
#' @examples
#' # Example usage:
#' library(ggplot2)
#' library(tidyverse)
#' df <- tibble(
#'   category1 = factor(sample(letters[1:5], 100, replace = TRUE)),
#'   category2 = factor(sample(LETTERS[1:3], 100, replace = TRUE)),
#'   value = rnorm(100)
#' )
#' plot_n_levels(df)
#'
#' @section Warning:
#' This function assumes the input dataset contains factor variables. Non-factor variables will be ignored.
#'
#' @export
plot_n_levels <- function(dataset, lan = "en") {
  my_plot <- dataset %>%
    select(where(is.factor)) %>%
    map(~ .x %>% levels %>% length) %>%
    as_tibble() %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_levels") %>%
    ggplot(aes(x = reorder(variable, n_levels), y = n_levels)) +
    geom_col() +
    coord_flip()

  if (lan == "en") {
    my_plot <- my_plot +
      labs(
        title = "Number of Levels in Categorical Variables",
        subtitle = "It is recommended that a categorical variable should not have more than 10 levels",
        x = "Variable",
        y = "Number of Levels"
      )
  } else if (lan == "es") {
    my_plot <- my_plot +
      labs(
        title = "Número de niveles en variables categóricas",
        subtitle = "Se recomienda que no haya más de 10 niveles en una variable categórica",
        x = "Variable",
        y = "Número de niveles"
      )
  } else {
    stop("Language not supported. Please use 'en' or 'es'.")
  }

  return(my_plot)
}
