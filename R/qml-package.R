#' qml: Quantum Machine Learning for Classification
#'
#' @description
#' Quantum machine learning classification using Density Matrix (DM) algorithm.
#' Main functions: dm_fit(), predict(), prepare_split_data().
#'
#' @section Quick Start:
#' ```r
#' # Basic usage
#' model <- dm_fit(Species ~ ., data = iris, n_breaks = 3)
#' predictions <- predict(model, newdata = iris)
#' 
#' # With rsample
#' library(rsample)
#' split <- initial_split(iris, strata = Species)
#' prepared_data <- prepare_split_data(split, "Species")
#' model <- dm_fit(Species ~ ., data = prepared_data)
#' ```
#'
#' @docType package
#' @name qml-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle badge
#' @importFrom rlang sym "!!"
#' @importFrom fasano.franceschini.test fasano.franceschini.test
## usethis namespace: end
NULL
