# To check whether this is sourced in/out of an Rmd file:
is_RMD <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))

if (is_RMD) {
  knitr::opts_chunk$set(
    cache = FALSE,
    warning = FALSE,
    message = FALSE,
    echo = TRUE,

    rows.print = 6
  )
}

options(max.print = 5)
options(dplyr.print_min = 5, dplyr.print_max = 5)

set.seed(1974)
library(here)
library(magrittr)
