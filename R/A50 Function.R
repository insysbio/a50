#
#    A50 Calculator
#

## First specify the packages of interest
packages = c("stringr", "dplyr", "tibble")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

extract <- function(text) {
    text |>
    stringr::str_split(',', simplify = F) |>
    unlist() |>
    as.numeric()
}

A50 <- function(x, y) {
    tibble::tibble(
        x = extract(x),
        y = extract(y)
    ) |>
    dplyr::mutate(
        y_av = (max(y) + min(y)) / 2,
        delta = abs(y_av - y),
        target = (delta + dplyr::lead(delta)) ==
            abs(dplyr::lead(y) - y),
        A50 = x + delta * (abs(dplyr::lead(x) - x) /
                           abs(dplyr::lead(y) - y))
    ) |>
    dplyr::filter(target == T) |>
    dplyr::pull(A50) |>
    unique() |>
    max() ## |> round(3)
}

## A50(x,y)
