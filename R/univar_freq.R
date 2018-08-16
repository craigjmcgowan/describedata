#' Univariate statistics for a discrete variable
#'
#' Descriptive statistics (N,%) for a continuous variable
#'
#' @param df A data frame or tibble.
#' @param var A discrete, numeric variable.
#' @param na.rm logical. Should missing values (including \code{NaN}) be
#'   removed?)
#'
#' @import dplyr
#' @export
#' @return A data.frame with columns \code{var}, NObs, and Percent
#'
#' @examples
#' univar_freq(iris, var = "Species")
#' univar_freq(mtcars, var = "cyl")
#'

univar_freq <- function(df, var, na.rm = FALSE) {

  quo_var <- quo_by <- rlang::sym(var)

  # Remove missing values if requested
  if (isTRUE(na.rm)) df <- filter(df, !is.na(!!quo_var))

  # Determine number and percent of observations in each category
  df %>%
    group_by(!!quo_var) %>%
    summarize(NObs = n(),
              Percent = round(n() / nrow(.), 3)) %>%
    # Make variable name column character
    mutate(!!quo_name(quo_var) := as.character(!!quo_var)) %>%
    # Add total N
    bind_rows(df %>%
                summarize(!!quo_name(quo_var) := "Total",
                          NObs = n(),
                          Percent = round(n() / nrow(.), 3)))

}
