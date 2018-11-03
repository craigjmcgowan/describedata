#' Create density histogram with normal distribution overlaid
#'
#' Plots a simple density histogram for a continuous variable with a normal
#' distribution overlaid. The overlaid normal distribution has the same mean
#' and standard deviation as the provided variable, and the plot provides a
#' visual means to assess the normality of the variable's distribution.
#'
#'
#' @param df A data.frame or tibble.
#' @param vars A character vector of continous variable names.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @export
#' @return A \code{ggplot} object.
#'
#' @examples
#' norm_dist_plot(df = iris, vars = "Sepal.Width")
#'
#' norm_dist_plot(df = iris,
#'                vars = c("Sepal.Width", "Sepal.Length"))

norm_dist_plot <- function(df, vars) {

  # check for valid and multiple variables
  if(!all(vars %in% names(df)))
    stop("Not all variables are in the provided data.frame")

  if(length(vars) > 1) {
    df <- suppressWarnings(gather(df, key = "facet", value = "value",
                 vars)) %>%
      select(facet, value)
  } else {
    quo_var <- rlang::sym(vars)
    df <- mutate(df,
                 facet = paste(vars),
                 value = !!quo_var)
  }

  df %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    arrange(facet, value) %>%
    group_by(facet) %>%
    # Create density to overlay
    mutate(grid = seq(min(value), max(value), length = n()),
           density = dnorm(grid, mean(value), sd(value))) %>%
    # Plot histogram and density on density scale - need to try and fix if possible
    ggplot(aes(value)) +
    geom_histogram(aes(y = ..density..), bins = 20) +
    geom_line(aes(grid, density), col = "red") +
    facet_wrap(~facet, scales = "free") +
    theme_minimal()

}
