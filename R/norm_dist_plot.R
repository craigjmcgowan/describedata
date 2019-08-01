#' Create density histogram with normal distribution overlaid
#'
#' Plots a simple density histogram for a continuous variable with a normal
#' distribution overlaid. The overlaid normal distribution has the same mean
#' and standard deviation as the provided variable, and the plot provides a
#' visual means to assess the normality of the variable's distribution.
#'
#'
#' @param df A data.frame or tibble.
#' @param vars A character vector of continuous variable names.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @importFrom rlang .data
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
      select(.data$facet, .data$value)
  } else {
    quo_var <- rlang::sym(vars)
    df <- mutate(df,
                 facet = paste(vars),
                 value = !!quo_var)
  }

  df %>%
    # Remove missing values
    filter(!is.na(.data$value)) %>%
    arrange(.data$facet, .data$value) %>%
    group_by(.data$facet) %>%
    # Create density to overlay
    mutate(grid = seq(min(.data$value), max(.data$value), length = n()),
           density = dnorm(.data$grid, mean(.data$value), sd(.data$value))) %>%
    # Plot histogram and density on density scale - need to try and fix if possible
    ggplot(aes(.data$value)) +
    geom_histogram(aes(y = .data$..density..), bins = 20) +
    geom_line(aes(.data$grid, .data$density), col = "red") +
    facet_wrap(~ .data$facet, scales = "free") +
    theme_minimal()

}
