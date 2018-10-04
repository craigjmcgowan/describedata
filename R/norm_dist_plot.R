#' Create density histogram with normal distribution overlaid
#'
#' Plots a simple density histogram for a continuous variable with a normal
#' distribution overlaid. The overlaid normal distribution has the same mean
#' and standard deviation as the provided variable, and the plot provides a
#' visual means to assess the normality of the variable's distribution.
#'
#' Multiple variables may be plotted by providing data in a long format with
#' all values in the \code{var} variable and the variable labels in the
#' \code{facet} variable. See \code{Examples}.
#'
#'
#' @param df A data.frame or tibble.
#' @param var Continous variable. Must be quoted.
#' @param facet Character vector of measurement names corresponding with values
#'   in \code{var}. Separate calculations will be performed on each group and
#'   the resulting plot will be facetted accordingly. Default \code{NULL}
#'   indicates that all values in \code{var} are for the same measure.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggplot2
#' @export
#' @return A \code{ggplot} object.
#'
#' @examples
#' norm_dist_plot(df = iris, var = "Sepal.Width")
#'
#' You can include multiple variables by combining into a long format as follows:
#'
#' iris_long <- gather(iris, key = "facet_var", value = "plot_var", Sepal.Width, Sepal.Length)
#' norm_dist_plot(df = iris_long, var = "plot_var", facet = "facet_var")

norm_dist_plot <- function(df, var, facet = NULL) {

  quo_var <- rlang::sym(var)
  if(!is.null(facet)) {
    quo_facet <- rlang::sym(facet)
    formula_facet <- as.formula(paste("~ ", facet))
  }

  plot <- df %>%
    # Remove missing values
    filter(!is.na(!!quo_var)) %>%
    when(
      !is.null(facet) ~
        arrange(., !!quo_facet, !!quo_var) %>%
        group_by(!!quo_facet),
      ~ select(., everything())
    ) %>%
    # Create density to overlay
    mutate(grid = seq(min(!!quo_var), max(!!quo_var), length = n()),
           density = dnorm(grid, mean(!!quo_var), sd(!!quo_var))) %>%
    # Plot histogram and density on density scale - need to try and fix if possible
    ggplot(aes(!!quo_var)) +
    geom_histogram(aes(y = ..density..), bins = 20) +
    geom_line(aes(grid, density), col = "red") +
    theme_minimal()

  if(!is.null(facet)) {
    plot <- plot +
      facet_wrap(formula_facet, scales = "free")
  }

  return(plot)
}
