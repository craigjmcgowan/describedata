#' Replica of Stata's ladder function
#'
#' Searches the ladder of powers histograms to find a transformation to make
#' \code{x} normally distributed. The Shapiro-Wilkes test is used to assess for
#' normality. The following transformations are included: identity, cubic,
#' square, square root, natural logarithm, inverse square root, inverse, 
#' inverse square, and inverse cubic.
#'
#' @param x A continuous numeric vector.
#' 
#' @import dplyr
#' @import broom
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom stats shapiro.test
#' 
#' @export
#' @return A data.frame 
#'   
#' @examples
#' ladder(iris$Sepal.Length)
#' ladder(mtcars$disp)
#'

ladder <- function(x) {
  
  order <- c("cubic", "square", "identity", "sqrt", "log", "1/sqrt",
             "inverse", "1/square", "1/cubic")
  
  tibble(identity = x,
         cubic = identity^3,
         square = identity^2,
         sqrt = sqrt(identity),
         log = log(identity),
         `1/sqrt` = 1/.data$sqrt,
         inverse = 1/.data$identity,
         `1/square` = 1/.data$square,
         `1/cubic` = 1/.data$cubic) %>%
    gather(key = "Transformation", value = "value") %>%
    filter(!is.na(.data$value)) %>%
    group_by(.data$Transformation) %>%
    do(tidy(shapiro.test(x = .$value))) %>%
    ungroup() %>%
    mutate(Transformation = factor(.data$Transformation, 
                                   levels = order)) %>%
    arrange(.data$Transformation) %>%
    select(-.data$method)
}


#' Replica of Stata's gladder function
#'
#' Creates ladder-of-powers histograms to visualize nine common transformations
#' and compare each to a normal distribution. The following transformations are
#' included: identity, cubic, square, square root, natural logarithm, inverse
#' square root, inverse, inverse square, and inverse cubic.
#'
#' @param x A continuous numeric vector.
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom stats dnorm sd
#' 
#' @export
#' @return A ggplot object with plots of each transformation
#'   
#' @examples
#' gladder(iris$Sepal.Length)
#' gladder(mtcars$disp)
#'

gladder <- function(x) {
  
  order <- c("cubic", "square", "identity", "sqrt", "log", "1/sqrt",
             "inverse", "1/square", "1/cubic")
  
  tibble(var = x) %>%
    mutate(identity = .data$var,
           cubic = identity^3,
           square = identity^2,
           sqrt = sqrt(identity),
           log = log(identity),
           `1/sqrt` = 1/.data$sqrt,
           inverse = 1/.data$identity,
           `1/square` = 1/.data$square,
           `1/cubic` = 1/.data$cubic) %>%
    select(-.data$var) %>%
    gather(key = "Transformation", value = "value") %>%
    filter(!is.na(.data$value)) %>%
    mutate(Transformation = factor(.data$Transformation, 
                                   levels = order)) %>%
    arrange(.data$Transformation, .data$value) %>%
    group_by(.data$Transformation) %>%
    # Create density to overlay
    mutate(grid = seq(min(.data$value), max(.data$value), length = n()),
           density = dnorm(.data$grid, mean(.data$value), sd(.data$value))) %>%
    # Plot histogram and density on density scale - need to try and fix if possible
    ggplot(aes(.data$value)) +
    geom_histogram(aes(y = .data$..density..), bins = 20) +
    geom_line(aes(.data$grid, .data$density), col = "red") +
    facet_wrap(~ .data$Transformation, scales = "free") +
    labs(x = "Value",
         y = "Fraction") +
    theme_bw()
}