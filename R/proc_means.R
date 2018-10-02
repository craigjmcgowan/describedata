#' Replica of SAS's PROC MEANS
#'
#' Descriptive statistics for continuous variables, with the option of
#' stratifying by a categorical variable.
#'
#' @param df A data frame or tibble.
#' @param vars A character vector of numeric variables to generate descriptive
#'   statistics for. If the default (\code{NULL}), all variables are included,
#'   except for any specified in \code{by}.
#' @param by Discrete variable. Separate statistics will be produced for
#'   each level. Default \code{NULL} provides statistics for all observations.
#' @param n logical. Display number of rows with values. Default \code{TRUE}.
#' @param mean logical. Display mean value. Default \code{TRUE}.
#' @param sd logical. Display standard deviation. Default \code{TRUE}.
#' @param min logical. Display minimum value. Default \code{TRUE}.
#' @param max logical. Display maximum value. Default \code{TRUE}.
#' @param median logical. Display median value. Default \code{FALSE}.
#' @param q1 logical. Display first quartile value. Default \code{FALSE}.
#' @param q3 logical. Display third quartile value. Default \code{FALSE}.
#' @param iqr logical. Display interquartile range. Default \code{FALSE}.
#' @param nmiss logical. Display number of missing values. Default \code{FALSE}.
#' @param nobs logical. Display total number of rows. Default \code{FALSE}.
#' @param p logical. Calculate p-value across \code{by} groups using \code{aov}.
#'   Ignored if no \code{by} variable specified. Default \code{FALSE}.
#'
#' @import dplyr
#' @import tidyr
#' @import haven
#' @import purrr
#' @export
#' @return A data.frame with columns variable, \code{by} variable, and
#'   a column for each summary statistic.
#'
#' @examples
#' proc_means(iris, vars = c("Sepal.Length", "Sepal.Width"))
#' proc_means(iris, by = "Species")
#'

proc_means <- function(df, vars = NULL, by = NULL, n = T, mean = TRUE,
                       sd = TRUE, min = TRUE, max = TRUE, median = FALSE,
                       q1 = FALSE, q3 = FALSE, iqr = FALSE, nmiss = FALSE,
                       nobs = FALSE, p = FALSE) {

  if(is.null(by)) p <- FALSE

  # Create quosure of by variable if provided
  if (!is.null(by)) quo_by <- rlang::sym(by)

  # If no variables provided, use all variables
  if (is.null(vars)) vars <- names(df)[!names(df) %in% by]

  # Select variables requested and zap formats and labels from import
  data <- df %>%
    when(!is.null(by) ~ select(., one_of(c(vars, by))),
         ~ select(., one_of(vars))) %>%
    zap_formats() %>%
    zap_labels()

  # Remove labels if for some reason zap_labels doesn't
  for (var in colnames(data)) {
    attr(data[[deparse(as.name(var))]], "label") <- NULL
  }

  # Gather variables together
  data <- data %>%
    when(!is.null(by) ~ gather(., key = "variable", value = "value", -!!quo_by) %>%
           group_by(variable, !!quo_by),
         ~ gather(., key = "variable", value = "value") %>%
           group_by(variable)
         )

  # Calculate p-values
  if(isTRUE(p)) {
    p_values <- data %>%
      group_by(variable) %>%
      do(broom::tidy(aov(value ~ !!quo_by, data = .))) %>%
      filter(!is.na(p.value)) %>%
      mutate(p.value = ifelse(p.value < 0.001, "< 0.001", paste(round(p.value, 3)))) %>%
      select(variable, p.value)
  }

  # Calculate summary statistics and return requested stats
  data %>%
    # Calculate all summary statistics
    summarize(N = sum(!is.na(value)),
              Mean = round(mean(value, na.rm = TRUE), 3),
              SD = round(sd(value, na.rm = TRUE), 3),
              Min = round(min(value, na.rm = TRUE), 3),
              Max = round(max(value, na.rm = TRUE), 3),
              Median = round(median(value, na.rm = TRUE), 3),
              Q1 = round(quantile(value, 0.25, na.rm = TRUE), 3),
              Q3 = round(quantile(value, 0.75, na.rm = TRUE), 3),
              IQR = round(IQR(value, na.rm = TRUE), 3),
              NMiss = sum(is.na(value)),
              NObs = n()) %>%
    # Select desired statistics to return and display
    when(!isTRUE(n) ~ select(., -N),
         ~ select(., everything())) %>%
    when(!isTRUE(mean) ~ select(., -Mean),
         ~ select(., everything())) %>%
    when(!isTRUE(sd) ~ select(., -SD),
         ~ select(., everything())) %>%
    when(!isTRUE(min) ~ select(., -Min),
         ~ select(., everything())) %>%
    when(!isTRUE(max) ~ select(., -Max),
         ~ select(., everything())) %>%
    when(!isTRUE(median) ~ select(., -Median),
         ~ select(., everything())) %>%
    when(!isTRUE(q1) ~ select(., -Q1),
         ~ select(., everything())) %>%
    when(!isTRUE(q3) ~ select(., -Q3),
         ~ select(., everything())) %>%
    when(!isTRUE(iqr) ~ select(., -IQR),
         ~ select(., everything())) %>%
    when(!isTRUE(nmiss) ~ select(., -NMiss),
         ~ select(., everything())) %>%
    when(!isTRUE(nobs) ~ select(., -NObs),
         ~ select(., everything())) %>%
    when(isTRUE(p) ~ left_join(., p_values, by = "variable") %>%
           mutate(p.value = ifelse(row_number() == 1, p.value, "")),
         ~ select(., everything()))

}
