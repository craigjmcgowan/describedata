#' Replica of SAS's PROC MEANS
#'
#' Descriptive statistics for continuous variables, with the option of
#' stratifying by a categorical variable.
#'
#' @param df A data frame or tibble.
#' @param vars Character vector of numeric variables to generate descriptive
#'   statistics for. If the default (\code{NULL}), all variables are included,
#'   except for any specified in \code{by}.
#' @param var_order Character vector listing the variable names in the order
#'   results should be displayed. If the default (\code{NULL}), variables are
#'   displayed in the order specified in \code{vars}.
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
#' @param p_round Number of decimal places p-values should be rounded to.
#' @param display_round Number of decimal places displayed values should be 
#'   rounded to
#'
#' @import dplyr
#' @import tidyr
#' @import haven
#' @import purrr
#' @importFrom rlang .data
#' @importFrom stats aov 
#' @export
#' @return A data.frame with columns variable, \code{by} variable, and
#'   a column for each summary statistic.
#'
#' @examples
#' proc_means(iris, vars = c("Sepal.Length", "Sepal.Width"))
#' proc_means(iris, by = "Species")
#'

proc_means <- function(df, vars = NULL, var_order = NULL, by = NULL, n = T,
                       mean = TRUE, sd = TRUE, min = TRUE, max = TRUE,
                       median = FALSE, q1 = FALSE, q3 = FALSE, iqr = FALSE,
                       nmiss = FALSE, nobs = FALSE, p = FALSE, p_round = 4,
                       display_round = 3) {

  if(is.null(by)) p <- FALSE

  # Create quosure of by variable if provided
  if (!is.null(by)) quo_by <- rlang::sym(by)

  # If no variables provided, use all variables
  if (is.null(vars)) vars <- names(df)[!names(df) %in% by]

  # Order for displaying results
  if(is.null(var_order)) var_order <- vars

  if(length(var_order) != length(vars)) {
    stop(paste(length(var_order), "variables in var_order, but stats calculated for",
               length(vars),
               "variables.\nCheck var_order or vars that all variables included."))
  }
  if(!all(var_order %in% vars)) {
    stop(paste("The following variables in var_order need to be included in vars:\n",
               paste(var_order[!var_order %in% vars],
                     collapse = ",")))
  }
  if(!all(vars %in% var_order)) {
    stop(paste("The following variables in vars need to be included in var_order:\n",
               paste(vars[!vars %in% var_order],
                     collapse = ",")))
  }

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
           group_by(.data$variable, !!quo_by),
         ~ gather(., key = "variable", value = "value") %>%
           group_by(.data$variable)
         )

  # Calculate p-values
  if(isTRUE(p)) {
    p_values <- data %>%
      group_by(.data$variable) %>%
      do(broom::tidy(aov(value ~ !!quo_by, data = .))) %>%
      filter(!is.na(.data$p.value)) %>%
      mutate(p.value = case_when(as.numeric(.data$p.value) <
                                   as.numeric(paste0("1e-", p_round)) ~
                                   paste0("< 0.",
                                          paste0(rep.int(0, p_round-1), collapse = ""),
                                          "1"),
                                 as.numeric(.data$p.value) >=
                                   as.numeric(paste0("1e-", p_round)) ~
                                   format(round(as.numeric(.data$p.value), p_round),
                                          nsmall = p_round),
                                 TRUE ~ NA_character_)) %>%
      select(.data$variable, .data$p.value) %>%
      ungroup()
  }

  # Calculate summary statistics and return requested stats
  data %>%
    # Calculate all summary statistics
    summarize(N = sum(!is.na(.data$value)),
              Mean = round(mean(.data$value, na.rm = TRUE), display_round),
              SD = round(sd(.data$value, na.rm = TRUE), display_round),
              Min = round(min(.data$value, na.rm = TRUE), display_round),
              Max = round(max(.data$value, na.rm = TRUE), display_round),
              Median = round(median(.data$value, na.rm = TRUE), display_round),
              Q1 = round(quantile(.data$value, 0.25, na.rm = TRUE), display_round),
              Q3 = round(quantile(.data$value, 0.75, na.rm = TRUE), display_round),
              IQR = round(IQR(.data$value, na.rm = TRUE), display_round),
              NMiss = sum(is.na(.data$value)),
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
           mutate(p.value = ifelse(row_number() == 1, .data$p.value, "")),
         ~ select(., everything())) %>%
    ungroup() %>%
    # Arrange display results
    mutate(variable = factor(.data$variable, levels = var_order)) %>%
    arrange(.data$variable)

}
