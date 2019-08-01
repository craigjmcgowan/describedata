#' Create publication-style table across one categorical variable
#'
#' Descriptive statistics for categorical variables as well as normally and
#' non-normally distributed continuous variables, split across levels of
#' a categorical variable. Depending on the variable type, an appropriate
#' statistical test is used to assess differences across levels of the
#' comparison variable.
#'
#' Statistical differences between normally distributed continuous variables
#' are assessed using \code{aov()}, differences in non-normally distributed
#' variables are assessed using \code{kruskal.test()}, and differences in
#' categorical variables are assessed using \code{chisq.test()} by default,
#' with a user option for \code{fisher.test()} instead.
#'
#'
#' @param df A data.frame or tibble.
#' @param compare Discrete variable. Separate statistics will be produced for
#'   each level, with statistical tests across levels. Must be quoted.
#' @param normal_vars Character vector of normally distributed continuous
#'   variables that will be included in the descriptive table.
#' @param non_normal_vars Character vector of non-normally distributed continuous
#'   variables that will be included in the descriptive table.
#' @param cat_vars Character vector of categorical variables that will be
#'   included in the descriptive table.
#' @param display_round Number of decimal places displayed values should be 
#'   rounded to
#' @param p Logical. Should p-values be calculated and displayed?
#'   Default \code{TRUE}.
#' @param p_round Number of decimal places p-values should be rounded to.
#' @param include_na Logical. Should \code{NA} values be included in the
#'   table and accompanying statistical tests? Default \code{FALSE}.
#' @param col_n Logical. Should the total number of observations be displayed
#'   for each column? Default \code{TRUE}.
#' @param cont_n Logical. Display sample n for continuous variables in the
#'   table. Default \code{FALSE}.
#' @param all_cont_mean Logical. Display mean (sd) for all continuous variables.
#'   Default \code{FALSE} results in mean (sd) for normally distributed variables
#'   and median (IQR) for non-normally distributed variables. Must be
#'   \code{FALSE} if \code{all_cont_median == TRUE}.
#' @param all_cont_median Logical. Display median (sd) for all continuous variables.
#'   Default \code{FALSE} results in mean (sd) for normally distributed variables
#'   and median (IQR) for non-normally distributed variables. Must be
#'   \code{FALSE} if \code{all_cont_mean == TRUE}.
#' @param iqr Logical. If the median is displayed for a continuous variable, should
#'   interquartile range be displayed as well (\code{TRUE}), or should the values
#'   for the 25th and 75th percentiles be displayed (\code{FALSE})? Default
#'   \code{TRUE}
#' @param fisher Logical. Should Fisher's exact test be used for categorical
#'   variables? Default \code{FALSE}. Ignored if \code{p == FALSE}.
#' @param workspace Numeric variable indicating the workspace to be used for
#'   Fisher's exact test. If \code{NULL}, the default, the default value of
#'   \code{2e5} is used. Ignored if \code{fisher == FALSE}.
#' @param var_order Character vector listing the variable names in the order
#'   results should be displayed. If \code{NULL}, the default, continuous
#'   variables are displayed first, followed by categorical variables.
#' @param var_label_df A data.frame or tibble with columns "variable" and
#'   "label" that contains display labels for each variable specified in
#'   \code{normal_vars}, \code{non_normal_vars}, and \code{cat_vars}.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import purrr
#' @import broom
#' @import forcats
#' @importFrom stats median sd aov fisher.test chisq.test kruskal.test quantile IQR na.omit
#' @importFrom rlang .data
#' 
#' 
#' @export
#' @return A data.frame with columns label, overall, a column for each level
#'   of \code{compare}, and p.value. For \code{normal_vars}, mean (SD) is
#'   displayed, for \code{non_normal_vars} median (IQR) is displayed, and for
#'   \code{cat_vars} n (percent) is displayed. For p values on continuous
#'   variables, a superscript 'a' denotes the Kruskal-Wallis test was used
#'
#' @examples
#' bivariate_compare(iris, compare = "Species", normal_vars = c("Sepal.Length", "Sepal.Width"))
#'
#' bivariate_compare(mtcars, compare = "cyl", non_normal_vars = "mpg")

bivariate_compare <- function(df, compare, normal_vars = NULL,
                              non_normal_vars = NULL,
                              cat_vars = NULL,
                              display_round = 2,
                              p = TRUE,
                              p_round = 4,
                              include_na = FALSE,
                              col_n = TRUE,
                              cont_n = FALSE,
                              all_cont_mean = FALSE,
                              all_cont_median = FALSE,
                              iqr = TRUE,
                              fisher = FALSE,
                              workspace = NULL,
                              var_order = NULL,
                              var_label_df = NULL) {

  quo_compare <- rlang::sym(compare)

  if (!is.null(var_label_df)) {
    if (!all(names(var_label_df) %in% c("variable", "label")))
      stop("var_label_df must contains columns `variable` and `label`")
  }

  if (isTRUE(all_cont_mean) & isTRUE(all_cont_median))
    stop("only one of all_cont_mean or all_cont_median can be TRUE")

  # Code display format for continuous variables
  if(isTRUE(all_cont_mean)) {
    norm_disp <- non_norm_disp <-"mean"
  } else if(isTRUE(all_cont_median)) {
    if(isTRUE(iqr)) {
      norm_disp <- non_norm_disp <- "median"
    } else {
      norm_disp <- non_norm_disp <- "percentile"
    }
  } else {
    norm_disp <- "mean"
    if(isTRUE(iqr)) {
      non_norm_disp <- "median"
    } else {
      non_norm_disp <- "percentile"
    }
  }

  # Add variable for outcome to hard code in tests later
  # Make categorical variables factors
  df <- df %>%
    mutate(temp_out = factor(!!quo_compare)) %>%
    when(!is.null(cat_vars) ~
           mutate_at(., vars(one_of(cat_vars)), as.factor),
         ~ select(., everything())) %>%
    select(.data$temp_out, one_of(normal_vars, non_normal_vars, cat_vars))


  if (isTRUE(include_na)) {
    df <- df %>%
      mutate(temp_out = forcats::fct_explicit_na(.data$temp_out)) %>%
      when(!is.null(cat_vars) ~
             mutate_at(., vars(one_of(cat_vars)), forcats::fct_explicit_na),
           ~ select(., everything()))
  }

  # Calculate p values
  if(isTRUE(p)) {
    p_values <- tibble() %>%
      # Categorical variables
      when(!is.null(cat_vars) ~ bind_rows(.,
        df %>%
          # Remove missing outcomes for p-value calcs
          filter(.data$temp_out != "(Missing)", !is.na(.data$temp_out)) %>%
          mutate_at(vars(one_of(cat_vars)), as.character) %>%
          select(one_of(cat_vars), .data$temp_out) %>%
          {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
          # Remove missing values for p-value calcs
          filter(value != "(Missing)", !is.na(.data$value)) %>%
          group_by(.data$variable) %>%
          when(
            !isTRUE(fisher) ~ do(., tidy(chisq.test(.$temp_out, .$value))),
            isTRUE(fisher) & is.null(workspace) ~
              do(., tidy(fisher.test(.$temp_out, .$value))),
            ~ do(., tidy(fisher.test(.$temp_out, .$value, workspace = workspace)))
          ) %>%
          ungroup() %>%
          mutate_all(as.character)),
        ~ bind_rows(., tibble())) %>%
      # Non-normal continuous variables
      when(!is.null(non_normal_vars) ~ bind_rows(.,
        df %>%
          # Remove missing outcomes for p-value calcs
          filter(.data$temp_out != "(Missing)", !is.na(.data$temp_out)) %>%
          select(.data$temp_out, one_of(non_normal_vars)) %>%
          {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
          # Remove missing values for p-value calcs
          filter(!is.na(.data$value)) %>%
          group_by(.data$variable) %>%
          do(tidy(kruskal.test(value ~ temp_out,
                               data = .))) %>%
          ungroup() %>%
          mutate_all(as.character)),
        ~ bind_rows(., tibble())) %>%
      # Normal continuous variables
      when(!is.null(normal_vars) ~ bind_rows(.,
        df %>%
          # Remove missing outcomes for p-value calcs
          filter(.data$temp_out != "(Missing)", !is.na(.data$temp_out)) %>%
          select(.data$temp_out, one_of(normal_vars)) %>%
          {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
          # Remove missing values for p-value calcs
          filter(!is.na(.data$value)) %>%
          group_by(.data$variable) %>%
          do(tidy(aov(value ~ temp_out, data = .)) %>%
               filter(grepl("temp_out", term))) %>%
          ungroup() %>%
          mutate_all(as.character)),
        ~ bind_rows(., tibble())) %>%
      select(.data$variable, .data$p.value) %>%
      mutate(p.value = case_when(as.numeric(.data$p.value) <
                                   as.numeric(paste0("1e-", p_round)) ~
                                   paste0("< 0.",
                                          paste0(rep.int(0, p_round-1), collapse = ""),
                                          "1"),
                                 as.numeric(.data$p.value) >=
                                   as.numeric(paste0("1e-", p_round)) ~
                                   format(round(as.numeric(.data$p.value), p_round),
                                          nsmall = p_round),
                                 TRUE ~ NA_character_),
             p.value = case_when(.data$variable %in% non_normal_vars ~
                                   paste0(.data$p.value, "^a^"),
                                 TRUE ~ .data$p.value))
  } else {
    p_values <- tibble(variable = c(cat_vars, normal_vars, non_normal_vars),
                       p.value = rep(" ", length(.data$variable)))
  }

  overall <- tibble() %>%
    # Continuous variable summaries
    when(!is.null(normal_vars) ~ bind_rows(.,
     df %>%
       select(one_of(c(normal_vars))) %>%
       {suppressWarnings(gather(., key = "variable", value = "value"))} %>%
       group_by(.data$variable) %>%
       summarize(mean = mean(.data$value, na.rm = T),
                 sd = sd(.data$value, na.rm = T),
                 median = median(.data$value, na.rm = T),
                 iqr = IQR(.data$value, na.rm = T),
                 q1 = quantile(.data$value, 0.25, na.rm = T),
                 q3 = quantile(.data$value, 0.75, na.rm = T)) %>%
       mutate(display = case_when(norm_disp == "mean" ~
                                    paste0(round(.data$mean, display_round),
                                           " (", round(.data$sd, display_round), ")"),
                                  norm_disp == "median" ~
                                    paste0(round(.data$median, display_round),
                                           " (", round(.data$iqr, display_round), ")"),
                                  norm_disp == "percentile" ~
                                    paste0(round(.data$median, display_round),
                                           " (", round(.data$q1, display_round), ", ",
                                           round(.data$q3, display_round), ")")),
              value = NA_character_) %>%
       select(.data$variable, .data$value, .data$display) %>%
       when(isTRUE(cont_n) ~
              bind_rows(.,
                        df %>%
                          filter(!is.na(.data$temp_out)) %>%
                          select(one_of(c(normal_vars))) %>%
                          {suppressWarnings(gather(., key = "variable", value = "value"))} %>%
                          group_by(.data$variable) %>%
                          summarize(n = n() - sum(is.na(.data$value))) %>%
                          mutate(display = paste0(.data$n),
                                 value = "n") %>%
                          select(.data$variable, .data$value, .data$display)),
            ~ select(., everything()))),
      ~ bind_rows(., tibble())) %>%
    when(!is.null(c(non_normal_vars)) ~ bind_rows(.,
      df %>%
        select(one_of(c(non_normal_vars))) %>%
        {suppressWarnings(gather(., key = "variable", value = "value"))} %>%
        group_by(.data$variable) %>%
        summarize(mean = mean(.data$value, na.rm = T),
                  sd = sd(.data$value, na.rm = T),
                  median = median(.data$value, na.rm = T),
                  iqr = IQR(.data$value, na.rm = T),
                  q1 = quantile(.data$value, 0.25, na.rm = T),
                  q3 = quantile(.data$value, 0.75, na.rm = T)) %>%
        mutate(display = case_when(non_norm_disp == "mean" ~
                                     paste0(round(.data$mean, display_round),
                                            " (", round(.data$sd, display_round), ")"),
                                   non_norm_disp == "median" ~
                                     paste0(round(.data$median, display_round),
                                            " (", round(.data$iqr, display_round), ")"),
                                   non_norm_disp == "percentile" ~
                                     paste0(round(.data$median, display_round),
                                            " (", round(.data$q1, display_round), ", ",
                                            round(.data$q3, display_round), ")")),
                      value = NA_character_) %>%
        select(.data$variable, .data$value, .data$display) %>%
        when(isTRUE(cont_n) ~
               bind_rows(.,
                         df %>%
                           filter(!is.na(.data$temp_out)) %>%
                           select(one_of(c(non_normal_vars))) %>%
                           {suppressWarnings(gather(., key = "variable", value = "value"))} %>%
                           group_by(.data$variable) %>%
                           summarize(n = n() - sum(is.na(.data$value))) %>%
                           mutate(display = paste0(.data$n),
                                  value = "n") %>%
                           select(.data$variable, .data$value, .data$display)),
             ~ select(., everything()))),
      ~ bind_rows(., tibble())) %>%
    # Categorical variable summaries
    when(!is.null(cat_vars) ~ bind_rows(.,
      df %>%
        select(one_of(cat_vars)) %>%
        {suppressWarnings(gather(., key = "variable", value = "value"))} %>%
        # Remove missings - if include_na = TRUE then there should be no NA
        na.omit() %>%
        group_by(.data$variable, .data$value) %>%
        summarize(n = n()) %>%
        mutate(display = paste0(.data$n, " (", round(.data$n / sum(.data$n) * 100), "%)")) %>%
        select(.data$variable, .data$value, .data$display)),
      ~ bind_rows(., tibble())) %>%
    ungroup() %>%
    mutate_all(as.character)

  by_gender <- tibble() %>%
    # Continuous variable summaries
    when(!is.null(c(normal_vars)) ~ bind_rows(.,
      df %>%
        select(.data$temp_out,
               one_of(c(normal_vars))) %>%
        {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
        group_by(.data$temp_out, .data$variable) %>%
        summarize(mean = mean(.data$value, na.rm = T),
                  sd = sd(.data$value, na.rm = T),
                  median = median(.data$value, na.rm = T),
                  iqr = IQR(.data$value, na.rm = T),
                  q1 = quantile(.data$value, 0.25, na.rm = T),
                  q3 = quantile(.data$value, 0.75, na.rm = T)) %>%
        mutate(display = case_when(norm_disp == "mean" ~
                                     paste0(round(.data$mean, display_round),
                                            " (", round(.data$sd, display_round), ")"),
                                   norm_disp == "median" ~
                                     paste0(round(.data$median, display_round),
                                            " (", round(.data$iqr, display_round), ")"),
                                   norm_disp == "percentile" ~
                                     paste0(round(.data$median, display_round),
                                            " (", round(.data$q1, display_round), ", ",
                                            round(.data$q3, display_round), ")")),
               value = NA_character_) %>%
        select(.data$temp_out, .data$variable, .data$value, .data$display) %>%
        when(isTRUE(cont_n) ~
               bind_rows(.,
                         df %>%
                           filter(!is.na(.data$temp_out)) %>%
                           select(.data$temp_out,
                                  one_of(c(normal_vars))) %>%
                           {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
                           group_by(.data$temp_out, .data$variable) %>%
                           summarize(n = n() - sum(is.na(.data$value))) %>%
                           mutate(display = paste0(.data$n),
                                  value = "n") %>%
                           select(.data$temp_out, .data$variable, .data$value, .data$display)),
             ~ select(., everything())) %>%
        spread(key = temp_out, value = "display")),
      ~ bind_rows(., tibble())) %>%
    when(!is.null(c(non_normal_vars)) ~ bind_rows(.,
       df %>%
         select(.data$temp_out,
                one_of(c(non_normal_vars))) %>%
         {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
         group_by(.data$temp_out, .data$variable)%>%
         summarize(mean = mean(.data$value, na.rm = T),
                   sd = sd(.data$value, na.rm = T),
                   median = median(.data$value, na.rm = T),
                   iqr = IQR(.data$value, na.rm = T),
                   q1 = quantile(.data$value, 0.25, na.rm = T),
                   q3 = quantile(.data$value, 0.75, na.rm = T)) %>%
         mutate(display = case_when(non_norm_disp == "mean" ~
                                      paste0(round(.data$mean, display_round),
                                             " (", round(.data$sd, display_round), ")"),
                                    non_norm_disp == "median" ~
                                      paste0(round(.data$median, display_round),
                                             " (", round(.data$iqr, display_round), ")"),
                                    non_norm_disp == "percentile" ~
                                      paste0(round(.data$median, display_round),
                                             " (", round(.data$q1, display_round), ", ",
                                             round(.data$q3, display_round), ")")),
                value = NA_character_) %>%
         select(.data$temp_out, .data$variable, .data$value, .data$display) %>%
         when(isTRUE(cont_n) ~
                bind_rows(.,
                          df %>%
                            filter(!is.na(.data$temp_out)) %>%
                            select(.data$temp_out,
                                   one_of(c(non_normal_vars))) %>%
                            {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
                            group_by(.data$temp_out, .data$variable) %>%
                            summarize(n = n() - sum(is.na(.data$value))) %>%
                            mutate(display = paste0(.data$n),
                                   value = "n") %>%
                            select(.data$temp_out, .data$variable, .data$value, .data$display)),
              ~ select(., everything())) %>%
         spread(key = temp_out, value = "display")),
         ~ bind_rows(., tibble())) %>%
    # Categorical variable summaries
    when(!is.null(cat_vars) ~ bind_rows(.,
      df %>%
        select(one_of(cat_vars), .data$temp_out) %>%
        {suppressWarnings(gather(., key = "variable", value = "value", -temp_out))} %>%
        na.omit() %>%
        group_by(.data$temp_out, .data$variable, .data$value) %>%
        summarize(n = n()) %>%
        mutate(display = paste0(.data$n, " (", round(.data$n / sum(.data$n) * 100), "%)")) %>%
        select(.data$temp_out, .data$variable, .data$value, .data$display) %>%
        spread(key = temp_out, value = "display", fill = "0 (0%)")),
      ~ bind_rows(., tibble()))  %>%
    ungroup() %>%
    mutate_all(as.character) %>%
    left_join(p_values, by = "variable")

  # Order for displaying results
  if(is.null(var_order)) var_order <- c(non_normal_vars, normal_vars, cat_vars)

  if(length(var_order) != length(c(non_normal_vars, normal_vars, cat_vars))) {
    stop(paste(length(var_order), "variables in var_order, but stats calculated for",
               length(c(non_normal_vars, normal_vars, cat_vars)),
               "variables.\nCheck var_order that all variables included."))
  }
  if(!all(var_order %in% c(non_normal_vars, normal_vars, cat_vars))) {
    stop(paste("The following variables in var_order need to be included in one of",
               "normal_vars, non_normal_vars, or cat_vars:\n",
               paste(var_order[!var_order %in% c(non_normal_vars, normal_vars, cat_vars)],
                     collapse = ",")))
  }

  # Display results
  display_temp <- overall %>%
    left_join(by_gender, by = c("variable", "value"))

  # Build display dataset in order - use bind_rows to keep factor levels in order
  display <- tibble()

  for(i in seq_along(var_order)) {
    display <- bind_rows(
      display,
      when(var_order[i],
           . %in% cat_vars ~
             filter(display_temp, variable == .) %>%
             mutate(value = factor(.data$value,
                                   levels = levels(pull(df, var_order[i])))) %>%
             arrange(.data$value) %>%
             mutate_all(as.character),
           . %in% c(normal_vars, non_normal_vars) ~
             filter(display_temp, variable == var_order[i]),
           ~ tibble()) # %>%  # Add blank line at top for variable name
             # bind_rows(tibble(variable = var_order[i]), .))
    )
  }

  # Remove duplicate values for final printing - make it pretty.
  display <- display %>%
    group_by(.data$variable) %>%
    mutate(., p.value = ifelse(row_number(.data$variable) == 1,
                               .data$p.value, " ")) %>%
    # Add variable labels if provided, otherwise return variable name
    when(!is.null(var_label_df) ~
           left_join(., var_label_df, by = c("variable")) %>%
           mutate(label = ifelse(row_number(.data$variable) == 1, label, " "),
                  label = case_when(!is.na(.data$value) ~ paste(.data$label, .data$value, sep = " - "),
                                    is.na(.data$value) ~ paste0(.data$label),
                                    TRUE ~ NA_character_)) %>%
           ungroup() %>%
           select(.data$label, "Overall" = display, one_of(levels(df$temp_out)), p.value),
         ~ mutate(., var = ifelse(row_number(.data$variable) == 1,
                                 as.character(.data$variable), " "),
                    var = case_when(!is.na(.data$value) ~ paste(.data$var, .data$value, sep = " - "),
                                    is.na(.data$value) ~ paste0(.data$var),
                                    TRUE ~ NA_character_)) %>%
           ungroup() %>%
           select(.data$var, "Overall" = display,
                  one_of(levels(df$temp_out)), p.value)) %>%
    # Remove p.value if not requested
    when(isTRUE(p) ~ select(., everything()),
         ~ select(., -p.value))


  # Add total N to column headers
  if (isTRUE(col_n)){
    n <- df %>%
      group_by(.data$temp_out) %>%
      summarize(n = n()) %>%
      filter(!is.na(.data$temp_out)) %>%
      arrange(.data$temp_out)

    display <- display %>%
      rename_at("Overall", function(x) paste0(x, " (n = ", sum(n$n), ")")) %>%
      rename_at(levels(df$temp_out), function(x) paste0(x, " (n = ", n$n, ")"))
  }

  return(display)
}
