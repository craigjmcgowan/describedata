#' Create publication-style table across one categorical variable
#'
#' Descriptive statistics for categorical variables as well as normally and
#' non-normally distributed continiuous variables, split across levels of
#' a categorical variable. Depending on the variable type, an appropriate
#' statistical test is used to assess differences across levels of the
#' comparison variable.
#'
#' @param df A data.frame or tibble.
#' @param compare Discrete variable. Separate statistics will be produced for
#'   each level, with statistical tests across levels.
#' @param normal_vars Normally distributed continuous variables that will be
#'   included in the descriptive table.
#' @param non_normal_vars Non-normally distributed continuous variables that
#'   will be included in the descriptive table.
#' @param cat_vars Categorical variables that will be included in the
#'   descriptive table.
#' @param include_na Logical. Should \code{NA} values be included in the
#'   table and accompanying statistical tests? Default \code{FALSE}.
#' @param fisher Logical. Should Fisher's exact test be used for categorical
#'   variables? Default \code{FALSE}.
#' @param workspace Numeric variable indicating the workspace to be used for
#'   Fisher's exact test. If \code{NULL}, the default, the default value of
#'   \code{2e5} is used.
#' @param var_label_df A data.frame or tibble with columns "variable" and
#'   "label" that contains display labels for each variable specified in
#'   \code{normal_vars}, \code{non_normal_vars}, and \code{cat_vars}.
#'
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import broom
#' @export
#' @return A data.frame with columns label, overall, a column for each level
#'   of \code{compare}, and p.value. For \code{normal_vars}, mean (SD) is
#'   displayed, for \code{non_normal_vars} median (IQR) is displayed, and for
#'   \code{cat_vars} n (%) is displayed.
#'
#' @examples
#' bivariate_compare(iris, compare = Species, normal_vars = c("Sepal.Length", "Sepal.Width"))
#'
#' bivariate_compare(mtcars, compare = cyl, non_normal_vars = "mpg")

bivariate_compare <- function(df, compare, normal_vars = NULL,
                              non_normal_vars = NULL,
                              cat_vars = NULL, fisher = FALSE,
                              workspace = NULL,
                              var_label_df = NULL,
                              include_na = FALSE) {

  quo_compare <- rlang::sym(compare)

  if (!is.null(var_label_df)) {
    if (!names(var_label_df) %in% c("variable", "label"))
      stop("var_label_df must contains columns `variable` and `label`")
  }

  # Add variable for outcome to hard code in tests later
  # Make categorical variables factors
  df <- df %>%
    mutate(temp_out = factor(!!quo_compare)) %>%
    when(!is.null(cat_vars) ~
           mutate_at(., vars(one_of(cat_vars)), as.factor),
         ~ select(., everything())) %>%
    select(temp_out, one_of(normal_vars, non_normal_vars, cat_vars))


  if (isTRUE(include_na)) {
    df <- df %>%
      mutate(temp_out = fct_explicit_na(temp_out)) %>%
      when(!is.null(cat_vars) ~
             mutate_at(., vars(one_of(cat_vars)), fct_explicit_na),
           ~ select(., everything()))
  }

  # Store categorical levels for later
  if(!is.null(cat_vars)) lev <- map(df %>% select(one_of(cat_vars)), levels)

  # Calculate p values
  p_values <- tibble() %>%
    # Categorical variables
    when(!is.null(cat_vars) ~ bind_rows(.,
      df %>%
        # Remove missing outcomes for p-value calcs
        filter(temp_out != "(Missing)", !is.na(temp_out)) %>%
        mutate_at(vars(one_of(cat_vars)), as.character) %>%
        select(one_of(cat_vars), temp_out) %>%
        gather(key = "variable", value = "value", -temp_out) %>%
        # Remove missing values for p-value calcs
        filter(value != "(Missing)", !is.na(value)) %>%
        group_by(variable) %>%
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
        filter(temp_out != "(Missing)", !is.na(temp_out)) %>%
        select(temp_out, one_of(non_normal_vars)) %>%
        gather(key = "variable", value = "value", -temp_out) %>%
        # Remove missing values for p-value calcs
        filter(!is.na(value)) %>%
        group_by(variable) %>%
        do(tidy(kruskal.test(value ~ temp_out,
                             data = .))) %>%
        ungroup() %>%
        mutate_all(as.character)),
      ~ bind_rows(., tibble())) %>%
    # Normal continuous variables
    when(!is.null(normal_vars) ~ bind_rows(.,
      df %>%
        # Remove missing outcomes for p-value calcs
        filter(temp_out != "(Missing)", !is.na(temp_out)) %>%
        select(temp_out, one_of(normal_vars)) %>%
        gather(key = "variable", value = "value", -temp_out) %>%
        # Remove missing values for p-value calcs
        filter(!is.na(value)) %>%
        group_by(variable) %>%
        do(tidy(lm(value ~ temp_out, data = .)) %>%
             filter(grepl("temp_out", term))) %>%
        ungroup() %>%
        mutate_all(as.character)),
      ~ bind_rows(., tibble())) %>%
    select(variable, p.value) %>%
    mutate(p.value = case_when(as.numeric(p.value) < 1e-4 ~ "< 0.0001",
                               as.numeric(p.value) >= 1e-4 ~
                                 format(round(as.numeric(p.value), 4), nsmall = 4),
                               TRUE ~ NA_character_),
           p.value = case_when(variable %in% non_normal_vars ~
                                 paste0(p.value, "^a^"),
                               TRUE ~ p.value))

  overall <- tibble() %>%
    # Continuous variable summaries
    when(!is.null(c(normal_vars, non_normal_vars)) ~ bind_rows(.,
      df %>%
        select(one_of(c(normal_vars, non_normal_vars))) %>%
        gather(key = "variable", value = "value") %>%
        group_by(variable) %>%
        summarize(n = n() - sum(is.na(value)),
                  nmiss = sum(is.na(value)),
                  median = median(value, na.rm = T),
                  iqr = IQR(value, na.rm = T)) %>%
        mutate(n_display = paste0(n, " (", nmiss, ")"),
               display = paste0(round(median, 2),
                                " (", round(iqr, 2),
                                ")"),
               value = NA_character_) %>%
        select(variable, value, n_display, display)),
      ~ bind_rows(., tibble())) %>%
    # Categorical variable summaries
    when(!is.null(cat_vars) ~ bind_rows(.,
      df %>%
        select(one_of(cat_vars)) %>%
        # Remove missings - if include_na = TRUE then there should be no NA
        na.omit() %>%
        gather(key = "variable", value = "value") %>%
        group_by(variable, value) %>%
        summarize(n = n()) %>%
        mutate(display = paste0(n, " (", round(n / sum(n) * 100), "%)")) %>%
        select(variable, value, display)),
      ~ bind_rows(., tibble())) %>%
    ungroup() %>%
    mutate_all(as.character)

  by_gender <- tibble() %>%
    # Continuous variable summaries
    when(!is.null(c(normal_vars, non_normal_vars)) ~ bind_rows(.,
      df %>%
        select(temp_out,
               one_of(c(normal_vars, non_normal_vars))) %>%
        gather(key = "variable", value = "value", -temp_out) %>%
        group_by(temp_out, variable) %>%
        summarize(median = median(value, na.rm = T),
                  iqr = IQR(value, na.rm = T)) %>%
        mutate(display = paste0(round(median, 2),
                                " (", round(iqr, 2),
                                ")"),
               value = NA_character_) %>%
        select(temp_out, variable, value, display) %>%
        spread(key = temp_out, value = "display")),
      ~ bind_rows(., tibble())) %>%
    # Categorical variable summaries
    when(!is.null(cat_vars) ~ bind_rows(.,
      df %>%
        select(one_of(cat_vars), temp_out) %>%
        na.omit() %>%
        gather(key = "variable", value = "value", -temp_out) %>%
        group_by(temp_out, variable, value) %>%
        summarize(n = n()) %>%
        mutate(display = paste0(n, " (", round(n / sum(n) * 100), "%)")) %>%
        select(temp_out, variable, value, display) %>%
        spread(key = temp_out, value = "display", fill = "0 (0%)")),
      ~ bind_rows(., tibble()))  %>%
    ungroup() %>%
    mutate_all(as.character) %>%
    left_join(p_values, by = "variable")

  # Order for displaying results
  var_order <- c(non_normal_vars, normal_vars, cat_vars)
  if (!is.null(cat_vars)) {
    value_order <- c(rep(NA, length(non_normal_vars) + length(normal_vars)),
                     unname(unlist(lev)))
  } else {
    value_order <- c(rep(NA, length(non_normal_vars) + length(normal_vars)))
  }

  # Display results
  overall %>%
    left_join(by_gender, by = c("variable", "value")) %>%
    mutate(value = ifelse(is.na(value) & variable %in% cat_vars,
                          "(Missing)", value),
           variable = factor(variable, levels = var_order),
           value = factor(value, levels = unique(value_order,
                                                 fromLast = TRUE))) %>%
    arrange(variable, value) %>%
    # Return variable to being character for joining
    mutate(variable = as.character(variable)) %>%
    group_by(variable) %>%
    mutate(p.value = ifelse(row_number(variable) == 1,
                            p.value, " ")) %>%
    # Add variable labels if provided, otherwise return variable name
    when(!is.null(var_label_df) ~
           left_join(., var_label_df, by = c("variable")) %>%
           mutate(label = ifelse(row_number(variable) == 1, label, " "),
                  label = case_when(!is.na(value) ~ paste(label, value, sep = " - "),
                                    is.na(value) ~ paste0(label),
                                    TRUE ~ NA_character_)) %>%
           ungroup() %>%
           select(label, "Overall" = display, one_of(levels(df$temp_out)), p.value),
         ~ mutate(., var = ifelse(row_number(variable) == 1,
                                 as.character(variable), " "),
                    var = case_when(!is.na(value) ~ paste(var, value, sep = " - "),
                                    is.na(value) ~ paste0(var),
                                    TRUE ~ NA_character_)) %>%
           ungroup() %>%
           select(var, "Overall" = display,
                  one_of(levels(df$temp_out)), p.value))
}
