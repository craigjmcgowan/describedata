#' Create table illustrating sample exclusions 
#'
#' Generate a table illustrating sequential exclusion from an analytical sample
#'   due to missing values.
#'
#' @param df A data.frame or tibble.
#' @param na_vars An ordered character vector containing variables that should
#'   excluded due to having missing values (\code{NA}, \code{NaN}). Exclusions
#'   occur in the order specified.
#'
#' @import dplyr
#' @import tidyr
#' @export
#' @return A data.frame with columns variable, 'Sequential Missing', and 
#'   'Total Missing' for display.
#'


sample_flow <- function(df, na_vars) {
  
  # Determine n missing for each variable
  nmiss <- df %>%
    select(one_of(na_vars)) %>%
    gather(key = "Variable", value = "value") %>%
    group_by(Variable) %>%
    summarize(`Total Missing` = sum(is.na(value))) %>%
    ungroup() %>%
    # Set variable as factor for ordering
    mutate(Variable = factor(Variable, levels = na_vars)) %>%
    arrange(Variable)
 

  # Determine sequential missing for each variable
  seq_miss_df <- df
  seq_miss <- tibble()

  for (i in 1:length(na_vars)) {
    quo_var <- rlang::sym(na_vars[i])
    
    seq_miss <- seq_miss_df %>%
      select(one_of(na_vars[i])) %>%
      summarize(Variable = na_vars[i],
                N = sum(!is.na(!!quo_var)),
                `Sequential Missing` = sum(is.na(!!quo_var))) %>%
      bind_rows(seq_miss)
    
    seq_miss_df <- seq_miss_df %>%
      filter(!is.na(!!quo_var))
  }
  
  seq_miss <- mutate(seq_miss, Variable = factor(Variable, levels = na_vars)) %>%
    arrange(Variable)
  
  left_join(seq_miss, nmiss, by = "Variable")
}
