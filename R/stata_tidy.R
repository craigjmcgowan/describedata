#' Tidy model output into similar format from Stata
#'
#' Create a display data frame similar to Stata model output for a fitted
#'   R model.
#'
#' @param mod A fitted model object
#' @param var_label_df A data.frame or tibble with columns "variable" and
#'   "label" that contains display labels for each variable in \code{mod}.
#'
#' @import broom
#' @import dplyr
#' @import broom
#' @import stringr
#' @export
#' @return A data.frame with columns term and display
#'
#'

stata_tidy <- function(mod, var_label_df = NULL) {
  tidy(mod) %>%
    mutate(display = case_when(p.value < 0.01 ~ 
                                 paste0(round(estimate, 3), "***\n(",
                                        round(std.error, 3), ")"),
                               p.value < 0.05 ~ 
                                 paste0(round(estimate, 3), "**\n(",
                                        round(std.error, 3), ")"),
                               p.value < 0.1 ~ 
                                 paste0(round(estimate, 3), "*\n(",
                                        round(std.error, 3), ")"),
                               TRUE ~ paste0(round(estimate, 3), "\n(",
                                             round(std.error, 3), ")"))) %>%
    # Add variable labels if provided
    when(
      !is.null(var_label_df) ~ 
        mutate(., variable = str_extract(term, 
                                 paste(var_label_df$variable, collapse = "|")))%>%
        left_join(var_label_df, by = "variable") %>%
        mutate(term = str_replace(term,
                                  paste(var_label_df$variable, collapse = "|"),
                                  paste0(label, ": "))) %>%
        select(term, display),
      ~ select(., term, display)
    )
}


