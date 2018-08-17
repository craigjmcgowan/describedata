#' Calculate pairwise correlations
#'
#' Internal function to calculate pairwise correlations and return p values
#'
#' @param df A data frame or tibble.
#'
#' @import dplyr
#' @import tidyr
#'
#' @return A data.frame with columns h_var, v_var, and p.value
#'


cor.prob <- function(df) {
  # Set degrees of freedome
  dfr <- nrow(df) - 2

  # Determine correlations
  R <- cor(df, use = "pairwise.complete.obs")
  above <- row(R) < col(R)

  # Calculate p values from F test
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)

  cor.mat <- t(R)
  cor.mat[upper.tri(cor.mat)] <- NA
  diag(cor.mat) <- NA

  cor.mat %>%
    as.data.frame() %>%
    rownames_to_column(var = "h_var") %>%
    gather(key = "v_var", value = "p.value", -h_var)
}

#' Replica of Stata's pwcorr function
#'
#' Calculate and return a matrix of pairwise correlation coefficients with
#'   significance levels.
#'
#' @param df A data.frame or tibble.
#' @param vars A character vector of numeric variables to generate pairwise
#'   correlations for. If the default (\code{NULL}), all variables are included.
#' @param var_label_df A data.frame or tibble with columns "variable" and
#'   "label" that contains display labels for each variable specified in
#'   \code{vars}.
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @return A data.frame displaying the pairwise correlation coefficients
#'   between all variables in \code{vars}.
#'

pwcorr <- function(df, vars = NULL, var_label_df = NULL) {

  if (is.null(vars)) vars <- names(df)

  # Variable labels for display
  if (!is.null(var_label_df)) {
    if (!names(var_label_df) %in% c("variable", "label"))
      stop("var_label_df must contains columns `variable` and `label`")
    labels <- var_label_df$label[var_label_df$variable %in% vars]
  } else {
    labels <- vars
  }

  # Restrict data to requested variables
  df <- select(df, one_of(vars))

  cor.matrix <- cor(df, use = "pairwise.complete.obs")
  cor.matrix[upper.tri(cor.matrix)] <- NA

  cor.matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "h_var") %>%
    gather(key = "v_var", value = "corr", -h_var) %>%
    left_join(cor.prob(df), by = c("h_var", "v_var")) %>%
    mutate(p.disp = case_when(p.value < 0.01 ~ "<0.01",
                               is.na(p.value) ~ NA_character_,
                               TRUE ~ paste(round(p.value, 2))),
           display = case_when(!is.na(corr) & !is.na(p.disp) ~
                                 paste0(round(corr, 2), "\n(", p.disp, ")"),
                               corr == 1 & is.na(p.disp) ~ "1",
                               is.na(corr) & is.na(p.disp) ~ " ",
                               TRUE ~ NA_character_)) %>%
    select(h_var, v_var, display) %>%
    mutate(h_var = factor(h_var, levels = vars, labels = labels),
           v_var = factor(v_var, levels = vars, labels = labels)) %>%
    arrange(h_var, v_var) %>%
    spread(key = "v_var", value = "display") %>%
    rename(" " = h_var)
}
