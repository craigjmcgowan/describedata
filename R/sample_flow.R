#' Create table illustrating sample exclusions
#'
#' Generate a table illustrating sequential exclusion from an analytical sample
#'   due to user specified exclusions.
#'
#' @param df A data.frame or tibble.
#' @param exclusions Character vector of logical conditions indicating which rows
#'   should be excluded from the final sample. Exclusions occur in the order
#'   specified.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @return A data.frame with columns Exclusion, 'Sequential Excluded', and
#'   'Total Excluded' for display.
#'


sample_flow <- function(df, exclusions = c()) {

  # if(length(exclusions) > 0 & length(inclusions) > 0)
  #   stop("specify only inclusion or exclusion criteria")

  seq_excl_df <- df
  seq_excl <- tibble()
  all_excl <- tibble()

  for (i in seq_along(exclusions)) {

    # Total excluded due to this exclusion
    all_excl <- bind_rows(
      all_excl,
      tibble(Exclusion = exclusions[i],
             `Total Excluded` = nrow(filter(df, eval(parse(text = exclusions[i])))))
    )

    # Sequential exclusions
    seq_excl <- bind_rows(
      seq_excl,
      tibble(Exclusion = exclusions[i],
             N = nrow(filter(seq_excl_df, !eval(parse(text = exclusions[i])))),
             `Sequential Excluded` = nrow(seq_excl_df) - .data$N)
    )

    # Update exclusion df for next iteration
    seq_excl_df <-
      filter(seq_excl_df, !eval(parse(text = exclusions[i])))

  }

  left_join(seq_excl, all_excl, by = "Exclusion") %>%
    mutate(Exclusion = factor(.data$Exclusion, levels = exclusions)) %>%
    arrange(.data$Exclusion)

}
