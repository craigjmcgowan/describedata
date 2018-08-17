#' Calculate Nagelkerke pseudo r-squared
#'
#' Calculate Nagelkerke pseudo r-squared from a fitted model object.
#'
#' @param mod A \code{glm} model object, usually from logistic regression.
#'   The model must have been fit using the \code{data} option, in order to
#'   extract the data from the model object.
#'
#' @export
#' @return Numeric value of Nagelkerke r-squared for the model
#'
#'

nagelkerke <- function(mod) {

  # Create dataset original model was fit on
  null_data <- mod$data[, names(mod$data) %in% paste(attr(terms(mod), "variables"))]
  null_data <- na.omit(null_data)

  # Create null model
  null <- update(mod, . ~ 1, data = null_data)

  # Return Nagelkerke Pseudo R-squared
  lr.stat <- lmtest::lrtest(null, mod)

  (1 - exp(-as.numeric(lr.stat$Chisq[2]) / nobs(mod))) /
    (1-exp(2*as.numeric(logLik(null) / nobs(mod))))

}
