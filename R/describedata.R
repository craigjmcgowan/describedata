#' describedata: Miscellaneous descriptive and SAS/Stata duplicate functions
#'
#' The helpR package contains descriptive functions for tasks such as making
#'  print-friendly bivariate tables, sample size flow counts, and more. It
#'  also contains R approximations of some common, useful SAS/Stata functions.
#'
#' @section Frequency functions:
#' The helper functions \code{\link{bivariate_compare}} and
#'   \code{\link{univar_freq}} create frequency tables. \code{\link{univar_freq}}
#'   produces simple n and percent for categories of a single variable,
#'   while \code{\link{bivariate_compare}} compares continuous or categorical
#'   variables across categories of a comparison variable. This is particularly
#'   useful for generating a Table 1 or 2 for a publication manuscript.
#'
#' @section Sample size functions:
#' \code{\link{sample_flow}} produces tables illustrating how final sample
#'   size is determined and the number of participants excluded by each
#'   exclusion criteria.
#'
#' @section Other helper functions:
#' \code{\link{nagelkerke}} calculates the Nagelkerke pseudo r-squared for a
#'   logistic regression model.
#'
#' @section Stata replica functions:
#' \code{\link{ladder}}, \code{\link{gladder}}, and \code{\link{pwcorr}} are
#'   approximate replicas of the respective Stata functions. Not all
#'   functionality is currently incorporated. \code{\link{stata_tidy}}
#'   reformats R model output to a format similar to Stata.
#'
#' @section SAS replica functions:
#' \code{\link{proc_means}} is an approximate replica of the respective SAS
#'   function. Not all functionality is currently incorporated.
#'
#'
#' @import dplyr
#' @import tidyr
#' @import broom
#' @import haven
#' @import stringr
#' @import lmtest
#'
#' @docType package
#' @name describedata
NULL
