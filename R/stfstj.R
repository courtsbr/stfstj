#' \code{stfstj} package
#'
#' Baixar dados do STF e do STJ
#'
#'
#' @docType package
#' @name stfstj
#' @importFrom dplyr %>%
#' @importFrom purrr %||%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))