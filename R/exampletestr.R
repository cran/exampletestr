#' @importFrom magrittr '%>%' '%T>%' '%<>%'
#' @importFrom roxygen2 roclet
#' @importFrom utils globalVariables
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  globalVariables(c(".", "file.edit"))
}
