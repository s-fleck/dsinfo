#' Remove dsinfo from an R object
#'
#' @param x Any \R object.
#'
#' @return `x` without a `"dsinfo"` attribute
#' @export
#'
strip_dsinfo <- function(x){
  attr(x, "dsinfo") <- NULL
  x
}
