
#' @return `reference_date()` retrieves the `reference_date` field of the
#'   `dsinfo` attribute of `x` (or `NULL` if no such attribute exists)
#'
#' @rdname dsinfo
#' @export
#'
reference_date <- function(x){
  if(is.null(attr(x, 'dsinfo'))){
    return(NULL)
  } else {
    attr(x, 'dsinfo')$reference_date
  }
}




#' @param value Value to assign.
#'
#' @rdname dsinfo
#' @export
`reference_date<-` <- function(x, value){
  stopifnot(is_reference_date(value))

  dsi <- hammr::dsinfo(x)

  if (inherits(dsi, "dsinfo")){
    dsi$reference_date <- value
    attr(x, "dsinfo") <- dsi
  } else {
    x <- set_dsinfo(x, reference_date = value)
  }

  x
}




#' @param y integer (year) or a `date_xx` object.
#' @param q,m integer. Quarter, month. Month and quarter are optional,
#'  and mutually exclusive (only supply one, not both). If `y` is a `date_xx`
#'  `q` and `m` must be `NULL`.
#'
#' @return `set_reference_date()` and `'reference_date<-'` can be used to
#'   directlty set the `reference_date` field of the `dsinfo` attribute of
#'   an R object.
#'
#' @rdname dsinfo
#' @export
#'
set_reference_date <- function(x, y, q = NULL, m = NULL){
  if(is_date_xx(y)){
    stopifnot(is.null(q) && is.null(m))
    value <- y
  } else {
    value <- make_date_xx(y, q, m)
  }

  x <- set_dsinfo(x, reference_date = value)
}




#' @return `has_reference_date()` returns `TRUE` if `x` has a valid
#'   `reference_date`, and `FALSE` otherwise
#'
#' @rdname dsinfo
#' @export
#'
has_reference_date <- function(x){
  hammr::is_date_xx(reference_date(x))
}




is_reference_date <- function(x){
  inherits(x, c('Date', 'POSIXt', 'Interval', 'date_xx'))
}
