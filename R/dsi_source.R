

#' Create sources for dsinfo
#'
#' A source can be a file system path or a person with an email address.
#'
#' `dsi_source()` is a constructor for the component objects that make up a
#'
#' @param title Title of the source
#' @param path Path to the source
#' @param email Email address of the source
#' @param date Date of the source
#'
#' @return  `dsi_source()` returns a `dsinfo_source` object.
#' @export
#'
#' @examples
#'
#' person <- dsi_source("blubb", "blah@@blubb.at")
#' files  <- dsi_sources_from_paths(c(tempfile(), tempfile(), tempfile()))
#'
#' src <- dsi_sources(person, files)
#'
#' x <- set_dsinfo(
#'   iris,
#'   title = "Iris",
#'   sources = src
#' )
#'
#' dsinfo(x)
#'
dsi_source <- function(title, path = NULL, email = NULL, date = NULL){
  res <- list(title = title, path = path, email = email, date = date)
  attr(res, "class") <- c("dsinfo_source", "list")
  res
}




#' `dsi_sources()` and `dsi_sources_list()` are constructors for objects that
#' can be used for the `source` parameter of [dsinfo()] (a list of sources)
#'
#' @param ... `dsinfo_source` or other `dsinfo_sources` objects.
#' @export
#' @return  `dsi_sources()`, `dsi_sources_list()`, and `dsi_sources_from_paths()`
#'   return a `dsinfo_sources` object, which is a list of `dsinfo_source`
#'   objects.
#' @rdname dsi_source
#'
dsi_sources <- function(...){
  dsi_sources_list(list(...))
}




#' @param sources a list of `dsinfo_source` objects.
#' @export
#'
#' @rdname dsi_source
#'
dsi_sources_list <- function(sources){

  sel1 <- vapply(sources, is_dsinfo_source, FALSE)
  sel2 <- vapply(sources, is_dsinfo_sources, FALSE)
  stopifnot(all(sel1 | sel2))

  res <- c(sources[sel1], unlist(sources[sel2], recursive = FALSE))

  attr(res, "class") <- c("dsinfo_sources", "list")
  res
}




#' `dsi_sources_from_paths()` is a helper function to automatically creates a
#' `dsi_sources` object from file system paths.
#'
#' @export
#' @param paths `character` vector of file system paths
#' @return `dsi_sources_from_paths` returns a `dsi_sources` object.
#' @rdname dsi_source
#'
#'
dsi_sources_from_paths <- function(paths){
  dsi_sources_list(
    lapply(paths, function(x){
      dsi_source(title = basename(x), path = x, date = file.info(x)$mtime)
    })
  )
}




# printing ----------------------------------------------------------------

#' @export
print.dsinfo_source <- function(x, ...){
  cat(format(x), sep = "\n")
  invisible(x)
}




#' @export
print.dsinfo_sources <- function(x, ...){
  cat(format(x), sep = "\n")
  invisible(x)
}




#' @export
format.dsinfo_sources <- function(x, ...){
  unlist(lapply(x, format))
}




#' @export
format.dsinfo_source <- function(
  x,
  ...
){
  if (!is.null(x$date)){
    title <- paste(
      x$title,
      style_subtle(paste0("(", x$date, ")"))
    )
  } else {
    title <- x$title
  }

  paths  <- vapply(
    x$path,
    function(x.) style_subtle(paste("  -", x.)),
    ""
  )

  emails <- vapply(
    x$email,
    function(x.) style_subtle(paste("  -", x.)),
    ""
  )

  if (!is_empty(emails)) emails <- paste(emails)

  c(title, paths, emails)
}




is_dsinfo_source <- function(x){
  inherits(x, "dsinfo_source")
}




is_dsinfo_sources <- function(x){
  inherits(x, "dsinfo_sources")
}
