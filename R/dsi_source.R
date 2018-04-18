

#' Create sources for dsinfo
#'
#'
#' `dsi_source()` is a constructor for the component objects that make up a
#' `dsi_sources()` object (a single source file or person).
#'
#' @param title Title of the source
#' @param path Path to the source
#' @param email Email address of the source
#' @param date Date of the source
#'
#' @return  `dsi_source()` returns a `dsinfo_source` object.
#' @export
#'
dsi_source <- function(title, path = NULL, email = NULL, date = NULL){
  res <- list(title = title, path = path, email = email, date = date)
  attr(res, "class") <- c("dsinfo_source", "list")
  res
}




#' `dsi_sources()` and `dsi_sources_list()` are constructors for objects that
#' can be used for the `source` parameter of [dsinfo()] (a list of sources)
#'
#' @param ... `dsinfo_source` objects.
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
  stopifnot(all(
    vapply(sources, function(x) inherits(x, "dsinfo_source"), FALSE)
  ))
  attr(sources, "class") <- c("dsinfo_sources", "list")
  sources
}




#' `dsi_sources_from_paths()` is a helper function to automatically creates a
#' `dsi_sources` object from file system paths.
#'
#' @export
#' @param paths `character` vector of file system paths
#' @return `dsi_sources_from_paths` returns a `dsi_sources` object.
#' @rdname dsi_source
#'
#' @examples
#' x <- set_dsinfo(
#'   iris,
#'   title = "Iris",
#'   sources = dsi_sources(
#'     dsi_source("R demo data", date = Sys.Date()),
#'     dsi_source("Alfred Bogus", email = "alfred@bogus.xx")
#'   )
#' )
#'
#' dsinfo(x)
#'
dsi_sources_from_paths <- function(paths){
  dsi_sources_list(
    lapply(paths, function(x){
      dsi_source(title = basename(x), path = x, date = file.info(x)$mtime)
    })
  )
}




format_sources <- function(x, indent = "  "){
  lapply(x, format_source) %>%
    unlist() %>%
    paste0(collapse = paste0("\n", indent)) %>%
    paste0("\n", indent, .)
}



format_source <- function(x){

  if (!is.null(x$date)){
    title <- paste(
      x$title,
      colt::clt_chr_subtle(paste0("(", x$date, ")"))
    )
  } else {
    title <- x$title
  }

  paths  <- vapply(x$path,  function(x.) colt::clt_chr_subtle(paste("   -", x.)), "")
  emails <- vapply(x$email, function(x.) colt::clt_chr_subtle(paste("   -", x.)), "")

  if(!isit::is_empty(emails)) emails <- paste(emails)

  c(title, paths, emails)
}
