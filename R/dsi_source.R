#' Create sources for dsinfo
#'
#' A source can (for example) be a file system path or a person with an email
#' address. `dsi_source()` is a constructor for a signle `dsinfo_source`, while
#' `dsi_sources()` is a list of such sources. In addition to the recommendations
#' of the [data package](https://frictionlessdata.io/data-package/) standard
#' `dsinfo` sources can also have a `date`.
#'
#' @param title `character` scalar. Title of the source
#' @param path `character` vector. Path(s) to the source
#' @param email `character` vector. Email address(es) of the source
#' @param date vector. Date of the source; usually [POSIXct] or [Date] but any
#'   atomic \R vector is accepted.
#'
#' @return  `dsi_source()` returns a `dsinfo_source` object.
#' @export
#'
#' @seealso [sources()] for an alternative way to get and set sources
#' @examples
#' person <- dsi_source("blubb", "blah@@blubb.at")
#' files  <- dsi_sources_from_paths(c(tempfile(), tempfile(), tempfile()))
#' src    <- dsi_sources(person, files)
#'
#' x <- set_dsinfo(
#'   iris,
#'   title = "Iris",
#'   sources = src
#' )
#'
#' dsinfo(x)
dsi_source <- function(title, path = NULL, email = NULL, date = NULL){
  assert(is_scalar_character(title))
  assert(is.null(path)  || is_scalar_character(path))
  assert(is.null(email) || is.character(email))
  assert(is.null(date)  || is.atomic(date))

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
#' @rdname dsi_source
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
#' @param paths `character` vector of file system paths
#' @return `dsi_sources_from_paths` returns a `dsi_sources` object.
#' @rdname dsi_source
#' @export
dsi_sources_from_paths <- function(paths, email = NULL){
  dsi_sources_list(
    lapply(paths, function(x){
      dsi_source(title = basename(x), path = x, date = file.info(x)$mtime, email = email)
    })
  )
}




#' Get or set dsinfo sources of R objects
#'
#' `sources()` and `sources<-()` provide a direct way to get and set the
#' `$sources` field of a `dsinfo` attribute.
#'
#' @param ... usually \R objects with [dsinfo] attributes, Can also handle
#'   `dsinfo` objects, [dsi_sources], or [dsi_source] objects directly.
#' @param list a `list` of \R objects like `...`. Will be combined
#'   with `...` if both are provided.
#' @param consolidate `logical` scalar. If `TRUE` remove duplicated sources
#'   from the output
#'
#' @seealso [dsi_source()] for more info on `dsinfo_sources` objects.
#' @return a [dsinfo_sources] object
#' @export
#' @examples
#' x <- iris
#' sources(x) <- dsi_source(
#'   "Fisher, R. A.: The use of multiple measurements in taxonomic problems",
#'   path = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'   date = 1936
#' )
#' sources(x)
#' # can also be used directly on dsinfo or dsi_source(s) objects.
#' y <- dsinfo(x)
#' z <- dsi_source("a dsi_source object")
#' sources(x, y, z)
#' sources(x, y, z, consolidate = TRUE)
sources <- function(
  ...,
  list = NULL,
  consolidate = FALSE
){
  assert(is_scalar_bool(consolidate))
  l <- c(list(...), list)

  src <- lapply(l, function(.){
    if (is_dsinfo_sources(.))
      .
    else if (is_dsinfo_source(.)){
      dsi_sources(.)
    } else if (inherits(., "dsinfo")) {
      .$sources
    } else {
      dsinfo(.)$sources
    }
  })

  src <- dsi_sources_list(compact(src))
  if (consolidate)
    consolidate_sources(src)
  else
    src
}




#' @param value Value to assign.
#' @rdname sources
#' @export
`sources<-` <- function(x, value){
  assert(
    is_dsinfo_source(value) || is_dsinfo_sources(value),
    "expected a `dsi_sources` or `dsi_source` objec, not ", preview_object(value)
  )

  dsi <- dsinfo(x)

  if (inherits(dsi, "dsinfo")){
    dsi$sources <- value
    attr(x, "dsinfo") <- dsi
  } else {
    x <- set_dsinfo(x, sources = value)
  }

  x
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
      style_subtle(paste0("(", format(x$date), ")"))
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



# is ----------------------------------------------------------------------

is_dsinfo_source <- function(x){
  inherits(x, "dsinfo_source")
}




is_dsinfo_sources <- function(x){
  inherits(x, "dsinfo_sources")
}




# utils -------------------------------------------------------------------

consolidate_sources <- function(
  x
){
  assert(is_dsinfo_sources(x))

  res <- vector("list", length(x))

  for (i in seq_along(x)){
    for (j in seq_along(x)){
      a <- x[[i]]
      b <- x[[j]]
      if (
        !identical(i, j) &&
        identical(a$title, b$title) &&
        identical(a$path, b$path) &&
        identical(a$email, b$email) &&
        identical(a$date, b$date)
      ){
        x[j] <- list(NULL)
      }
    }
  }

  res <- compact(x)
  res <- res[order(vapply(res, `[[`, character(1), "title"))]
  dsi_sources_list(res)
}
