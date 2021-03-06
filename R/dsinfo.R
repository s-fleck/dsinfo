#' Set and Display Info about a Data Set
#'
#' Add metadata inspired by the 'Data Package' standard to any to \R object. A
#' `dsinfo` attribute may contain a `list` of arbitrary elements; however, it
#' is highly recommended to stick to the elements outlined in the *Arguments*
#' section of this help file.
#'
#' `set_dsinfo()` and `dsinfo<-()` can be used to set the `dsinfo` attribute
#' of an \R Object. `update_dsinfo()` is similar but only updates metadata
#' elements that were changed or added. It is equivalent to
#' `set_desinfo(..., .add = TRUE)`. `dsinfo()` can be used to access said
#' attribute.
#'
#'
#' @param x any \R object
#'
#' @param name A short url-usable (and preferably human-readable) name of the
#'   dataset This MUST be lower-case and contain only alphanumeric characters
#'   along with ".", "_" or "-" characters. It will function as a unique
#'   identifier and therefore SHOULD be unique in relation to any registry in
#'   which this dataset will be deposited (and preferably globally unique).
#'
#'   The name SHOULD be invariant, meaning that it SHOULD NOT change when a data
#'   dataset is updated, unless the new dataset version should be considered a
#'   distinct dataset, e.g. due to significant changes in structure or
#'   interpretation. Version distinction SHOULD be left to the version property.
#'   As a corollary, the name also SHOULD NOT include an indication of time
#'   range covered.
#'
#'   **For \R I do not recommend using the name attribute. Assign an S3 class
#'   that fullfills the same criteria instead**
#'
#' @param id A property reserved for globally unique identifiers. Examples of
#'   identifiers that are unique include UUIDs and DOIs.
#'
#'   A common usage pattern for datasets is as a packaging format within
#'   the bounds of a system or platform. In these cases, a unique identifier for
#'   a dataset is desired for common data handling workflows, such as updating
#'   an existing dataset. While at the level of the specification, global
#'   uniqueness cannot be validated, consumers using the id property MUST ensure
#'   identifiers are globally unique.
#' @param license The license(s) under which the dataset is provided.
#'
#' @param title A string providing a title or one sentence description for this dataset
#'
#' @param description a description of the dataset. The description MUST be
#'   markdown formatted -- this also allows for simple plain text as plain text
#'   is itself valid markdown. The first paragraph (up to the first double line
#'   break) should be usable as summary information for the dataset.
#'
#' @param homepage A URL for the home on the web that is related to this dataset.
#'
#' @param version a version string identifying the version of the dataset. It
#'   should conform to the Semantic Versioning requirements. See http://semver.org/
#'
#' @param sources The raw sources for this dataset. It MUST be a list of
#'   Source objects. Each Source object MAY have title, path and email
#'   properties. See [dsi_sources]
#'
#' @param contributors The people or organizations who contributed to this
#'   dataset. It MUST be a list. Each entry is a Contributor and MUST be an
#'   object. A Contributor MUST have a name property and MAY contain path,
#'   email, role and organization properties.
#'
#' @param keywords An character vector of keywords to assist users
#'   searching for the dataset in catalogs.
#'
#' @param created a Datetime scalar
#'
#' @param reference_date Reference date for the data set. May be a [base::Date],
#'   [base::POSIXt], [dint::date_xx] or a [lubridate::period].
#'
#' @param image Provided for compatability with the Data Package standard. An
#'   image to use for this data package. For example, when showing the package
#'   in a listing.
#'
#'   The value of the image property MUST be a string pointing to the location
#'   of the image. The string must be a url-or-path, that is a fully qualified
#'   HTTP address, or a relative POSIX path (see the url-or-path definition in
#'   Data Resource for details).
#'
#' @param profile for compatability with the Data Package standard. A string
#'   identifying the profile of this descriptor as per the profiles
#'   specification. (see Data Package sepcifications)
#'
#' @param ... any number of arbitrary metadata elements that will also be
#'   attached to dsinfo.
#'
#' @param .add if `FALSE` (default), the complete `dsinfo` attribute is
#'   replaced, dropping values that are not present in the new attribute.
#'   If `TRUE`, the new values are added to the existing `dsinfo`
#'   attribute (values that exist in the original and new dsinfo are still
#'   replaced by the new values).
#'
#' @return `dsinfo()` returns the `dsinfo` attribute of `x` (or `NULL` if there
#' is none).
#'
#' @export
#' @import dint
#'
dsinfo <- function(x){
  attr(x, "dsinfo")
}




#' @rdname dsinfo
#' @return `set_dsinfo()`, `update_dsinfo()`, and `set_source_script()` return
#'   `x` with an additional `dsinfo` attribute.
#' @export
#' @examples
#' x <- set_dsinfo(iris, "i001", "iris-dataset", title = "The iris dataset")
#' dsinfo(x)
set_dsinfo <- function(
  x,

  # recommended
  id = NULL,
  name = NULL,
  reference_date = NULL,
  version = NULL,

  # data-package recommended
  license = NULL,

  # data-package optional
  title = NULL,
  description = NULL,
  homepage = NULL,
  sources = NULL,
  contributors = NULL,
  keywords = NULL,
  created = NULL,

  # data package-compat
  profile = NULL,  #recommended
  image = NULL,  #optional
  ...,
  .add = FALSE
){
  # Preconditions
    for (el in c(name, id, title, description, version)){
      stopifnot(is.null(el) || is_scalar_character(el))
    }

    for (el in c(homepage, keywords, profile)){
      stopifnot(is.null(el) || is.character(el))
    }

    stopifnot(
      is.null(reference_date) || is_reference_date(reference_date),
      is.null(name) || is_dsinfo_name(name),
      is_bool(.add)
    )


  # Processing
    if (!is.null(name)) name <- tolower(name)

    if (inherits(sources, "dsinfo_source")){
      sources <- dsi_sources(sources)
    }

    stopifnot(is.null(sources) || inherits(sources, "dsinfo_sources"))

    info <- c(
      list(
        # recommended
        id = id,
        name = name,
        reference_date = reference_date,
        version = version,

        # data-package recommended
        license = license,

        # data-package optional
        title = title,
        description = description,
        homepage = homepage,
        sources = sources,
        contributors = contributors,
        keywords = keywords,
        image = image,
        created = created
      ),
      list(...)
    )


  if (.add){
    old_info <- dsinfo(x)

    for (nm in names(old_info)){
      if (is.null(info[[nm]])) info[[nm]] <- old_info[[nm]]
    }
  }


  info <- info[!unlist(lapply(info, is.null))]
  class(info) <- c("dsinfo", "list")

  if (inherits(x, "data.table")){
    x <- data.table::copy(x)
    data.table::setattr(x, "dsinfo", info)
  } else {
    dsinfo(x) <- info
  }


  x
}




#' @rdname dsinfo
#' @export
update_dsinfo <- function(...){
  set_dsinfo(..., .add = TRUE)
}




#' @rdname dsinfo
#' @export
#'
`dsinfo<-` <- function(x, value){
  attr(x, "dsinfo") <- value
  x
}



#' `set_source_script()` automatically adds the current script as a source
#'
#' @param x any \R object
#' @inheritParams dsi_source
#' @rdname dsinfo
#'
#' @export
#' @examples
#' x <- set_source_script(iris)
#' dsinfo(x)
set_source_script <- function(
  x,
  email = NULL,
  path = get_source_script(),
  title = basename(path),
  date = Sys.time()
){
  source_script <- dsinfo::dsi_source(
    title = title,
    path = path,
    date = date,
    email = email
  )

  attr(source_script, "auto_source_script") <-TRUE

  ori <- dsinfo(x)$sources
  rem <- vapply(ori, function(.) isTRUE(attr(., "auto_source_script")), logical(1))
  ori <- ori[!rem]

  if (length(ori)){
    src <- dsi_sources(dsi_sources_list(ori), source_script)
  } else {
    src <- dsi_sources(source_script)
  }

  update_dsinfo(
    x,
    sources = src
  )
}



# printing ----------------------------------------------------------------

#' @export
print.dsinfo <- function(x, ...){
  cat(format(x), sep = "\n")
  cat("\n")
  invisible(x)
}




#' @export
format.dsinfo <- function(
  x,
  ...
){
  title_els <- c("id", "name", "reference_date", "version")
  r1 <- list()

  if ("reference_date" %in% names(x)){
    x[["reference_date"]] <- format(x[["reference_date"]])
  }

  header_title <- paste(compact(x[c("id", "name")]), collapse = ": ")
  header_title <- style_accent(header_title)
  version <- paste(
    format(compact(x[c("version", "reference_date")])),
    collapse = " - "
  )

  if (!is_blank(version)){
    version <-
      style_subtle(paste("(", version, ")", collapse = "", sep = ""))
  }


  r1[["header"]]  <- paste(header_title, version)
  r1[["title"]]   <- paste_if_el(x, "title")
  r1[["desc"]]    <- paste_if_el(x, "description", prefix = "\n", suffix = "\n")

  if (identical(r1[["desc"]], "")){
    r1[["desc"]] <- "\t"
  }

  if (!is.null(x$sources)){
    r1[["sources"]] <-
      c(style_warning("sources:"), paste0(" ", format(x$sources)))
  }


  y <- x[!names(x) %in% union(title_els, c("description", "title", "sources"))]
  y <- lapply(y, as.character)

  if (length(y) > 0){
    r2 <- paste0(
      style_warning(paste0(names(y), ":")),
      "\n  ",
      style_subtle(y)
    )

    res <- c(r1, r2)
  } else {
    res <- r1
  }


  res <- res[res != ""]
  res <- unlist(res)
  res
}




# utils -------------------------------------------------------------------

paste_if_el <- function(x, els, prefix = NULL, suffix = NULL){
  sel <- grep(
    paste(els, collapse = "|"),
    names(x)
  )

  res <- x[sel]
  res <- lapply(res, as.character) # necessary for dates
  res <- unlist(res)

  if (!is.null(res)){
    paste(prefix, res, suffix, collapse = " - ", sep = "")
  } else {
    ""
  }
}




is_dsinfo_name <- function(x){
  is_scalar(x) && isTRUE(grepl("^[A-Za-z0-9_\\.-]*$", x))
}




#' Get the current script
#'
#' Tries to guess the path of the currently executing \R script .
#'
#' @return `character` scalar. the path of the current script.
#' @export
#' @examples
#' get_source_script()
get_source_script <- function(){
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle  <- "--file="
  match   <- grepl(needle, cmdArgs)

  if (any(match)) {
    res <- try(path_tidy(sub(needle, "", cmdArgs[match])))

  } else {
    res <- try(path_tidy(sys.frames()[[1]]$ofile))
  }

  if (is_try_error(res) || !length(res))
    res <- try(rstudioapi::getSourceEditorContext()$path)

  if (is_try_error(res))
    res <- "unknown"

  res
}

