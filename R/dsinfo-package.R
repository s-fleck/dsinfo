.onLoad <- function(...){

  if (requireNamespace("crayon", quietly = TRUE)){
    style_error   <- crayon::make_style("#BB3333", colors = 256)
    style_fatal   <- function(...) style_error(crayon::bold(...))
    style_warning <- crayon::make_style("#EEBB50", colors = 256)
    style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
    style_accent  <- style_warning
    col_nchar     <- crayon::col_nchar

  } else {
    style_fatal   <- function(...) paste(...)
    style_error   <- style_fatal
    style_warning <- style_fatal
    style_subtle  <- style_fatal
    style_accent  <- style_fatal
    col_nchar     <- function(...) nchar(...)
  }

  assign("style_fatal", style_fatal, envir = parent.env(environment()))
  assign("style_error", style_error, envir = parent.env(environment()))
  assign("style_warning", style_warning, envir = parent.env(environment()))
  assign("style_subtle", style_subtle, envir = parent.env(environment()))
  assign("style_accent", style_accent, envir = parent.env(environment()))
  assign("col_nchar", col_nchar, envir = parent.env(environment()))

}
