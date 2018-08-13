is_scalar_character <- function(x){
  is.character(x) && identical(length(x),  1L)
}




compact <- function(x){
  x[!vapply(x, is.null, FALSE)]
}




is.scalar <- function(x){
  identical(length(x), 1L)
}




is.flag <- function(x){
  is.scalar(x) & is.logical(x)
}




is_empty <- function(x){
  identical(length(x), 0L)
}




is_blank <- function(x){
  trimws(x) == ""
}




cat_lines <- function(x){
  stopifnot(all(vapply(x, is.character, FALSE)))
  cat(paste0(x, collapse = "\n"))
}



is_data.table <- function(x){
  requireNamespace("data.table", quietly = TRUE) &&
  data.table::is.data.table(x)
}
