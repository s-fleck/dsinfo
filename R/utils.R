is_scalar_character <- function(x){
  is.character(x) && identical(length(x),  1L)
}




compact <- function(x){
  x[!is.null(x)]
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
