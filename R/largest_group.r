#'@export
largest_group <- function(data,...){
  do.call(pmax,data[unlist(list(...))])
}