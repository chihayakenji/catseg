#'@export

concentration <- function(group,popsize,data){ 
  return((data[,group])/(data[,popsize]))
}