largest_group_name <- function(data,...){
 #find index(es) of largest column(s) in each row
  largest <- apply(data[,unlist(list(...))],1,function(x){which(x==max(x))})
 #find name(s) of largest column(s)
  names <- lapply(largest,function(x){names(data[,unlist(list(...))])[x]})
 #returns a list of names
 return(names)
}