#based on:
#Wright, R., Holloway, S., & Ellis, M. (2011). Reconsidering both Diversity and Segregation: A Reply to Poulsen, Johnston and Forrest, and to Peach. Journal of Ethnic and Migration Studies, 37(1), 167–176. doi:10.1080/1369183X.2011.523865
#'@export
whe <- function(data,...){
  #calculate the entropy index
  diversity <- entropy_diversity(data,...)
  
  #find out what is the most numerous group
  domin <- largest_group_name(data,...)
  
  #find out the size of the largest group
  l_size <- largest_group(data,...)
  
  #find the size of the area population
  pop <- rowSums(data[,unlist(list(...))])
  #classify
  #moderately diverse: all others
  result <- "moderate diversity"
  #low diversity tract
  result <- ifelse(diversity <= 0.3707, "low diversity",result)
  result <- ifelse(l_size/pop > .8, "low diversity", result)
  #high diversity tract
  result <- ifelse(diversity >= 0.7414 & l_size/pop <= .45, "high diversity", result)
  #find out what is the most numerous group
  domin <- largest_group_name(data,...)
  result <- paste(result,domin,"neighbourhood",sep=" ")
  
  
}