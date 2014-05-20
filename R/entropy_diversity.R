entropy_diversity <- function(data,...){
  #define internal variables
  pop <- rowSums(data[,unlist(list(...))])
                     
  #calculate the proportions for each group in the area
  p_i <- data.frame(lapply(data[,unlist(list(...))],function(x){x/pop}))

  #calculate entropy
  h <- -1*((1/log(pop))*rowSums(data.frame(lapply(p_i,
                                                  function(x){
                                                              x*log(x)
                                                              }))))
  return(h)
}