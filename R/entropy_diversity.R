entropy_diversity <- function(data,...){
  #define internal variables
  popsize <- rowSums(data[,unlist(list(...))])
                     
  #calculate the proportions for each group in the area
  p_frame <- data.frame(lapply(data[,unlist(list(...))],function(x){x/popsize}))
                                                                                            
  #calculate entropy
  entropy <- -1*((1/log(popsize))*rowSums(data.frame(lapply(p_frame,
                                                            function(x){x*log(x)})
                                                     )
                                          )
                 )
  return(entropy)
}