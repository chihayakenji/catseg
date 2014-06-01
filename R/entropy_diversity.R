entropy_diversity <- function(data,...){
  #define internal variables
  pop <- rowSums(data[,unlist(list(...))])
                     
 #calculate the proportions for each group in the area
  p_i <- data.frame(lapply(data[,unlist(list(...))],function(x){x/pop}))

  #calculate entropy
  h <- -1*((
            1/log(pop)
            )*rowSums(
                      data.frame(lapply(p_i,
                                        function(x){  
                                                    ifelse(x!=0,
                                                           x*log(x^(-1)),
                                                           0)
                                                    }))))
 
 h <- ifelse(pop>1,
             h,
             0
             )
 
 return(h)
}