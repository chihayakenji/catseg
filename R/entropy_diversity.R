#as defined in Holloway et. all 2012
#E = s * sum_{k=1}{K}((k_j/t_j)*ln(t_j/k_j))
#'@export
entropy_diversity <- function(data,...){
  #define internal variables
  pop <- rowSums(data[,unlist(list(...))])
                     
 #calculate the proportions for each group in the area
  n <- length(unlist(list(...)))
  p_i <- data.frame(lapply(data[,unlist(list(...))],function(x){x/pop}))
  h <- ((
         1/log(n)
            )*rowSums(
                      data.frame(lapply(p_i,
                                        function(x){  
                                                    ifelse(x!=0,
                                                           x*log(1/x),
                                                           0)
                                                    }))))
 
 h <- ifelse(pop>1,
             h,
             0
             )
 
 return(h)
}