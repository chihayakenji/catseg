#'@export

polarization <- function(majority,lminority,popsize,data){
  return(concentration(lminority,popsize,data)>=.2 &
           (data[,lminority]>=(data[,popsize]-data[,majority]-data[,lminority])*2)
         )
}