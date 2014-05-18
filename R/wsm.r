#based on:
#Wright, R., Holloway, S., & Ellis, M. (2011). Reconsidering both Diversity and Segregation: A Reply to Poulsen, Johnston and Forrest, and to Peach. Journal of Ethnic and Migration Studies, 37(1), 167–176. doi:10.1080/1369183X.2011.523865

wsm <- function(data,unitid,unitpop,...){
  #calculate the entropy index
  diversity <- entropy_diversity(data,unitid,unitpop,..)
  
  #find out what is the most numerous group
  
  domin <- largest_group(data,...)
  #classify
  #low diversity tract
    #entropy <= 0.3707 or one group > .80 of pop
  #high diversity tract
    #entropy >= 0.7414 and no group > .45 of pop
  #moderately diverse: all others
}