#Brimicombe, A. J. (2007). Ethnicity, religion, and residential segregation in London: evidence from a computational typology of minority communities. Environment and Planning B: Planning and Design, 34(5), 884–904. doi:10.1068/b3309

#'@export



bmc <- function(data,unitid,cityid,majority,popsize,...){
  #   if(!is.data.frame(data)) stop("data must be a data frame")
  #   if(!is.numeric(data$unithost)) stop("unithost must be numeric")
  #   if(!is.numeric(data$unitmin)) stop("unitmin must be numeric")
  #   if(any(duplicated(data$unitid)) stop("unitid must uniquely identify units"))
  
  # Function to count population in case unitpop is not specified
  #   sumGroups <- function(majority,...){ # works fine
  #     sum <- majority
  #     for (i in 1:length(list(...))){
  #       sum <- sum + unlist(list(...)[i])
  #     }
  #     return(sum)
  #   }
  
  isGhetto <- function(minority,popsize,data){ 
    #>=.3 of largest minority live in polarization >=.6?
    return(sum(data[(data[minority])/(data[popsize])>=0.6,minority])/sum(data[minority])>=.3)  
  }
  
  getChi <- function(group,data){
    #returns the signed chi square for a group (as a vector)
    expected <- chisq.test(data[,group])$expected
    observed <- data[,group]
    signchi <- ((observed-expected)^2)/expected
    signchi <- ifelse(observed-expected<0,signchi*-1,signchi)
    return(signchi)
  }
  
  rNorm <- function(v){
    #robust normalisation of values
    median <-  as.numeric(quantile(v,na.rm=TRUE)[3])
    lowq <- as.numeric(quantile(v,na.rm=TRUE)[2])
    upq <- as.numeric(quantile(v,na.rm=TRUE)[4])
    rV <- ifelse(v<median, ((v-median)/(median-lowq)),
                 ((v-median)/(upq-median)))
    return(rV)
  }
  
  rule1 <- function(majority,data){ 
    #host group's positive signed chi-square?
    return(getChi(majority,data)>=0)
  }
  
  rule2a <- function(data,...){
    #any mynority with positive signed chi square?
    value <- logical(length=nrow(data))
    for (i in 1:length(unlist(list(...)))){
      value <- ifelse(getChi(unlist(list(...)[i]),data)>=0,TRUE,value)
    }
    return(value)
  }
  
  rule2b <- function(data,...){
    #any mynority with robust normalised chi >=  ?
    value <- logical(length=nrow(data))
    for (i in 1:length(unlist(list(...)))){
      value <- ifelse(rNorm(getChi(unlist(list(...)[i]),data))>=0,TRUE,value)
    }
    return(value)
  }
  
  rule3 <- function(majority,popsize,data){ 
    #host group <= .5 ?
    return((data[,majority]/data[,popsize])<=.5)
  }
  

###Stoped here
  rule4 <- function(majority,lminority,popsize,data,...){
    #largest minority group >= 2* the sum of the other minorities
    data[,"largest"] <- names(data[,unlist(list(...))])[max.col(data[unlist(list(...))],ties.method="first")]  
    print(data[,"largest"] )
    return((data[,lminority])>=
             (data[,popsize]-data[,majority]-data[,lminority])*2 &
             rNorm(getChi("largest",data)) >= 0 #Dunno how to get getChi to use the column for the largest minority in each unit. May need to loop by unit, but also to store the chi statistics for each minority.
           )    
  }



  rule5 <- function(majority,popsize,data){#works fine
    return((data[,majority]/data[,popsize])>=.8)
  }
  
  data["largestMinority"] <- do.call(pmax,data[unlist(list(...))])
  Rule1 <- rule1(unithost,unitpop,data)
  Rule2 <- rule2(unithost,"largestMinority",unitpop,data)
  Rule3 <- rule3(unithost,"largestMinority",unitpop,data)
  Rule4 <- rule4(unithost,"largestMinority",unitpop,cityid,data,...)
  Rule5 <- rule5(unithost,unitpop,data)
  
  #placeholder for neighbourhood classification
  type <- rep_len(NA,length.out=nrow(data))
  type <- ifelse(as.numeric(Rule1)==1 & as.numeric(Rule5)==1,6,type)
  type <- ifelse(as.numeric(Rule1)==1 & as.numeric(Rule5)==0,5,type)
  type <- ifelse(as.numeric(Rule1)==0 & as.numeric(Rule2)==1,1,type)
  type <- ifelse(as.numeric(Rule1)==0 & as.numeric(Rule2)==0 
                 & as.numeric(Rule3)==0,2,type)
  type <- ifelse(as.numeric(Rule1)==0 & as.numeric(Rule2)==0 
                 & as.numeric(Rule3)==1 & as.numeric(Rule4)==0,3,type)
  type <- ifelse(as.numeric(Rule1)==0 & as.numeric(Rule2)==0 
                 & as.numeric(Rule3)==1 & as.numeric(Rule4)==1,4,type)
  #   data["rule1"] <- Rule1
  #   data["rule2"] <- Rule2
  #   data["rule3"] <- Rule3
  #   data["rule4"] <- Rule4
  #   data["rule5"] <- Rule5
  #   data["concentration"] <- data["largestMinority"]/data[unitpop]
  #   data["hostprop"] <- data[unithost]/data[unitpop]
  #   data["otherminorities"] <- 
  #     (data[unitpop]-data[unithost]-data["largestMinority"])/data[unitpop]
  
  #variable and factor levels
  data["type"] <- factor(type, levels=c(1,2,3,4,5,6), 
                         labels = c("Assimilation/Pluralism Enclave",
                                    "Mixed Enclave",
                                    "Polarized Enclave",
                                    "Extreme Polarized Enclave",
                                    "Nonisolated Host Community",
                                    "Isolated Host Community"))
  
  #cleanup
  data["largestMinority"] <- NULL
  
  return(data)
}