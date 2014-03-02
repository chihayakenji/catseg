#classifies neighbourhoods according to Poulsen et.al classification.
#Poulsen, M., Johnston, R., & Forrest, J. (2001). Intraurban ethnic enclaves: introducing a knowledge-based classification method. Environment and Planning A, 33(11), 2071–2082.

#rule 1: Minority enclave rule
#rule 2: Associated rule
#rule 3: Encapsulation rule
#rule 4: Ghetto rule
#rule 5: Host community isolation rule

# #fake data for testing the code
# fakedata <- data.frame (SAMSKod = c(81,82,83,84,85,86,91,92,93,94,95,96),
#                         KomKod = c(8,8,8,8,8,8,9,9,9,9,9,9),
#                         popSizeKom = c(1500,1500,1500,1500,1500,1500,2500,2500,2500,2500,2500,2500),
#                         eBack1Kom = c(1000,1000,1000,1000,1000,1000,1400,1400,1400,1400,1400,1400),
#                         eBack2Kom = c(100,100,100,100,100,100,300,300,300,300,300,300),
#                         eBack3Kom = c(150,150,150,150,150,150,350,350,350,350,350,350),
#                         eBack4Kom = c(250,250,250,250,250,250,450,450,450,450,450,450),
#                         popSizeSAMS = c(280,240,170,180,370,260,330,300,470,300,660,440),
#                         eBack1SAMS = c(250,50,100,100,300,200,150,20,300,140,520,270),
#                         eBack2SAMS = c(10,10,20,20,20,20,60,30,40,40,40,90),
#                         eBack3SAMS = c(10,40,25,25,25,25,60,20,85,95,45,45),
#                         eBack4SAMS = c(10,140,25,35,25,15,60,230,45,25,45,35),
#                         stringsAsFActors = FALSE)

#'@export

pjf <- function(data,unitid,cityid,unithost,unitpop,...){
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
  
  
  rule4 <- function(majority,lminority,popsize,cityid,data,...){#works fine
    data[,"largest"] <- names(data[,unlist(list(...))])[max.col(data[unlist(list(...))],ties.method="first")]  
    data["ghetto"] <- FALSE
    for (i in 1:length(data[,"largest"])){
      min <- data[i,"largest"]
      data[i,"ghetto"] <- isGhetto(min,popsize,data[data[,cityid]==data[i,cityid],])
    }
    return((data[,lminority]/data[,popsize])>=.6 
          & (data[,lminority])>=(data[,popsize]-data[,majority]-data[,lminority])*2
          & data[,"ghetto"])    
  }
  
  
  data["largestMinority"] <- largest_group(data,...)
  
  #host group >= .5 ?
  Rule1 <- concentration(unithost,unitpop,data)>=.5
  
  #concentration>=.2, host population >=.3 host population < .5 ?
  Rule2 <- concentration("largestMinority",unitpop,data)>=.2 &
           concentration(unithost,unitpop,data)>=.3 &
           concentration(unithost,unitpop,data)<.5
  
  #Polarized?
  Rule3 <- polarization(unithost,"largestMinority",unitpop,data)
  
  Rule4 <- rule4(unithost,"largestMinority",unitpop,cityid,data,...)

  #host group >= .8 ?
  Rule5 <- concentration(unithost,unitpop,data)>=.8
  
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
  
  #variable and factor levels
  type <- factor(type, levels=c(1,2,3,4,5,6), 
                         labels = c("Assimilation/Pluralism Enclave",
                                    "Mixed Enclave",
                                    "Polarized Enclave",
                                    "Extreme Polarized Enclave",
                                    "Nonisolated Host Community",
                                    "Isolated Host Community"))
  
  
  return(type)
}