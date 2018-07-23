library(reticulate)
source_python('mainFunction.py',convert = TRUE)

getDataR_upto4 <- function(filepath,alphabetH1,alphabetH2,alphabetH3,alphabetH4, pc){
  datastructure = mainFunctionPy_upto4(filepath,alphabetH1,alphabetH2,alphabetH3,alphabetH4,pc)
  clean_results <- list(
    Entropy = unlist(datastructure[1])[[1]],
    #meanEntropy = unlist(datastructure[2]),
    Counts2 = unlist(datastructure[2])[[1]],
    F1 = datastructure[3][[1]],
    F2 = unlist(datastructure[4])[[1]],
    F3 = unlist(datastructure[5])[[1]],
    F4 = unlist(datastructure[6])[[1]],
    tEntropy = unlist(datastructure[7])[[1]],
    levels = c("H0","H1","H2","H3","H4"),
    observations = unlist(datastructure[8])
    
  )
  clean_results
}


getDataR_upto3 <- function(filepath,alphabetH1,alphabetH2,alphabetH3, pc){
  datastructure = mainFunctionPy_upto3(filepath,alphabetH1,alphabetH2,alphabetH3,pc)
  clean_results <- list(
    Entropy = unlist(datastructure[1])[[1]],
    #meanEntropy = unlist(datastructure[2]),
    Counts2 = unlist(datastructure[2])[[1]],
    F1 = datastructure[3][[1]],
    F2 = unlist(datastructure[4])[[1]],
    F3 = unlist(datastructure[5])[[1]],
    tEntropy = unlist(datastructure[6])[[1]],
    levels = c("H0","H1","H2","H3"),
    observations = unlist(datastructure[7])
    
  )
  clean_results
}

getDataR_upto2 <- function(filepath,alphabetH1,alphabetH2, pc){
  datastructure = mainFunctionPy_upto2(filepath,alphabetH1,alphabetH2,pc)
  clean_results <- list(
    Entropy = unlist(datastructure[1])[[1]],
    #meanEntropy = unlist(datastructure[2]),
    Counts2 = unlist(datastructure[2])[[1]],
    F1 = datastructure[3][[1]],
    F2 = unlist(datastructure[4])[[1]],
    tEntropy = unlist(datastructure[5])[[1]],
    levels = c("H0","H1","H2"),
    observations = unlist(datastructure[6])
    
  )
  clean_results
}
