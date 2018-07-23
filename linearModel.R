

source_python('LM.py')

linearModelR <- function(levels, Entropy_DF, Group){
  LinearModel = linearModelPy(levels, Entropy_DF, Group)
  clean_results <- list(
    LM = LinearModel
  )
  clean_results
}
