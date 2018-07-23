source_python('alphabet.py')

getAlphabetsR <- function(filepath){
  alphabets = generateAlphabetsPy(filepath)
  clean_results <- list(
    H1 = alphabets[1],
    H2 = alphabets[2],
    H3 = alphabets[3],
    H4 = alphabets[4],
    H2_noTab = alphabets[5],
    H3_noTab = alphabets[6],
    H4_noTab = alphabets[7]
        )
  clean_results
}


