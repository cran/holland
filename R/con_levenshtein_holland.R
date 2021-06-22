#' @title Congruence Index based on the Levenshtein distance
#' @keywords congruence
#' @export con_levenshtein_holland
#' @description The function computes the location-weighted, cost-sensitive (referring to the hexagon relationships) Levenshtein distance (Levenshtein, 1966) see also Needleman & Wunsch (1970).  
#' @details The function finds the distance according to Levenshtein (1966) between two sequences (see Abbott, 1995), which are the Holland codes given in argument a, which is the person code, and argument b, which is the environment code. Computational details can be found in Needleman & Wunsch, (1970).
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @param costs character with default set to \code{costs = "hexa"} to use a matrix with replacement costs based on the RIASEC structure for the calculation of the distance. 
#' @param weights a numeric vector with length equal to \code{a} and \code{b} assigning a weight to the places (1:6) of the letter-codes defined in \code{a} and \code{b}.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Levenshtein, V. I. (1966). Binary Codes Capable of Correcting Deletions, \emph{Insertions and Reversals. Soviet Physics Doklady}, 10, 707.
#' @references Abbott, A. (1995, August). Sequence Analysis: New Methods for Old Ideas. \emph{Annual Review of Sociology}, 21, 93–113.
#' @references Needleman, S. B., & Wunsch, C. D. (1970). A general method applicable to the search for similarities in the amino acid sequence of two proteins. \emph{Journal of Molecular Biology, 48}(3), 443–453. http://doi.org/10.1016/0022-2836(70)90057-4
#' @examples 
#' con_levenshtein_holland(a="RIA",b="SEC") # max. difference 
#' con_levenshtein_holland(a="RIA",b="RIA") # max. similarity
#' # with 6 characters in Holland-code
#' w <- c(1.5,1.25,1,0.75,0.5,0.25)
#' con_levenshtein_holland(a="RIASEC",b="SECRIA", weights=w) # max. difference

con_levenshtein_holland <- function(a, b, costs="hexa", weights=c(1.5,1.25,1) ){
  # stellen gewichtete Levenshtein Distanz (für 3 stellige hollandcodes)
  # kostensensitive (hexagon beziehungen)  Levenshtein Distanz
  # vgl.Needleman, S. B., & Wunsch, C. D. (1970). A general method applicable to the search for similarities in the amino acid sequence of two proteins. Journal of Molecular Biology, 48(3), 443–453. http://doi.org/10.1016/0022-2836(70)90057-4
  #func. by: jhheine@googlemail.com
  rel=FALSE # internal option to standardize strings with different length - not used yet
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 6 ) stop("a Levenshtein-distance is in this function limited to a max. of up to six-letter Holland-codes")
  if(length(a) != length(weights)) stop("the weights vector must have the same length as number of characters in Holland-codes in a and b")
  distance <- data.frame(row.names=0:length(a))
  distance[,1] <- 0:length(a)
  if(costs=="hexa"){
    costs.hexa <- matrix(c(0.0,.25,.50,.75,.50,.25,.25,0.0,.25,.50,.75,.50,.50,.25,0.0,.25,.50,.75,.75,.50,.025,0.0,.25,.50,.50,.75,.50,.25,0.0,.25,.25,.50,.75,.50,.25,0.0),dimnames = list(c("R","I","A","S","E","C"),c("R","I","A","S","E","C")),ncol = 6,byrow = TRUE)
    costs.v <- diag(costs.hexa[a,b])
    }

  for(j in 1:length(b)){
    distance[,j+1] <- NA
  }
  distance[1,] <- 0:length(b)
  
  for(i in 2:(length(a)+1)){
    for(j in 2:(length(b)+1)){
      if(a[i-1]==b[j-1]){
        similarity_add <- 0
      } else {
        similarity_add <- weights[(j-1)] * costs.hexa[a[i-1],b[j-1]]
      }
      
      distance[i,j] <- min(
        distance[i-1,j] + (costs.hexa[a[i-1],b[j-1]])/2 ,
        distance[i,j-1] + (costs.hexa[a[i-1],b[j-1]])/2 ,
        distance[i-1,j-1] + similarity_add
      )
    }
  }
  if(rel==FALSE){
    return(distance[length(distance[,1]),length(distance[1,])])
  } else {
    return(round(distance[length(distance[,1]),length(distance[1,])]/max(length(a), length(b)), 6))
  }
}
