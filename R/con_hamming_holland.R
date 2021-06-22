#' @title Congruence Index based on the Hamming distance
#' @keywords congruence
#' @export con_hamming_holland
#' @description The function computes the location-weighted, cost-sensitive Hammig distance (Hamming, 1950).  
#' @details The function finds the distance according to Hamming (1950) between two sequences (see Abbott, 1995), which are the Holland codes given in argument a, which is the person code, and argument b, which is the environment code.
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @param costs character with default set to \code{costs = "hexa"} to use a matrix with replacement costs based on the RIASEC structure for the calculation of the distance. 
#' @param weights a numeric vector with length equal to \code{a} and \code{b} assigning a weight to the places (1:6) of the letter-codes defined in \code{a} and \code{b}.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Hamming, R. (1950). Error detecting and error correcting codes. \emph{Bell System Technical Journal, The, 29}(2), 147–160.
#' @references Abbott, A. (1995, August). Sequence Analysis: New Methods for Old Ideas. \emph{Annual Review of Sociology}, 21, 93–113.
#' @examples 
#' con_hamming_holland(a="RIA",b="SEC") # max. difference 
#' con_hamming_holland(a="RIA",b="RIA") # max. similarity
#' con_hamming_holland(a="RIASEC",b="SECRIA", weights=c(1.5,1.25,1,0.75,0.5,0.25)) # max. difference
con_hamming_holland <- function(a,b,costs="hexa",weights=c(1.5,1.25,1)){
  # Kostensensitive , (stellen-)gewichtete Hamming Distanz
  #func. by: jhheine@googlemail.com 
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 6 ) stop("a Hamming-distance is in this function limited to a max. of up to six-letter Holland-codes")
  if(length(a) != length(weights)) stop("the weights vector must have the same length as number of characters in Holland-codes in a and b")
  if(costs=="hexa"){
  costs.hexa <- matrix(c(0.0,.25,.50,.75,.50,.25,.25,0.0,.25,.50,.75,.50,.50,.25,0.0,.25,.50,.75,.75,.50,.025,0.0,.25,.50,.50,.75,.50,.25,0.0,.25,.25,.50,.75,.50,.25,0.0),dimnames = list(c("R","I","A","S","E","C"),c("R","I","A","S","E","C")),ncol = 6,byrow = TRUE)
  }
  costs.v <- diag(costs.hexa[a,b])
  erg <- sum(costs.v*weights)  
  return(erg)
}
