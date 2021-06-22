#' @title Convert Holland score profiles to Holland-letter-codes
#' @keywords misc
#' @export sco2let
#' @description The function converts a individual (person or environment) score profile consisting of six numerical score values into a Holland-letter code with length varying from 1 to 6 letters.  
#' @details The numeric vector containing the score profile (see argument \code{A}) should have named numerical values (which is recommended for clarity). In this case, the order of the scores (e.g. \code{names(A) <- c("R","I","A","S","E","C")} or \code{names(A) <- c("C","E","S","I","A","R")} or any other) in the vector assigned to argument \code{A} does not matter. 
#' However, a vector with unnamed numerical values can also be used, in which case the assumption is made that the order of the Holland scores (numerical values) follows the scheme \code{names(A) <- c("R","I","A","S","E","C")}; see examples below.  
#'
#' @param A a numeric vector with Holland score values for the interest profile of length = 6.
#' @param len a integer with values of either 1, 2, 3, 4, 5 or 6 indicating how many letters to return; default is set to \code{len = 3} to return a Holland three-letter code.
#' @return a character with the Holland-letter code (in upper case letters).
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @examples 
#' # A fictional interest profile:
#' A <- c(70, 90, 120, 50, 60, 130)
#' names(A) <- c("R","I","A","S","E","C")
#' A
#' sco2let(A)
#' # which is the same as ...
#' A <- c(70, 90, 120, 50, 60, 130); names(A)
#' A
#' sco2let(A)
#' # But see ...
#' A <- c(70, 90, 120, 50, 60, 130)
#' names(A) <-c("c","e","s","i","a","r")
#' A
#' sco2let(A)
#' # other length of letter code ...
#' sco2let(A, len = 1)
#' sco2let(A, len = 6)
#' sapply(1:6, function(x){sco2let(A,x)})

################################################################################
# A <- c(70, 90, 120, 50, 60, 130); 
# names(A) <- c("r","i","a","s","e","c")
# names(A) <-c("C","E","S","I","A","R")
# len =1
sco2let<-function(A, len=3){
  #func. by: jhheine@googlemail.com 
  if(length(A)!=6) stop("vector for Holland score profile must contain 6 scores")
  if((sum(1:6%in%len)==0)) stop("len must be either 1, 2, 3, 4, 5 or 6" )
  if(is.null(names(A))){names(A) <- c("R","I","A","S","E","C")}
  names(A) <- toupper(names(A))
  if( any(!names(A)%in%c("R","I","A","S","E","C") )) stop("unknown names in vector for Holland score profile")
  erg <- paste(names(sort(A,decreasing = TRUE)[1:len]),collapse = "")
  return (erg)
}
