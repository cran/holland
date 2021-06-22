#' @title Congruence Index according to Iachan (1984)
#' @keywords congruence
#' @export con_iachan_holland
#' @description The function computes the congruence index according to Iachan (1984).  
#' @details The function finds the congruence according to Iachan (1984) between the three-letter Holland-codes given in argument a, which is the person code, and argument b, which is the environment code. The Index is defined for three letters from the Holland code. The degree of congruence is output, according to its definition by Iachan (1984), as a reciprocal value of a distance. This means, for example, that a value of '28' is the result for a perfect fit !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Iachan, R. (1984). A measure of agreement for use with the Holland classification system. \emph{Journal of Vocational Behavior, 24} (2), 133–141.
#' @examples 
#' con_iachan_holland(a="RIA",b="SEC") # max. difference 
#' con_iachan_holland(a="RIA",b="RIA") # max. similarity
################################################################################
con_iachan_holland<-function(a,b){
  # kongruenzindex nach Iachan(1984) auch M-Index bezeichnet 
  # zur Bestimmung der Übereinstimmung für vector a==Personencodes mit 
  # dem Vector b==Umweltcode diese Reihenfolge ist bei der Eingabe W I C H T I G  !!!
  # (vgl. Rolfs, Henning "Beruflichen Interessen", xxxx)  
  # (vgl. Sageder,J. in Abel, J.& Tarnai, Ch. 19xx; Seite 130)
  #func. by: jhheine@googlemail.com 
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 3 ) stop("a Iachan-Index is only defined for three-letter Holland-code")
  IM<-matrix(c(22,10,4,10,5,2,4,2,1),ncol=3)
  bM<-t(matrix(rbind(b,b,b),ncol=3))
  aM<-matrix(rbind(a,a,a),ncol=3)
  ergM<-((aM==bM)*IM)
  erg <- sum(ergM)
  return (erg)
}
