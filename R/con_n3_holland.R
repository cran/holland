#' @title Congruence Index according to Joerin Fux (2005)
#' @keywords congruence
#' @export con_n3_holland
#' @description The function computes the congruence index according to Joerin Fux (2005).  
#' @details The function finds the congruence according to Joerin Fux (2005) between the up to six-letter Holland-codes given in argument a, which is the person code, and argument b, which is the environment code. The Index as defined by Jörin (2005) targets (only) three letters from the Holland code. The degree of congruence is output, according to its definition by Joerin Fux (2005), as a reciprocal value of a distance. This means, for example, that a value of '3' is the result for a perfect fit of two three-letter codes !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Joerin Fux, S. (2005). \emph{Persönlichkeit und Berufstätigkeit: Theorie und Instrumente von John Holland im deutschsprachigen Raum, unter Adaptation und Weiterentwicklung von Self-directed Search (SDS) und Position Classification Inventory (PCI)}. 1. Aufl. Göttingen: Cuvillier.
#' @examples 
#' con_n3_holland(a="RIA",b="SEC") # max. difference 
#' con_n3_holland(a="RIA",b="RIA") # max. similarity
################################################################################
con_n3_holland<-function(a,b){
  # kongruenzindex nach Jörin (2001) cited nach "Persönlichkeit und Berufstätigkeit, Simone Jörin Fux 2005 
  # zur Bestimmung der Übereinstimmung zwischen dem Vector a == Personencode b == Umweltcode
  # (vgl. Joerin-Fux, Simone "Persö. & Berufst.: Theor. & Instrum. v. J. Holland")  
  #func. by: jhheine@googlemail.com 
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 6 ) stop("the n3 index is in this function limited to a max. of up to six-letter Holland-codes")
  
  l <- length(a)
  #erg <- sum(matrix(c( (a==b[1]),(a==b[2]),(a==b[3])   ),ncol=3))
  erg <- sum( sapply(1:l, function(x){a==b[x]}) )
  
  return (erg)
}
