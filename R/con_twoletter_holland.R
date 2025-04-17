#' @title Congruence Index according to Healy & Mourton (1983)
#' @keywords congruence
#' @export con_twoletter_holland
#' @description The function computes the two-letter congruence index according to Healy & Mourton (1983).  
#' @details The function finds the congruence according to Healy & Mourton (1983) between the two-letter Holland codes given in argument a, which is the person code, and argument b, which is the environment code. The Index as defined by Healy & Mourton (1983) targets (only) two letters from the Holland code. The degree of congruence is output, according to its definition by Healy & Mourton (1983), as a reciprocal value of a distance. This means, for example, that a value of '3' is the result for a perfect fit of two two-letter codes !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Healy, C. C. & Mourton, D. L. (1983). Derivatives of the Self-Directed Search: Potential clinical and evaluative uses. \emph{Journal of Vocational Behavior, 23}(3), 318–328. https://doi.org/10.1016/0001-8791(83)90045-3 
#' @examples 
#' con_twoletter_holland(a="RI",b="SE") # max. difference 
#' con_twoletter_holland(a="RI",b="RI") # max. similarity
################################################################################
# func. by joerg-henrik heine jhheine(at)googlemail.com 
con_twoletter_holland<-function(a,b)
{
  # Two–letter code agreement (Healy & Mourton, 1983)
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 2) print("the index is in this function use only the first two-letters of the Holland-codes")
  ### Hilfsfunktionen
  two<-function(per,umw)
    {verg<-1;
     if(sum(per[1:2] == umw[1:2])==2){verg<-3}
     if(sum(per[2:1] == umw[1:2])==2){verg<-3}
     if(sum(per[1:2] == umw[1:2])==1){verg<-2}
     if(sum(per[2:1] == umw[1:2])==1){verg<-2}  
     return(verg)}
  ### Ende Hilfsfunktionen 
  erg<-two(a,b) 
  return (erg)
}

  
  