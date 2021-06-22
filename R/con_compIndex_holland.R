#' @title Compatibility Index according to Wiggins & Moody (1981)
#' @keywords congruence
#' @export con_compindex_holland
#' @description The function computes the three-letter \emph{Compatibility} index according to Wiggins & Moody (1981).  
#' @details The function finds the congruence according to Wiggins & Moody (1981) between the three-letter Holland codes given in argument a, which is the person code, and argument b, which is the environment code. The Index as defined by Wiggins & Moody (1981) targets (only) three letters from the Holland code. The degree of congruence is output, according to its definition by Wiggins & Moody (1981), as a reciprocal value of a distance. This means, for example, that a value of '8' is the result for a perfect fit of two three-letter codes !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Wiggins, J. D. & Moody, A. (1981). A field-based comparison of four career-exploration approaches. \emph{Vocational Guidance Quarterly, 30}(1), 15–20. US: American Counseling Assn. https://doi.org/10.1002/j.2164-585X.1981.tb01071.x
#' @examples 
#' con_compindex_holland(a="RIA",b="SEC") # max. difference 
#' con_compindex_holland(a="RIA",b="RIA") # max. similarity
################################################################################
# func. by joerg-henrik heine jhheine(at)googlemail.com 
con_compindex_holland<-function(a,b)
{
  # Compatibility index (Wiggins & Moody, 1981)
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 3) print("the index is in this function use only the first two-letters of the Holland-codes")

  ### Hilfsfunktion  bei dieser ist die Reihenfolge der if statements wichtig!!!
  comp<-function(per,umw)
    {verg<-0;
     if( (sum(per[3] == umw[3])==1) ){verg<-1}
     
     if( (sum(per[1] == umw[3])==1) ){verg<-2}
     if( (sum(per[2] == umw[2])==1) ){verg<-2}
     if( (sum(per[2] == umw[3])==1) ){verg<-2}
     
     
     if( (sum(per[2:3] == umw[c(2,3)])==2) ){verg<-3}
     if( (sum(per[2:3] == umw[c(3,2)])==2) ){verg<-3}
     if( (sum(per[2:3] == umw[c(1,2)])==2) ){verg<-3}
     if( (sum(per[2:3] == umw[c(2,1)])==2) ){verg<-3}
     if( (sum(per[2:3] == umw[c(3,1)])==2) ){verg<-3}
     if( (sum(per[2:3] == umw[c(1,3)])==2) ){verg<-3}
     if( (sum(per[1] == umw[c(2)])==1) ){verg<-3}
     
     if( (sum(per[1:3] == umw[c(1,2,3)])==2) ){verg<-4}
     if( (sum(per[1:3] == umw[c(1,3,2)])==2) ){verg<-4}
     if( (sum(per[1:3] == umw[c(3,1,2)])==2) ){verg<-4}
     if( (sum(per[1:3] == umw[c(3,2,1)])==2) ){verg<-4}
     if( (sum(per[1:3] == umw[c(2,3,1)])==2) ){verg<-4}
     if( (sum(per[1:3] == umw[c(2,1,3)])==2) ){verg<-4}
     
     if( (per[1]==umw[1]) & (sum(per[2:3] == umw[c(3)])==1) ){verg<-5}
     if(sum(per[1:2] == umw[2:1])==2){verg<-5}
     
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(1,2,3)])==3) ){verg<-6}
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(1,3,2)])==3) ){verg<-6}
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(3,1,2)])==3) ){verg<-6}
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(3,2,1)])==3) ){verg<-6}
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(2,3,1)])==3) ){verg<-6}
     if( (per[1]!=umw[1]) & (sum(per[1:3] == umw[c(2,1,3)])==3) ){verg<-6}
     
     if(sum(per[1:2] == umw[1:2])==2){verg<-7}
     if(sum(per[1:3] == umw[c(1,3,2)])==3){verg<-7}
     
     if(sum(per[1:3] == umw[1:3])==3){verg<-8}
    
     return(verg)}
  ### Ende Hilfsfunktionen 
  erg <- comp(per = a,umw = b)
  return (erg)
}

  
  