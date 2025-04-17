#' @title Congruence Index according to Wolfe & Betz (1981)
#' @keywords congruence
#' @export con_threeletter_holland
#' @description The function computes the three-letter congruence index according to Wolfe & Betz (1981).  
#' @details The function finds the congruence according to Wolfe & Betz (1981) between the three-letter Holland codes given in argument a, which is the person code, and argument b, which is the environment code. The Index as defined by Wolfe & Betz (1981) targets (only) three letters from the Holland code. The degree of congruence is output, according to its definition by Wolfe & Betz (1981), as a reciprocal value of a distance. This means, for example, that a value of '2' is the result for a perfect fit of two three-letter codes !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Wolfe, L. K. & Betz, N. E. (1981). Traditionality of choice and sex-role identification as moderators of the congruence of occupational choice in college women. \emph{Journal of Vocational Behavior, 18}(1), 43–55. https://doi.org/10.1016/0001-8791(81)90028-2
#' @examples 
#' con_threeletter_holland(a="RIA",b="SEC") # max. difference 
#' con_threeletter_holland(a="RIA",b="RIA") # max. similarity
################################################################################
# func. by joerg-henrik heine jhheine(at)googlemail.com 
con_threeletter_holland<-function(a,b)
{
  # Three–letter code agreement (Wolfe & Betz, 1981) 
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 3) print("the index is in this function use only the first three-letters of the Holland-codes")
  
  
  ### Hilfsfunktionen
  three<-function(per,umw)
    {verg<-0;
     if(sum(per[1:3] == umw[1:3])==3){verg<-2}
     if(sum(per[1:2] == umw[1:2])==2){verg<-2}
     if(sum(per[1:3] == umw[c(3,2,1)])==3){verg<-2}
     if(sum(per[1:3] == umw[c(3,1,2)])==3){verg<-2}
     if(sum(per[1:3] == umw[c(2,3,1)])==3){verg<-2}
     if(sum(per[1:3] == umw[c(2,1,3)])==3){verg<-2}
     if(sum(per[1:3] == umw[c(1,3,2)])==3){verg<-2}
     if(sum(per[1] == umw[1])==1){verg<-1}
     if(sum(per[1:2] == umw[c(3,2)])==2){verg<-2}
     if(sum(per[1:2] == umw[c(3,1)])==2){verg<-2}
     if(sum(per[1:2] == umw[c(2,3)])==2){verg<-2}
     if(sum(per[1:2] == umw[c(2,1)])==2){verg<-2}
     if(sum(per[1:2] == umw[c(1,3)])==2){verg<-2}  
     if(sum(per[1:2] == umw[c(1,2)])==2){verg<-2}
     return(verg)}
  ### Ende Hilfsfunktionen 
  erg<-three(a,b) 
  return (erg)
}

  
  