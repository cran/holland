#' @title Congruence Index according to Holland (1963)
#' @keywords congruence
#' @export con_oneletter_holland
#' @description The function computes the one letter congruence index according to Holland (1963).  
#' @details The function finds the congruence according to Holland (1963) between the Holland codes given in argument a, which is the person code, and argument b, which is the environment code. The Index is based on one letter from the Holland code, which is, according to Holland (1963) typically the first letter. But this can be changed to using any of the six letters, see argument \code{letter}. The degree of congruence is output as distance. This means, for example, that a value of '0' is the result for a perfect fit ! The function offers via the argument \code{hexadist} the extension to consider the spatial distance of the six dimensions in the hexagon for the calculation of the congruence (cf. Bowles, S. M., & Tunick, R. H. 2008).
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @param hexadist logical with default set to \code{hexadist = FALSE}. If set to \code{hexadist = TRUE} the spacial distances in the hexagon are considered for the calculation of the first letter congruence.
#' @param letter a integer indicating the position of the letter to be used.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Bowles, S. M. & Tunick, R. H. (2008). \emph{Is Congruence Dead? An Examination of the Correlation Between Holland’s Congruence and Job Satisfaction Using Improved Methodology.} Morgantown, West Virginia: West Virginia University.
#' @examples 
#' con_oneletter_holland(a="RIASEC",b="AIRCES")
#' con_oneletter_holland(a="RIASEC",b="AIRCES",hexadist=TRUE)
#' con_oneletter_holland(a="RIASEC",b="AIRCES",letter=2)
#' con_oneletter_holland(a="RIASEC",b="AIRCES",letter=6)
################################################################################
#### funktion: zur Bestimmung der Übereinstimmung für jede zeile der matrix X mit dem Vector V ######
con_oneletter_holland<-function(a,b, hexadist=FALSE, letter=1)
{ # a="RIA"; b="RIS";a="RIA"; b="SEC";
  # kongruenzindex nach Holland (1963) 
  # mit erweiterung zur nutzung der hexagon distanz vgl. Bowles, S. M., & Tunick, R. H. (2008). Is Congruence Dead? An Examination of the Correlation Between Holland’s Congruence and Job Satisfaction Using Improved Methodology. ProQuest.
  # (vgl. Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. Vocational Guidance Quarterly, 11(4), 232–239.) 
  #func. by: jhheine@googlemail.com
  # ausgabe als distanz: d.h. '0' als ergebnis bedeutet perfekte passung !
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  hexa <- matrix(c(0, 1, 2, 3, 2, 1, 1, 0, 1, 2, 3, 2, 2, 1, 0, 0, 2, 3, 3, 2, 1, 0, 1, 2, 2, 3, 2, 1, 0, 1, 1, 2, 3, 2, 1, 0),dimnames = list(c("R","I","A","S","E","C"),c("R","I","A","S","E","C")),ncol = 6,byrow = TRUE)  # hexagon distanz
  
  if(hexadist==FALSE){
    erg <- 1*!(a[letter]==b[letter])
  }
  if(hexadist==TRUE){
    erg <-hexa[a[letter],b[letter]]
  }
  return(erg)
}
  
  
  
  