#' @title Congruence Index c according to Brown & Gore (1994)
#' @keywords congruence
#' @export con_brown_c_holland
#' @description This function computes an index od congruence according to Brown & Gore (1994). 
#' @details The function finds the congruence according to Brown & Gore (1994) between the three-letter Holland-codes given in argument a, which is the person code, and argument b, which is the environment code. The Index is (currently) only defined for three letters from the Holland code. The degree of congruence is output, according to its definition by Brown & Gore (1994), as a reciprocal value of a distance. This means, for example, that a value of '18' is the result for a perfect fit !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Brown & Gore (1994). An Evaluation of interest congruence indices: Distribution Characteristics and Measurement Properties. \emph{Journal of Vocational Behaviour, 45}, 310-327.
#' @examples 
#' con_brown_c_holland(a="RIA",b="SEC") # max. difference 
#' con_brown_c_holland(a="RIA",b="RIA") # max. similarity
################################################################################

con_brown_c_holland<-function(a,b){
 # kongruenzindex nach Brown & Gore (1994) 
 # zur Bestimmung der Übereinstimmung für vector a = Personencodes mit 
 # dem Vector b = Umweltcode diese Reihenfolge ist bei der Eingabe W I C H T I G  !!!
 # (vgl. Brown & Gore 1994. An Evaluation of interest congruence Indicees: Distribution Characteristics and Measurement Properties. Journal of Vocational Behaviour, 45, 310-327. )  
 #func. by: jhheine@googlemail.com 
 a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
 b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
 if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
 if(length(a) > 3 ) stop("a Brown-Index is only defined for three-letter Holland-code")
 ### Hilfsfunktion
 einsbis6<-function(n){temp<- n%%6 ; if(temp==0){temp<-6} ;return(temp)}
 ### Ende Hilfsfunktion 
 ria<-c("R","I","A","S","E","C")
 ### eigentliche Brown index Function
 brw<-function (a,b){
    gesindex<-0
    for (i in 1:3){
      p<-a[i]; u<-b[i]
      index<-0
      if(p==u){index<-3}
      wo<-which(ria==p)
      if(u==ria[einsbis6(wo-1)] | u==ria[einsbis6(wo+1)]){ index<-2 }    
      if(u==ria[einsbis6(wo-2)] | u==ria[einsbis6(wo+2)]){ index<-1 }
      gesindex<-gesindex+index*(4-i)
    }
     return(gesindex) 
  }
 ### Ende eigentliche Brown Funktion
 erg<-brw(a = a, b = b) 
 return (erg)
}
