#' @title Congruence Index according to Zener & Schnuelle (1976)
#' @keywords congruence
#' @export con_zs_holland
#' @description The function computes the congruence index according to Zener & Schnuelle (1976).  
#' @details The function finds the congruence according to Zener & Schnuelle (1976) between the three-letter Holland-codes given in argument a, which is the person code, and argument b, which is the environment code. The Index as defined by Zener & Schnuelle (1976) targets (only) three letters from the Holland code. The degree of congruence is output, according to its definition by Zener & Schnuelle (1976), as a reciprocal value of a distance. This means, for example, that a value of '6' is the result for a perfect fit of two three-letter codes !
#' @param a a character vector with person Holland codes.
#' @param b a character vector with environment Holland codes.
#' @return a numeric with value for congruence.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Zener, T. B. & Schnuelle, L. (1976). Effects of the self-directed search on high school students. \emph{Journal of Counseling Psychology, 23}(4), 353–359.
#' @examples 
#' con_zs_holland(a="RIA",b="SEC") # max. difference 
#' con_zs_holland(a="RIA",b="RIA") # max. similarity
################################################################################
# func. by joerg-henrik heine jhheine(at)googlemail.com 
con_zs_holland <- function(a,b)
  # kongruenzindex nach Zener-Schnuelle(1976) 
  # zur Bestimmung der Übereinstimmung für jede zeile der matrix X==Personencodes mit 
  # dem Vector V==Umweltcode diese Reihenfolge ist bei der Eingabe W I C H T I G  !!!
  # (vgl. Joerin-Fux, Simone "Persö. & Berufst.: Theor. & Instrum. v. J. Holland")  
  # (vgl. Sageder,J. in Abel, J.& Tarnai, Ch. 19xx)
{
  a <- toupper(unlist(strsplit(a,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  b <- toupper(unlist(strsplit(b,split = "",fixed = TRUE))) # um auch z.B. "RIA" eingeben zu können
  if(length(a) != length(b)) stop("a and b must have the same number of characters in Holland-code")
  if(length(a) > 3) stop("the zs index is in this function limited to three-letter Holland-codes")
  collapse<-function (x, sep = "") {paste(x, collapse = sep)}# abgewandelt aus # library(BBmisc)
  erg1 <- collapse(as.character(charmatch(a,b,nomatch=0 )))

  erg1[erg1=="123"]<-6 # alle 3 gleich (in richtiger reihenfolge)
  erg1[erg1=="120"]<-5 # erste 2 gleich (in richtiger reihenfolge)
  erg1[erg1=="321"]<-4 # 3 gleich bel. reihenfolge (ohne 123)
  erg1[erg1=="312"]<-4 # 3 gleich bel. reihenfolge (ohne 123)
  erg1[erg1=="231"]<-4 # 3 gleich bel. reihenfolge (ohne 123)
  erg1[erg1=="213"]<-4 # 3 gleich bel. reihenfolge (ohne 123)
  erg1[erg1=="132"]<-4 # 3 gleich bel. reihenfolge (ohne 123)
  erg1[erg1=="100"]<-3 # nur erster gleich
  erg1[erg1=="130"]<-2 # ersten 2 gleich bel. reihenfolge (ohne 120) 
  erg1[erg1=="210"]<-2 # ersten 2 gleich bel. reihenfolge (ohne 120)
  erg1[erg1=="230"]<-2 # ersten 2 gleich bel. reihenfolge (ohne 120)
  erg1[erg1=="310"]<-2 # ersten 2 gleich bel. reihenfolge (ohne 120)
  erg1[erg1=="320"]<-2 # ersten 2 gleich bel. reihenfolge (ohne 120)
  erg1[erg1=="100"]<-1 # nur erster an bel. stelle gleich
  erg1[erg1=="200"]<-1 # nur erster an bel. stelle gleich
  erg1[erg1=="300"]<-1 # nur erster an bel. stelle gleich
  erg1[erg1=="000"]<-0 # keine gleich 
  erg1[erg1!="0"&erg1!="1"&erg1!="2"&erg1!="3"&erg1!="4"&erg1!="5"&erg1!="6"]<-0
  erg1<-as.numeric(erg1)
  return (erg1)
}
