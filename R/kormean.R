#' @title Take the Mean of two Correlation Matrices
#' @keywords misc
#' @export kormean
#' @description This function takes the mean of two correlation matrices using the Fisher-Z transformation of the coefficients in both matrices. 
#' @details this function uses the numerical values given in parameters \code{xn} and \code{yn} to compute the weighted mean of the Fisher-Z transformed coefficients in both correlation matrices. If either parameter \code{xn} or \code{yn} is not assigned a numerical value, the unweighted mean of both matrices is computed.     
#' @param x a correlation matrix
#' @param y a correlation matrix
#' @param xn numeric value (optionally) the number of observations for correlation matrix given in x
#' @param yn numeric value (optionally) the number of observations for correlation matrix given in y
#' @return the mean correlations of both matrices as a matrix object
#' @examples 
#' ## Correlation matrix for overall ASIT norm sample
#' data(AIST_2005_F_1270) # female sub-sample
#' data(AIST_2005_M_1226) # male sub-sample 
#' kormean(x=AIST_2005_F_1270,y=AIST_2005_M_1226,xn=1270,yn=1226)
################# funktionsbegin #########
kormean<-function(x,y,xn=NA,yn=NA)
{
  #func. by: jhheine@googlemail.com 
x=as.matrix(x); y=as.matrix(y)
stopifnot(unique(dim(x))==6)
stopifnot(unique(dim(y))==6)

xz<-(1/2) * log(  (1+x) / (1-x)  ) #Fisher-z transformation
yz<-(1/2) * log(  (1+y) / (1-y)  ) #Fisher-z transformation

if (is.na(xn) | is.na(yn) ) {xn=1;yn=1 } # wenn kein N angegeben Gewichtung 1:1

xyz<- (xz*xn + yz*yn)/(xn+yn)  # mittelung

xyr<-(exp(xyz*2)-1)/(exp(xyz*2)+1) # zurück in korrelationskoeffizienten

diag(xyr)<-1 # diagonale wieder auf 1 setzen 

return(xyr)
}
# ende der funktion