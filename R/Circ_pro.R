#' @title Running Mplus for empirical RIASEC and additional construct angular locations
#' @keywords calculus
#' @export Circ_pro
#' @description This function generates and runs Mplus syntax which is discussed in Nagy et. al. (2009) and extracts the estimated model parameters - the angular locations of the six RIASEC dimensions and angular locations for the additional (projected) construct, based on their correlations, in the asumed circumplex array within the theory of vocational choice by John Holland (1997).
#' @details This function uses the function \code{extractModelParameters()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#'
#' By deault the labels of the dimensions are taken from the column / row names of the matrix objekt given in \code{Cor}. If there are no named columns / rows, dimension lables are created. 
#' 
#' more to come ... 
#' 
#' @param N number of observations for correlations as numeric
#' @param Cor must be an R matrix object with RIASEC and additional construct correlations. The order of the entrys of the correlation matrix must start with the additional construct dimensions - thus the last six collums (rows) in the correlation matrix are the six RIASEC dimensions.
#' @param M number of additional construct dimensions to project into the RIASEC circumplex - e.g. \code{M=5} for big-five personality dimensions - at default \code{M} is calculated from dimensions of \code{Cor}.            
#' @param showOutput default set to \code{TRUE} - see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param Mplus_command default set to \code{"Mplus"} as in the default in \code{\link[MplusAutomation]{MplusAutomation}} - see examples and see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param replaceOutfile default set to "always" - see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param ... additional parameters passed through 
#' @return returns a list object containing the empirical RIASEC and additional construct angular locations extracted from the Mplus result file after running the Mplus syntax. 
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' # generating runing and extracting parameters from Mplus files 
#' # refering to a R object (example4) with correlation data 
#' data(example4) # loading fictional example correlation matrix
#' # not Run until Mplus is installed on your Sytem #####
#' \dontrun{test <- Circ_pro(300,example4) 
#' test
#' ### ploting the result as a circumplex
#' plot(test)
#' # for black and white printing
#' plot(test,ltype=c(1,2),lcolor=c("grey","grey","black","black"))}

#func. by: jhheine@googlemail.com 
Circ_pro<-function(N, Cor, M=dim(Cor)[1]-6,showOutput=TRUE,Mplus_command="Mplus", replaceOutfile= "always", ...){

  stopifnot(any(class(Cor)=="matrix"))
  stopifnot(dim(Cor)[1]==dim(Cor)[2])
  if ( is.null(dimnames(Cor))==FALSE ){dimensions<-dimnames(Cor)[[1]]}
  if ( is.null(dimnames(Cor))==TRUE ){dimensions<-c( paste("dim",1:M,sep=""),c("R","I","A","S","E","C") )}
  stopifnot(length(dimensions)==dim(Cor)[1])
  dimnames(Cor)<-list(dimensions,dimensions)

  tempRIASEC<-Cor[(M+1):dim(Cor)[1],(M+1):dim(Cor)[2]] # Riasec korrelationen
  #schritt 1 empirische winkel 
  Mplus_esyn(N=N,Cor=tempRIASEC) # syntax
  runModels(showOutput=showOutput,replaceOutfile=replaceOutfile, Mplus_command=Mplus_command,...)
  empirisch <- Mplus_eeal(target="tempRIASEC.out", konstrukt = dimnames(tempRIASEC)[[1]]) # auslesen
  
  ########
  tempPROJEKT<-Cor
  #schritt 2 projektion
  Mplus_psyn(N=N, Cor=tempPROJEKT, M=M, mpluserg=empirisch$Mplus.datei) # syntax
  runModels(showOutput=showOutput,replaceOutfile=replaceOutfile, Mplus_command=Mplus_command,...)
  projektion<-Mplus_epal(target="tempPROJEKT.out", M=M, konstrukt = dimensions[1:M])
  
  # ausgabe
  ergebnis<-list(empirisch=empirisch, projektion=projektion)
  class(ergebnis) <- c("proCirc","list")
  return (ergebnis)
  
}
