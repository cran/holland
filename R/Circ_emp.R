#' @title Running Mplus for empirical RIASEC angular locations
#' @keywords calculus
#' @export Circ_emp
#' @description This function generates and runs Mplus syntax which is discussed in Nagy et. al. (2009) and extracts the estimated model parameters - the angular locations of the six RIASEC dimensions, based on their correlations, in the asumed circumplex array within the theory of vocational choice by John Holland (1997).
#' @details more to come ...
#' 
#' @param N number of observations for correlations as numeric
#' @param Cor either an R matrix object with RIASEC correlations or the name of a correlation matrix, stored as a .dat text file, as character. If parameter \code{Cor} is a matrix then correlation will be saved as .dat file in current workspace directory. 
#' @param konstrukt optionaly a character vector with length = 6 containing labels for construct dimensions - default is konstrukt = c("R","I","A","S","E","C"). 
#' @param showOutput default set to \code{TRUE} - see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param Mplus_command default set to \code{"Mplus"} as in the default in \code{\link[MplusAutomation]{MplusAutomation}} - see examples and see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param replaceOutfile default set to "always" - see description of the function \code{runModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' @param name optional a name (as character) for the Mplus syntax to be saved - default is the object name or the name given in paramter \code{Cor} as character (with ending changed to '.inp').
#' @param ... additional parameters passed through
#' @return returns a list object containing the empirical RIASEC angular locations extracted from the Mplus result file after running the Mplus syntax.
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples # generating runing and extracting parameters from Mplus files 
#' # refering to a correlation data stored in object ‘example2'
#' data(example2)# loading fictional example correlation matrix
#' # not Run until Mplus is installed on your Sytem #####
#' \dontrun{test <- Circ_emp(300,example2) 
#' test
#' ### ploting the result as a circumplex
#' plot(test)
#' # for black and white printing
#' plot(test,ltype=c(1,2),lcolor=c("grey","grey","black","black"))}
#' 
############## funktions beginn ####
#func. by: jhheine@googlemail.com 
Circ_emp <-function(N,Cor, konstrukt = c("R","I","A","S","E","C"),showOutput=TRUE,Mplus_command="Mplus", replaceOutfile= "always",name=NULL,...){
  
  if(any(class(Cor)=="character")){
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat") {
      stop("name for correlation matrix should end with '.dat'","\n")
      #cat("name for correlation matrix should end with '.dat'","\n")
      #stopifnot(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat")
    }
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]=="dat" & is.null(name)) { 
      name<-paste(unlist(strsplit(Cor,".",fixed=TRUE))[1],".inp",sep="")
    }
    if(unlist(strsplit(name,".",fixed=TRUE))[2]!="inp"){
      stop("name for Mplus syntax should end with '.inp'","\n")
      #cat("name for Mplus syntax should end with '.inp'","\n")
      #stopifnot(unlist(strsplit(name,".",fixed=TRUE))[2]=="inp")                  
    }
  }
  
  if(any(class(Cor)=="matrix") ){
    if(is.null(name)){
      name <-  paste(deparse(substitute(Cor)),".inp",sep="")
    }
    if(dim(Cor)[1]!=dim(Cor)[2]){
     stop(paste("correlation matrix '",deparse(substitute(Cor)), "' is not symetric!","\n",sep=""))
      # cat("correlation matrix '",deparse(substitute(Cor)), "' is not symetric!","\n",sep="")
      }
    #stopifnot(dim(Cor)[1]==dim(Cor)[2])
    write_dat(ob=Cor,file = paste(deparse(substitute(Cor)),".dat",sep="") )
    Cor<-paste(deparse(substitute(Cor)),".dat",sep="")
  }
 ##########################
  Mplus_esyn(N=N, Cor=Cor, name = name)
  
  runModels(showOutput=showOutput,replaceOutfile=replaceOutfile, Mplus_command=Mplus_command,...)
  
 return( Mplus_eeal(target=paste(unlist(strsplit(name,".",fixed=TRUE))[1],".out",sep=""), konstrukt = konstrukt)) 
}