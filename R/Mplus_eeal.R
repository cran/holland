#' @title Extracting empirical RIASEC angular locations from Mplus output 
#' @keywords calculus
#' @export Mplus_eeal
# @docType function
#' @description This function extracts the empirical RIASEC angular locations from an Mplus output file, which is the result of processing the Mplus syntax generated with the function \code{\link{Mplus_esyn}()}. 
#' @details This function uses the function \code{readModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' more to come   
#' @param target name of the Mplus output file name within the working directory as character. May also be a full path or relative path. Example: target = "MyMplus.out"
#' @param konstrukt optionally a character vector with length = 6 containing labels for construct dimensions - default is konstrukt = c("R","I","A","S","E","C"). 
#' @param ... additional parameters passed through 
#' @return returns a list object containing the RIASEC angular locations extracted from the Mplus output.
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' ## generating an example Mplus syntax referring to a R object with correlation data 
#' data(example1) # loading fictional example correlation matrix
#' Mplus_esyn(N = 300, Cor = example1)
#' ##### not run until Mplus inputfile 'example1.inp' is processed by Mplus ####
#' \dontrun{Mplus_eeal("example1.out")}
#' ################################################################
#' ### clean up work directory
#' file.remove("example1.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example1.dat") # remove generated cor. data from work dir.

############## funktions beginn ####
# extrahieren der ergebnisse aus Mplus- und bestimmen der empirischen winkel im RIASEC Circumplex 
Mplus_eeal <-function(target, konstrukt = c("R","I","A","S","E","C"),... )
{
############################## bedeutung der funktionsargumente #############################
# target:  name der mplus outputdatei aus der die parameter zur winkelbestimmung ausgelesen werden sollen
# konstrukt: default=c("R","I","A","S","E","C") optional (ausfuehrlichere) konstruktbezeichnungen 
# plot: default=FALSE, wenn plot = FALSE dann werden die empirischen vectoren nicht in ein perfektes hexagon geplottet  
# titel: default titel = target, wenn nicht anderes angegeben wird bei plot=TRUE target als nanme verwendet 
  #func. by: jhheine@googlemail.com 
#############################################################################################

temp2 <- readModels(target = target, what="parameters", ...)$parameters # hier werden die Mplus Ergebnisse in R eingelesen.
####### hier auslesen der einzelenen werte ##############################
PI<-(temp2$unstandardized[(temp2$unstandardized$paramHeader=="Variances" & temp2$unstandardized$param=="PI"),3]) 
PA<-(temp2$unstandardized[(temp2$unstandardized$paramHeader=="Variances" & temp2$unstandardized$param=="PA"),3])
PS<-(temp2$unstandardized[(temp2$unstandardized$paramHeader=="Variances" & temp2$unstandardized$param=="PS"),3])
PE<-(temp2$unstandardized[(temp2$unstandardized$paramHeader=="Variances" & temp2$unstandardized$param=="PE"),3])
PC<-(temp2$unstandardized[(temp2$unstandardized$paramHeader=="Variances" & temp2$unstandardized$param=="PC"),3])  
temperg2 <- c(0, PI,PA,PS,PE,PC)
names(temperg2) <- konstrukt  
temperg1 <- (temperg2 * 180)/pi
names(temperg1) <- konstrukt 
perf.circ.rad <- c(0, 1.047198, 2.094395, 3.141593, 4.18879, 5.235988)  
names(perf.circ.rad) <- konstrukt
perf.circ.deg <- (perf.circ.rad * 180)/pi
mat<-as.matrix(rbind(temperg2, perf.circ.rad))  
dimnames(mat) <- list(c("empiric.circumplex","perfect.circumplex"),konstrukt)
##################
ergebnis<-list(Mplus.datei = target, emp.winkel.grad = temperg1, emp.winkel.rad = temperg2, perf.circ.deg = perf.circ.deg, perf.circ.rad = perf.circ.rad, mat = mat)

# if(plot==TRUE){emp.plot(mat, Titel=titel, namen.perf = c("R","I","A","S","E","C"))} else{if (plot==FALSE) {return (ergebnis)}}

#
# {print("plot")}
class(ergebnis) <- c("empCirc","list")
return (ergebnis)

}# Ende der Funktion  
 
