#' @title Extracting RIASEC and additional construct angular locations from Mplus output 
#' @keywords calculus
#' @export Mplus_epal
# @docType function
#' @description This function extracts the empirical RIASEC angular locations and angular locations for the additional (projected) construct from an Mplus output file, which is the result of processing the Mplus syntax generated with the function \code{\link{Mplus_psyn}()}. 
#' @details This function uses the function \code{readModels()} in package \code{\link[MplusAutomation]{MplusAutomation}}.
#' 
#' 
#' more to come   
#' @param target name of the Mplus output file name within the working directory as character. May also be a full path or relative path. Example: target = "MyMplus.out"
#' @param M number of additional construct dimensions projected into the RIASEC circumplex - e.g. \code{M=5} for big-five personality dimensions
#' @param konstrukt optionally a character vector with length = M containing labels for construct dimensions. 
#' @param ... additional parameters passed through 
#' @return returns a list object containing the RIASEC and additional construct angular locations extracted from the Mplus output.
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' ## generating an example Mplus syntax refering to a R object with correlation data 
#' data(example1) # loading fictional example correlation matrix
#' Mplus_esyn(N = 300, Cor = example1)
#' ##### not run until Mplus inputfile example1.inp is processed by Mplus #####
#' \dontrun{Mplus_epal("example1.out",M=0)}
#' ################################################################
#' ### clean up work directory
#' file.remove("example1.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example1.dat") # remove generated cor. data from work dir.


Mplus_epal<-function(target, M, konstrukt = "",...)
  {
####### version 1.00 ####################### bedeutung der funktionsargumente #############################
# verbesserte Version gegenueber editiert in Saarbrücken
# target:  name der mplus outputdatei aus der die parameter zur winkelberechnung ausgelesen werden sollen
# M: default=5, wieviele dimensionen das hineinezuprojezierende konstrukt hat (beliebige anzahl)
# konstrukt: default=c("dim1","dim2","dim3","dim4","dim5") optional konstruktbezeichnungen als vektorobjekt mit der laenge M 
  #func. by: jhheine@googlemail.com 
#############################################################################################


if (konstrukt[1]==""){konstrukt<-paste("dim", 1:M, sep = "")} # erstellen der default bezeichnungen mit der laenge M

if(length(konstrukt) != M){print("number of dimension labels dose not match the number of dimensions !")}

temp2 <- readModels(target = target, what="parameters", ...)$parameters # hier werden die Mplus Ergebnisse in R eingelesen.

##### hier noch baustelle 10-06-2021 ggf doch $unstandardized ??? --> fixed / confirmed 20.july.2021

von<-which(temp2$stdyx.standardized$paramHeader=="FG.WITH" & temp2$stdyx.standardized$param=="DIM1") # startpunkt des auslesens
bis<-which(temp2$stdyx.standardized$paramHeader=="DIM1.WITH" & temp2$stdyx.standardized$param=="FS")+((length(konstrukt)*3)-1) # endpunkt des auslesens

i1 <- sapply(paste("DIM",1:length(konstrukt),sep = ""),function(x){which(temp2$stdyx.standardized$paramHeader=="FG.WITH" & temp2$stdyx.standardized$param==x)})
i2 <- sapply(paste("DIM",1:length(konstrukt),sep = ""),function(x){which(temp2$stdyx.standardized$paramHeader=="FC.WITH" & temp2$stdyx.standardized$param==x)})
i3 <- sapply(paste("DIM",1:length(konstrukt),".WITH",sep = ""),function(x){which(temp2$stdyx.standardized$paramHeader==x & temp2$stdyx.standardized$param=="FS")})

# von<-which(temp2$stdyx.standardized$paramHeader=="DIM1.WITH" & temp2$stdyx.standardized$param=="FG") # startpunkt des auslesens
# bis<-which(temp2$stdyx.standardized$paramHeader=="DIM1.WITH" & temp2$stdyx.standardized$param=="FG")+((length(konstrukt)*3)-1) # endpunkt des auslesens
#materg <-matrix(temp2$stdyx.standardized[von:bis,3], nrow=3, dimnames = list( c("Fg","Fc","Fs"),konstrukt))# aufbereiten des ergebnisses als matrix
materg <-matrix(temp2$stdyx.standardized[c(i1,i2,i3),3], nrow=3, dimnames = list( c("Fg","Fc","Fs"),konstrukt))# aufbereiten des ergebnisses als matrix
#### hier weitermachen 20 july 2021 fehler in 59 nur mit Mplus version 7.4  mit Mplus version 8.4 klappt alles ...
# temperg<-list()
# for (i in 0:(M-1)){temperg[[paste("dim", (i+1), sep = "")]] <- temp2$stdyx.standardized[(von+(i*3)):((von+(i*3))+2),3]} # aufbereiten des ergebnisses als liste
temperg1 <- apply(materg, 2, function(x){   ( (atan(x[3]/x[2])+pi ) * 180) /pi })
temperg2 <- apply(materg, 2, function(x){   ( (atan(x[3]/x[2])+pi ) )  })

temperg <- as.list(as.data.frame((materg))) # passt gecheckt!

#temperg1 <- ( (unlist(lapply(temperg, function(x){ atan((x)[3]/(x)[2])+pi } ))) * 180) /pi   # hier die umrechnung in grad 
#temperg2 <- unlist(lapply(temperg, function(x){ atan((x)[3]/(x)[2])+pi } )) # hier die winkel in rad 
# quaderg: bezeichnung der Quadranten (1,2,3 und 4) nach Rechenduden Seite 759
quaderg<-unlist(lapply(temperg, function(x) if ((x)[2]==abs((x)[2])&&(x)[3] == abs((x)[3])) {x[1]<-1} else { if ((x)[2]!=abs((x)[2])&&(x)[3] == abs((x)[3])) {x[1]<-2} else {if ((x)[2]!=abs((x)[2])&&(x)[3] != abs((x)[3])) {x[1]<-3} else {if ((x)[2]==abs((x)[2])&&(x)[3] != abs((x)[3])) {x[1]<-4} } } } ))
names(quaderg) <- konstrukt
names(temperg1) <- konstrukt
names(temperg2) <- konstrukt
################### Logikabfrage sind die Gradangaben im richtigen Quadranten und Korrektur ###################
mtemp<-cbind(quaderg,temperg1)
######### hilfsfunktion matrix in liste deren spalten ################
my.as.list.matrix <- function(mat) {
       if(!is.matrix(mat))stop("Argument must be a matrix")
       n <- NCOL(mat)
       res <- vector(mode="list", length=n)
       for (i in seq(along=res))
         res[[i]] <- mat[,i]
       res
}
######### ende der hilfsfunktion matrix in liste deren spalten ################
listwinkel<-my.as.list.matrix(t(mtemp))
# hilfsfunktion Quadrantenlogik ##### entscheidung nach Rechenduden ##############
ft <-function(x) { if ((x)[1]==1 && (x)[2]>270) {x[2]<-(x[2]-270)}
                   else { if ((x)[1]==1 && (x)[2]>180) {x[2]<-(x[2]-180)}
                          else { if ((x)[1]==1 && (x)[2]> 90) {x[2]<-(x[2]- 90)}        
                                 else { if ((x)[1]==2 && (x)[2]>270) {x[2]<-(x[2]-180)}
                                        else { if ((x)[1]==2 && (x)[2]>180) {x[2]<-(x[2]- 90)}
                                               else { if ((x)[1]==2 && (x)[2]< 90) {x[2]<-(x[2]+ 90)}       
                                                      else { if ((x)[1]==3 && (x)[2]>270) {x[2]<-(x[2]- 90)}
                                                             else { if ((x)[1]==3 && (x)[2]< 90) {x[2]<-(x[2]+180)}
                                                                    else { if ((x)[1]==3 && (x)[2]<180) {x[2]<-(x[2]+ 90)}       
                                                                           else {if ((x)[1]== 4 &&(x)[2] < 90) {x[2]<-x[2]+ 270} 
                                                                                 else {if ((x)[1]== 4 &&(x)[2] < 180) {x[2]<-x[2]+180} 
                                                                                       else {if ((x)[1]== 4 &&(x)[2] < 270) {x[2]<-x[2]+ 90}                   
                                                                                             else  {x[2]<-x[2]}  
                                                                                       } } } } } } } } } } } }
#  ende der hilfsfunktion Quadrantenlogik ##### entscheidung nach Rechenduden ##############
korigwinkel<-unlist(lapply(listwinkel, ft))  
names(korigwinkel) <- konstrukt
################### rad: (x * pi)/180
ergebnis<-list(Mplus.datei = target, M.dimensionen = M, bezeichnung.dimensionen = konstrukt, quadrant.dimension = quaderg, winkel.grad = korigwinkel, winkel.rad = (korigwinkel* pi)/180, unkorig.winkel.grad = temperg1, unkorig.winkel.rad = temperg2 )

class(ergebnis) <- c("epalMplus","list")

return (ergebnis)

# aufrufen: pro.winkel("xxx.out", M = 5, konstrukt = c("Neuro","Extra","Offen","Vertr","Gewis"))
# Ende der Funktion
}
