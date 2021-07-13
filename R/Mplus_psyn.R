#' @title Generating Mplus syntax for empirical RIASEC angular locations with projected constructs
#' @keywords calculus
#' @export Mplus_psyn
# @docType function
#' @description This function generates an extended executable Mplus syntax which is discussed in Nagy et. al. (2009).
#' the resulting Mplus syntax estimates the angular locations of the six RIASEC dimensions, based on their correlations, in the asumed circumplex array within the theory of vocational choice by John Holland (1997). in contrast to the function \code{\link{Mplus_esyn}()} and the syntax discussed by Nagy et. al. (2009), additional constructs (correlated to RIASEC dimensions) are 'projeted' into the circular array of the six vocational interest dimensions. 
#' @details for execution of this function it is necessary that you have Mplus (version 6.11 or version 7) installed on your system. The function \code{Mplus_psyn} will read a Mplus output file, located in the current R workspace directory, which is a result of manualy runing an Mplus input file, generatetd by the function \code{\link{Mplus_esyn}()}. The name of the Mplus output data must be specified in the argument \code{mpluserg}.
#' The structure of the correlation 'data' must follow the structure given in the data \code{\link{example3}} in this package - thus the correlation data must start with the additional construct dimensions which should be projected into the RIASEC circumplex.   
#' @param N number of observations for correlations as numeric
#' @param Cor either an R matrix object with RIASEC and additional construct correlations or the name of a correlation matrix, stored as a .dat text file, as character (see details).
#' @param M number of additional construct dimensions to project into the RIASEC circumplex - e.g. \code{M=5} for big-five personality dimensions
#' @param mpluserg name of the Mplus output data (as character - e.g. "myoutput.out") from which the fixation RIASEC-parameters should be read - this is usaly the result of Mplus prosessing an input syntax generated with the function \code{\link{Mplus_esyn}()}.            
#' @param name optional a name (as character) for the Mplus syntax to be saved - default is the object name or the name given in paramter \code{Cor} as character (with ending changed to '.inp').
#' @param dummyvorl default is dummyvorl = 1, which results in the Mplus syntax given in Nagy et. al. (2009). !! don't change this !!
#' if changed: \emph{"Alternatively, the name of an external dummy template to be used can also be specified here, which is then read in and used. It is strongly recommended to use the internal syntax template, since the use of an external dummy template cannot be documented further here."}.
#' @param verbose logical with default set to \code{verbose=FALSE}; if set to \code{verbose=TRUE} the resulting Mplus syntax is \emph{additionally} printed on the console.
#' @param eol character see \code{\link{write.table}} to control the generation of correct line endings in text files on different operating systems.
#' @param ... additional parameters passed through
#' @return resulting Mplus syntax wil be saved in the current working directory
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' ## first preparing an Mplus output data
#' data(example1) # loading fictional example correlation matrix for fixing
#' Mplus_esyn(N = 300, Cor = example1)
#' ## !!! now first open the 'example1.inp' with Mplus and click run !!! 
#' ##### not run until Mplus is installed on your system #####
#' # generating an example Mplus syntax referring to a correlation data 
#' # stored in the already existing file 'example3' 
#' # for projection of 5 personality dimensions into the circumplex
#' data(example3) # loading fictional example correlation matrix with add. constr.
#' \dontrun{ Mplus_psyn(N = 300, Cor = example3,M = 5, mpluserg = "example1.out") }
#' ################################################################
#' ### clean up work directory
#' file.remove("example1.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example1.dat") # remove generated cor. data from work dir.

############## funktions beginn ####
#func. by: jhheine@googlemail.com 
Mplus_psyn <-function(N, Cor, M, mpluserg, name=NULL, dummyvorl = 1, verbose=FALSE, eol = "\r\n", ...){

  if(any(class(Cor)=="character")){
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat") { 
      stop("name for correlation matrix should end with '.dat'","\n")
      #cat("name for correlation matrix should end with '.dat'","\n")
      #stopifnot(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat")
    }
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]=="dat" & name=="") { 
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
      #cat("correlation matrix '",deparse(substitute(Cor)), "' is not symetric!","\n",sep="")
      }
    #stopifnot(dim(Cor)[1]==dim(Cor)[2])
    write_dat(ob=Cor,file = paste(deparse(substitute(Cor)),".dat",sep="") )
    Cor<-paste(deparse(substitute(Cor)),".dat",sep="")
  }
  #  print(name) #debug
  # print(Cor) #debug
  
  #} #debug  
  
  ############################## bedeutung der funktionsargumente #############################
  # dummyvorl: default ist dummyvorl = 1 dann wird der funktionsinterne standard-dummy-Mplus-syntax verwendet
  # alternativ dazu kann hier auch der name der zu verwendenden Dummyvorlage angegeben werden
  
  # name: name des auszugebenden Mplus syntax angeben
  # mpluserg: name der mplus outputdatei aus der die fix parameter ausgelesen werden sollen
  # N: anzahl der Personen
  # Cor: name der korrelationsmatrix (dat datei)
  # M: anzahl der hineinzuprojezierenden dimensionen 
  #############################################################################################
  
  ############ standard dummy Mplus syntax definieren (nach Nagy 2009)#################################
  # einlesen eines dummy syntaxes und modifizieren desselben
  dummy<-as.data.frame(matrix((c("Title:      Circumplex CFA mit AIST  ", "            (AIST fixiert auf Positionen ; MmM dimensionales Konstrukt reinprojeziert)",
                                 "            ! dieser Syntax ist erst nach der R-verarbeitung lauffaehig", 
                                 "Data:       file is KkK;", "            type is fullcorr;", 
                                 "            nobservations are NnN;", "           ", "            ", 
                                 "Variable:   names are", "            DdD ", 
                                 "            R I A S E C ;", "            usevariables are", 
                                 "            UuU ", "            R I A S E C;", 
                                 "            ", "                        ", "Analysis:   type is general;", 
                                 "            estimator is ml;", "            iteration is 2000;", 
                                 "Model:      ! Spezifikation der Skalierungsparameter (Faktoren erster Ordnung)", 
                                 "            ", "            CR by R*CcC;", "            CI by I*CcC;", 
                                 "            CA by A*CcC;", "            CS by S*CcC;", "            CE by E*CcC;", 
                                 "            CC by C*CcC;", "            R@0 I@0 A@0 S@0 E@0 C@0;", 
                                 "            !CR (2);", "            !CI (2);", "            !CA (2);", 
                                 "            !CS (2);", "            !CE (2);", "            !CC (2);", 
                                 "                        ", "            ! Spezifikation der latenten Faktoren", 
                                 "            Fg by CR@1 CI@1 CA@1 CS@1 CE@1 CC@1;", "            Fc by CR@FfF1;", 
                                 "            Fc by CI@FfF2;", "            Fc by CA@FfF3;", "            Fc by CS@FfF4;", 
                                 "            Fc by CE@FfF5;", "            Fc by CC@FfF6;", "            Fs by CR@GgG1;", 
                                 "            Fs by CI@GgG2; ", "            Fs by CA@GgG3; ", 
                                 "            Fs by CS@GgG4; ", "            Fs by CE@GgG5; ", 
                                 "            Fs by CC@GgG6; ", "            ", "            Fg*.40 (b0);", 
                                 "            Fc*.60 (b1);", "            Fs*.60 (b1);", "            ", 
                                 "            ! Fehlervarianzen fixieren, auf Werte die vorher rausgekommen sind;", 
                                 "            CR@EeE1 CI@EeE2 CA@EeE3 CS@EeE4 CE@EeE5 CC@EeE6;", 
                                 "            !Fixierung der latenten Korrelationen auf 0;", "            Fg with Fc@0 Fs@0;", 
                                 "            Fc with Fs@0;", "              ", "            ! hier werden nun die neuen Variablen reinprojeziert;", 
                                 "            ", "                              ", "            ", "            !Korrelationen untereinander;", 
                                 "                              ", 
                                 "            ", "Output:     samp res stand tech1 tech4;")), ncol=1))
  ############ standard dummy Mplus syntax definieren #################################
  # hier wird ein alternativ angegebene Mplus dummy syntax eingelesen falls dummyvorl ungleich 1 ist
  if (dummyvorl != 1){dummy<-read.table(file= dummyvorl, sep = "\t",colClasses = "character")} 
  
  
  #####################################
  # DdD
  D <- paste("dim", 1:M, sep = "", collapse = " ") # erzeugen des namensvectors fuer die hineinzuprojezierenden Variablen
  dummy$V1 <- gsub(pattern = "DdD", D, x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # einsetzen der namen der hineinzuprojezierenden dimensionen in names are ...
  # UuU
  dummy$V1 <- gsub(pattern = "UuU", D, x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # einsetzen der namen der hineinzuprojezierenden dimensionen in use var...
  # MmM
  dummy$V1 <- gsub(pattern = "MmM", M, x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # einsetzen der anzahl der hineinzuprojezierenden dimensionen
  # KkK
  dummy$V1 <- gsub(pattern = "KkK", Cor, x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
  # NnN
  dummy$V1 <- gsub(pattern = "NnN", N, x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # einsetzen der anzahl der Personen
  #####################################
  dummy$V1 <- gsub(pattern = "! dieser Syntax ist erst nach der R-verarbeitung lauffaehig", "! dieser Syntax ist jetzt in Mplus lauffaehig", x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # user mitteilung das der Mplus syntax jetzt OK ist
  #####################################
  
  # zerlegen von dummy in teile
  dummy1<- matrix(dummy[1:62,],ncol=1) # erster teil                 
  dummy2<- matrix(dummy[64:65,],ncol=1) # zweiter teil                 
  dummy3<- matrix(dummy[67:68,],ncol=1) # dritter teil                 
  
  # erstellen des zus‰tzlich Mplus syntaxes zum reinprojezieren                 
  # rein:
  d <- paste("dim", 1:M, sep = "") # erzeugen des einzel namensvectors fuer die hineinzuprojezierenden Variablen                 
  d<-as.list(d)  
  d <- (unlist(lapply(d, function(x){paste("           ",x, "with Fg Fc Fs;")       } ))) # xxxxx               
  rein <-as.data.frame(matrix(d, ncol=1))
  # ende von rein 
  # koru
  kk <- paste("dim", 1:(M-1), sep = "") # erzeugen des einzel namensvectors fuer die hineinzuprojezierenden Variablen                 
  kkk <- paste("dim", 2:(M), sep = "") # erzeugen des einzel namensvectors fuer die hineinzuprojezierenden Variablen                 
  ##### erstellen einer diagonalliste mit schleife
  ko <- list()
  for (i in (M-1):1) {ko[[(i)]] <- paste((kkk[i:(M-1)]), collapse = " ")} 
  ##### ende erstellen einer diagonalliste mit schleife
  ko<-unlist(ko)               
  with <- rep.int("with", (M-1))
  leer <- rep.int("           ", (M-1))
  semi <- rep.int(";", (M-1))
  koru <- matrix(c(leer,kk,with,ko,semi),ncol=5)
  koru <- as.data.frame(matrix(apply(koru, c(1), function(x){paste(x , collapse = " ")}),ncol=1))
  # ende von koru
  
  dummy<-as.data.frame(rbind(dummy1, rein, dummy2, koru, dummy3)) # alles zusammensetzen                 
  
  
  ############################## jetzt wird dummmy mit den fixierungen versehen aus den Mplus ergebnissen #######################################                 
  
  temp<-readModels(target = mpluserg,what="parameters", ...)$parameters # hier werden die Mplus Ergebnisse (fix parameter) in R eingelesen.
  
  
  #############
  dummy$V1 <- gsub(pattern = "CcC", temp$unstandardized[(temp$unstandardized$paramHeader=="CR.BY" & temp$unstandardized$param=="R"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse CcC
  ###################
  dummy$V1 <- gsub(pattern = "FfF1", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CR"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF1
  dummy$V1 <- gsub(pattern = "FfF2", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CI"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF2
  dummy$V1 <- gsub(pattern = "FfF3", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CA"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF3
  dummy$V1 <- gsub(pattern = "FfF4", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CS"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF4
  dummy$V1 <- gsub(pattern = "FfF5", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CE"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF5
  dummy$V1 <- gsub(pattern = "FfF6", temp$unstandardized[(temp$unstandardized$paramHeader=="FC.BY" & temp$unstandardized$param=="CC"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse FfF6
  ##################                 
  ###################
  dummy$V1 <- gsub(pattern = "GgG1", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CR"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG1
  dummy$V1 <- gsub(pattern = "GgG2", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CI"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG2
  dummy$V1 <- gsub(pattern = "GgG3", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CA"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG3
  dummy$V1 <- gsub(pattern = "GgG4", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CS"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG4
  dummy$V1 <- gsub(pattern = "GgG5", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CE"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG5
  dummy$V1 <- gsub(pattern = "GgG6", temp$unstandardized[(temp$unstandardized$paramHeader=="FS.BY" & temp$unstandardized$param=="CC"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse GgG6
  ##################
  ###################
  dummy$V1 <- gsub(pattern = "EeE1", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CR"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE1
  dummy$V1 <- gsub(pattern = "EeE2", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CI"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE2
  dummy$V1 <- gsub(pattern = "EeE3", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CA"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE3
  dummy$V1 <- gsub(pattern = "EeE4", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CS"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE4
  dummy$V1 <- gsub(pattern = "EeE5", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CE"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE5
  dummy$V1 <- gsub(pattern = "EeE6", temp$unstandardized[(temp$unstandardized$paramHeader=="Residual.Variances" & temp$unstandardized$param=="CC"),3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) # ersetzen der ergebnisse EeE6
  ##################
  #### puhhh das wars
  if(verbose==TRUE){print(dummy)} 
    # ab hier das rausschreiben des fertig modifizierten syntaxes fuer Mplus
  write.table(dummy, file = name, append = FALSE, quote = F, sep = "",
              eol = eol, na = "NA", dec = ".", row.names = F,
              col.names = F)
}

# aufrufen:pro.genMplus.syn(mpluserg = "xxx.out" , name = "xxx.inp", N = xxx, Cor = "xxx.dat", M = x)
# ende der funktion             
