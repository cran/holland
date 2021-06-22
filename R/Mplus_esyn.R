#' @title Generating Mplus syntax for empirical RIASEC angular locations
#' @keywords calculus
#' @export Mplus_esyn
# @docType function
#' @description This function generates executable Mplus syntax which is discussed in Nagy et. al. (2009). the resulting Mplus syntax estimates the angular locations of the six RIASEC dimensions, based on their correlations, in the assumed circumplex array within the theory of vocational choice by John Holland (1997).
#' @details by default the Mplus syntax file is written in the current working directory.
#' 
#' @param N number of observations for correlations as numeric
#' @param Cor an R matrix object with RIASEC correlations.
#' @param name optional a name (as character) for the Mplus syntax to be saved - default is the object name or the name given in paramter \code{Cor} as character (with ending changed to '.inp').
#' @param dummyvorl default is dummyvorl = 1, which results in the Mplus syntax given in Nagy et. al. (2009). !! don't change this !!
#' if changed: \emph{"Alternatively, the name of an external dummy template to be used can also be specified here, which is then read in and used. It is strongly recommended to use the internal syntax template, since the use of an external dummy template cannot be documented further here."}.
#' @param verbose logical with default set to \code{verbose=FALSE}; if set to \code{verbose=TRUE} the resulting Mplus syntax is \emph{additionally} printed on the console.
#' @param eol character see \code{\link{write.table}} to control the generation of correct line endings in text files on different operating systems.
#' @param ... additional parameters passed through
#' @return resulting Mplus syntax will be saved in the current working directory
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' # generating an example Mplus syntax refering to a R object 
#' # with correlation data 
#' data(example1) # loading fictional example correlation matrix
#' Mplus_esyn(N = 300, Cor = example1)
#' ## Mplus syntax is now saved in the current workspace
#' ################################################################
#' ### clean up work directory
#' file.remove("example1.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example1.dat") # remove generated cor. data from work dir.

############## funktions beginn ####
#func. by: jhheine@googlemail.com 
# erzeugen eines Mplus Syntaxes nach Nagy(2009) - berechnen der empirischen winkel
Mplus_esyn <-function(N,Cor,name=NULL,dummyvorl = 1, verbose=FALSE, eol = "\r\n", ...){
  
  if(any(class(Cor)=="character")){
    if(unlist(strsplit(Cor,".",fixed=T))[2]!="dat") {
      stop("name for correlation matrix should end with '.dat'","\n")
      #cat("name for correlation matrix should end with '.dat'","\n")
      #stopifnot(unlist(strsplit(Cor,".",fixed=T))[2]!="dat")
    }
    if(unlist(strsplit(Cor,".",fixed=T))[2]=="dat" & is.null(name) ) {
      name<-paste(unlist(strsplit(Cor,".",fixed=T))[1],".inp",sep="")
    }
    if(unlist(strsplit(name,".",fixed=T))[2]!="inp"){
      stop("name for Mplus syntax should end with '.inp'","\n")
      #cat("name for Mplus syntax should end with '.inp'","\n")
      #stopifnot(unlist(strsplit(name,".",fixed=T))[2]=="inp")
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
   #print(name) #debug
   #print(Cor) #debug

#} #debug
# library(MplusAutomation)
  
  ############################## bedeutung der funktionsargumente #############################
  # dummyvorl: default ist dummyvorl = 1 dann wird der funktionsinterne standard-dummy-Mplus-syntax verwendet
  # alternativ dazu kann hier auch der name der zu verwendenden Dummyvorlage angegeben werden
  # name: name des auszugebenden Mplus syntax angeben
  # N: anzahl der Personen
  # Cor: name der korrelationsmatrix (dat datei) 
  #############################################################################################
  
 # library(MplusAutomation)
  ############ standard dummy Mplus syntax definieren (nach Nagy 2009)#################################
  dummy<-as.data.frame(matrix((c("Title:      Circumplex CFA mit AIST  ",
                                 "            (Keine Restriktionen)",
                                 "            ! dieser Syntax ist erst nach der R-verarbeitung lauffaehig",
                                 "Data:       file is KkK;",
                                 "            type is fullcorr;",
                                 "            nobservations are NnN;",
                                 "            ",
                                 "            ",
                                 "Variable:   names are",
                                 "            R I A S E C ;",
                                 "            usevariables are",
                                 "            R I A S E C;",
                                 "            ",
                                 "            ",
                                 "Analysis:   type is general;",
                                 "            estimator is ml;",
                                 "            iteration is 2000;",
                                 "            ",
                                 "Model:      ! (1) First order Factors and Scaling Parameters (Faktoren erster Ordnung)",
                                 "            CR by R *.6 (1);",
                                 "            CI by I *.6 (1);",
                                 "            CA by A *.6 (1);",
                                 "            CS by S *.6 (1);",
                                 "            CE by E *.6 (1);",
                                 "            CC by C *.6 (1);",
                                 "            R-C@0;",
                                 "            ",
                                 "            ! (2) Definition of second order factors Fg, Fc, and Fs (Spez. lat. Fakt.)",
                                 "            Fg by CR@1 CI@1 CA@1 CS@1 CE@1 CC@1;",
                                 "            Fc by CR@1;",
                                 "            Fc by CI* 0.50 (c2);",
                                 "            Fc by CA*-0.50 (c3);",
                                 "            Fc by CS*-1.00 (c4);",
                                 "            Fc by CE*-0.50 (c5);",
                                 "            Fc by CC* 0.50 (c6);",
                                 "            Fs by CR@0;",
                                 "            Fs by CI* 0.87 (s2);",
                                 "            Fs by CA* 0.87 (s3);",
                                 "            Fs by CS* 0.00 (s4);",
                                 "            Fs by CE*-0.87 (s5);",
                                 "            Fs by CC*-0.87 (s6);",
                                 "            ! (3) Variance and covariance terms for Fg, Fc, and",
                                 "            ! Fs (beta s)",
                                 "            Fg*.50 (b0);",
                                 "            Fc*.50 (b1);",
                                 "            Fs*.50 (b1);",
                                 "            Fg with Fc@0 Fs@0;",
                                 "            Fc with Fs @0;",
                                 "            ! (4) Placeholder variables for angular locations (fuer winkelpositionen)",
                                 "            PI by CI@0;",
                                 "            PA by CA@0;",
                                 "            PS by CS@0;",
                                 "            PE by CE@0;",
                                 "            PC by CC@0;",
                                 "            PI*1.05 (p2);",
                                 "            PA*2.09 (p3);",
                                 "            PS*3.14 (p4);",
                                 "            PE*4.19 (p5);",
                                 "            PC*5.24 (p6);",
                                 "            PI-PC with PA-PC@0;",
                                 "            PI-PC with Fg@0 Fc@0 Fs@0;",
                                 "    Model Constraint:",
                                 "            ! Constraining the sum of factor variances to 1",
                                 "            b0 = 1 - b1 ;",
                                 "            ! Expressing factor loadings as the cosine and sine",
                                 "            ! of their angular locations",
                                 "            c2 = cos(p2); s2 = sin(p2);",
                                 "            c3 = cos(p3); s3 = sin(p3);",
                                 "            c4 = cos(p4); s4 = sin(p4);",
                                 "            c5 = cos(p5); s5 = sin(p5);",
                                 "            c6 = cos(p6); s6 = sin(p6);",
                                 "            ", 
                                 "            ", 
                                 "Output:     samp res stand tech1 tech4;")), ncol=1))
  ############ standard dummy Mplus syntax definieren #################################
  # hier wird ein alternativ angegebene Mplus dummy syntax eingelesen falls dummyvorl ungleich 1 ist
  if (dummyvorl != 1){dummy<-read.table(file= dummyvorl, sep = "\t",colClasses = "character")} 
  
  
  #####################################
  # KkK
  dummy$V1 <- gsub(pattern = "KkK", Cor, x = dummy$V1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
  # NnN
  dummy$V1 <- gsub(pattern = "NnN", N, x = dummy$V1, ignore.case = FALSE, perl = FALSE,fixed = FALSE, useBytes = FALSE) # einsetzen der anzahl der Personen
  #####################################
  dummy$V1 <- gsub(pattern = "! dieser Syntax ist erst nach der R-verarbeitung lauffaehig", "! dieser Syntax ist jetzt in Mplus lauffaehig", x = dummy$V1, ignore.case = FALSE, perl = FALSE,fixed = FALSE, useBytes = FALSE) # user mitteilung das der Mplus syntax jetzt OK ist
  #####################################
  if(verbose==TRUE){print(dummy)} 
  # ab hier das rausschreiben des fertig modifizierten syntaxes fuer Mplus
  write.table(dummy, file = name, append = FALSE, quote = F, sep = "",eol = eol, na = "NA", dec = ".", row.names = F,col.names = F)
  # aufrufen:emp.genMplus.syn(name = "xxx.inp", N = xxx, Cor = "xxx.dat", dummyvorl = 1)
}
# ende der funktion             
