#' @title Generating Mplus syntax for testing empirical RIASEC angular locations
#' @keywords calculus
#' @export Mplus_tsyn
# @docType function
#' @description This function generates executable Mplus syntax which is discussed in Nagy et. al. (2009). the resulting Mplus syntax estimates the fit of the empirical angular locations of the six RIASEC dimensions, found based on their correlations, in the assumed circumplex array within the theory of vocational choice by John Holland (1997).
#' @details more to come ...
#' 
#' @param N number of observations for correlations as numeric
#' @param Cor either an R matrix object with RIASEC correlations or the name of a correlation matrix, stored as a .dat text file, as character (see details).  
#' @param name optional a name (as character) for the Mplus syntax to be saved - default is the object name or the name given in parameter \code{Cor} as character (with ending changed to '.inp').
#' @param test either character (default test="perfect"), which tests against a perfect circumplex array, or a numeric vector with length = 6 giving the six angular locations (in radians) to test against.
#' @param dummyvorl default is dummyvorl = 1, which results in the Mplus syntax given in Nagy et. al. (2009). !! don't change this !!
#' if changed: \emph{"Alternativ dazu kann hier auch der Name einer zu verwendenden Dummyvorlage angegeben werden die dann eingelesen und verwendet wird. Es empfiehlt sich dringend die interne Syntaxvorlage zu verwenden - es sei den zum weiterentwickeln und Testen der Funktionen"}.
#' @param verbose logical with default set to \code{verbose=FALSE}; if set to \code{verbose=TRUE} the resulting Mplus syntax is \emph{additionally} printed on the console.
#' @param eol character see \code{\link{write.table}} to control the generation of correct line endings in text files on different operating systems.
#' @param ... additional parameters passed through
#' @return resulting Mplus syntax will be saved in the current working directory
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' 
#' @examples 
#' ## generating an example Mplus syntax referring to a R object with correlation data 
#' data(example2) # loading fictional example correlation matrix
#' # generate and write Mplus input file in your workspace directory
#' Mplus_tsyn(N = 300, Cor = example2)
#' ## Mplus syntax is now saved in the current workspace
#' ################################################################
#' ### clean up work directory
#' file.remove("example2.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example2.dat") # remove generated cor. data from work dir.


############## funktions beginn ####
#func. by: jhheine@googlemail.com 
# erzeugen eines Mplus Syntaxes nach Nagy(2009) - test against a perfect circumplex 
Mplus_tsyn <-function(N,Cor,name=NULL, test = "perfect", dummyvorl = 1, verbose=FALSE, eol = "\r\n", ...){

  if(any(class(Cor)=="character")){
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat") { 
      stop("name for correlation matrix should end with '.dat'","\n")
      # cat("name for correlation matrix should end with '.dat'","\n")
      # stopifnot(unlist(strsplit(Cor,".",fixed=TRUE))[2]!="dat")
    }
    if(unlist(strsplit(Cor,".",fixed=TRUE))[2]=="dat" & name=="") { 
      name<-paste(unlist(strsplit(Cor,".",fixed=TRUE))[1],".inp",sep="")
    }
    if(unlist(strsplit(name,".",fixed=TRUE))[2]!="inp"){
      stop("name for Mplus syntax should end with '.inp'","\n")
      # cat("name for Mplus syntax should end with '.inp'","\n")
      # stopifnot(unlist(strsplit(name,".",fixed=TRUE))[2]=="inp")                                                  
    }
  }
  if(any(class(Cor)=="matrix") ){
    if(is.null(name)){
      name <-  paste(deparse(substitute(Cor)),".inp",sep="")
    }
    if(dim(Cor)[1]!=dim(Cor)[2]){
      #cat("correlation matrix '",deparse(substitute(Cor)), "' is not symetric!","\n",sep="")
      stop(paste("correlation matrix '",deparse(substitute(Cor)), "' is not symetric!","\n",sep=""))
      }
    # stopifnot(dim(Cor)[1]==dim(Cor)[2])
    write_dat(ob=Cor,file = paste(deparse(substitute(Cor)),".dat",sep="") )
    Cor<-paste(deparse(substitute(Cor)),".dat",sep="")
  }
 #  print(name) #debug
 # print(Cor) #debug

#} #debug
  
  # version 0.95
    
    # library(MplusAutomation)
    
    ############################## bedeutung der funktionsargumente #############################
    # dummyvorl: default ist dummyvorl = 1 dann wird der funktionsinterne standard-dummy-Mplus-syntax verwendet
    # alternativ dazu kann hier auch der name der zu verwendenden Dummyvorlage angegeben werden
    # name: name des auszugebenden Mplus syntax angeben
    # N: anzahl der Personen
    # Cor: name der korrelationsmatrix (dat datei) 
    # test: default ist test = "perfect" es wird der test gegen das perfekte 
    
    #############################################################################################
        
    # library(MplusAutomation)
    #####version 0.95####### abfragen der beutzereingabe test ############
    testvec <- test  # angabe des Benutzers von Alternativen Winkel fixierungen 
    if (test[1] == "perfect") {testvec <- c(0, 1.04720, 2.09440, 3.14159, 4.18879, 5.23599)} # wenn keine Angabe des Benutzers von Alternativen Winkel fixierungen
    stopifnot(length(testvec) == 6 & class(testvec)=="numeric")
    
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
                                   "            ",
                                   "            ",
                                   
                                   "    Model Constraint:",
                                   "            ! Constraining the sum of factor variances to 1",
                                   "            b0 = 1 - b1 ;",
                                   "            ! Expressing factor loadings as the cosine and sine",
                                   "            ! of their angular locations",
                                   "            ! Test of a perfect Circumplex",
                                   "            c2 = cos(#WW2#); s2 = sin(#WW2#);",
                                   "            c3 = cos(#WW3#); s3 = sin(#WW3#);",
                                   "            c4 = cos(#WW4#); s4 = sin(#WW4#);",
                                   "            c5 = cos(#WW5#); s5 = sin(#WW5#);",
                                   "            c6 = cos(#WW6#); s6 = sin(#WW6#);",
                                   "            ", 
                                   "            ", 
                                   "Output:     samp res stand tech1 tech4;")), ncol=1))
    ############ standard dummy Mplus syntax definieren #################################
    # hier wird ein alternativ angegebene Mplus dummy syntax eingelesen falls dummyvorl ungleich 1 ist
    if (dummyvorl != 1){dummy<-read.table(file= dummyvorl, sep = "\t",colClasses = "character")} 
    
    ########version 0.95#########eintragen der Winkelfixierungen testvec in dummy ####################
    dummy$V1 <- gsub(pattern = "#WW2#", testvec[2], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
    dummy$V1 <- gsub(pattern = "#WW3#", testvec[3], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
    dummy$V1 <- gsub(pattern = "#WW4#", testvec[4], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
    dummy$V1 <- gsub(pattern = "#WW5#", testvec[5], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
    dummy$V1 <- gsub(pattern = "#WW6#", testvec[6], x = dummy$V1, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE) # einsetzen des namens der korrelationsmatrix (dat datei)
    
    #####################################
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
    if(verbose==TRUE){print(dummy)} 
    # ab hier das rausschreiben des fertig modifizierten syntaxes fuer Mplus
    write.table(dummy, file = name, append = FALSE, quote = F, sep = "",
                eol = eol, na = "NA", dec = ".", row.names = F,
                col.names = F)
  }
  # ende der funktion
