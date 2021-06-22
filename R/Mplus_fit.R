#' @title Reading some fit indices from Mplus output
#' @keywords calculus
#' @export Mplus_fit
# @docType function
#' @description This function reads the fit-indices "ChiSq","df","p","CFI","RMSEA","SRMR" and returns them as a list and writes them (optionally) as a csv file into the current workspace directory.
#' @details no details   
#' @param target name of the Mplus output file - e.g. target = "MyMplus.out" for an Mplus output file in the current workspace directory.
#' @param w logical - write fit coefficients as csv table? - default: \code{w = FALSE}.
#' @param ... additional parameters passed through
#' @return a list containing coefficients for model fit and a '.csv' file in the current workspace directory.
#' @examples 
#' ## first prepare an Mplus syntax 
#' data(example2) # loading fictional example correlation matrix
#' Mplus_esyn(N = 300, Cor = example2)
#' ## !!! now first open the data example2.inp with Mplus and click run !!! 
#' ##### not run until Mplus is installed on your system #####
#' \dontrun{Mplus_fit("example2.out")}
#' ################################################################
#' ### clean up work directory
#' file.remove("example2.inp") # remove generated Mplus syntax from work dir.
#' file.remove("example2.dat") # remove generated cor. data from work dir.
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
Mplus_fit<-function (target,w=FALSE,...)
{
  # library(MplusAutomation)
  #temp<-extractModelSummaries(target = target,...)
  temp<-readModels(target = target, what="summaries",...)$summaries
  koef<-c( temp$ Filename,temp$ ChiSqM_Value, temp$ ChiSqM_DF, temp$ ChiSqM_PValue,temp$ CFI, temp$ RMSEA_Estimate, temp$ SRMR)
  nam<-c( "Datei","ChiSq","df","p","CFI","RMSEA","SRMR")  
  Dateiname<-gsub(".out","",temp$ Filename)
  
  if (w==TRUE) {write.table((rbind(nam,koef)), file = paste("FIT", Dateiname, ".csv",sep=""), row.names = FALSE, col.names = FALSE, sep = ";")}
erg<-list(Mplus_file = temp$ Filename, fit = list(Chi = temp$ ChiSqM_Value, df = temp$ ChiSqM_DF, p = temp$ ChiSqM_PValue, CFI = temp$ CFI, RMSEA =temp$ RMSEA_Estimate, SRMR = temp$ SRMR ))
  return(erg)
  } # ende der funktion