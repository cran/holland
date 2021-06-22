#' @title writing R matrix objects as .dat text files
#' @keywords misc
#' @export write_dat
#' @description this function writes R matrix objects as .dat text files to be read by Mplus.
#' @details no details.
#' @param ob the R-object to be written as .dat file.
#' @param file optionally the name of the .dat file as character - default is 'objectname'.dat.
#' @param ... additional parameters passed through.
#' @return a .dat text file by default written in the current workspace directory. 
#' @examples ## writing R-object example1 as example1.dat
#' \dontrun{ 
#' data(example1)
#' write_dat(example1) 
#' }

############## funktions beginn ####
#func. by: jhheine@googlemail.com 
write_dat <-function(ob,file = paste(deparse(substitute(ob)),".dat",sep=""), ...) 
{
  write.table(ob, file = file, row.names = F, col.names = F, sep = "\t")
}
# ende der funktion             
