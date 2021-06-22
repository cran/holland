#' @exportS3Method plot empCirc
#' @keywords calculus
#' @method plot empCirc
#' @title S3 plotting method for object of class \code{"empCirc"}
#' @description plotting method for object of class 'empCirc' which results from function \code{\link{Circ_emp}} and contains the empirical RIASEC angular locations extracted from the Mplus result file after running the Mplus syntax.
#' @details no details
#' @param x object of class 'empCirc'.
#' @param main titel for plot.
#' @param lcolor (character) vector for colors; default set to \code{lcolor=c("blue","blue","red","red")}.
#' @param ltype vector for line types; default set to \code{ltype=c(1,2)}
#' @param lwd numeric defining the line width; default set to \code{lwd=3}
#' @param defhexa list of definition to draw the hexagon 
#' @param ... other parameters passed through
#' @return a plot visualizing the empirical RIASEC angular locations within the Hexagon.
#' 
########################### hier die plot method #############################
plot.empCirc<-function(x,main=NULL, lcolor=c("blue","blue","red","red"), ltype=c(1,2), lwd=3, defhexa= list(hexa=TRUE, seg=TRUE,gr=5,r=4,nseg=7,x.cent=0,y.cent=0), ...) 
{
  ############################## bedeutung der funktionsargumente ###########################
  ###########################################################################################
   if(is.null(main)){main<-paste( "angular locations for ",deparse(substitute(x)),sep="")}
  #if(main=="def"){main<-paste("plot for",x$empirisch$Mplus.datei,x$projektion$Mplus.datei,sep=" ")}
  #if(main==""){main<-paste( "angular locations for ",deparse(substitute(x)),sep="")}
  #plot( 1:10,1:10, type="n",asp=1)
  #parameter:-------------------------
  # x<- test # plotting objekt  (ergebnis von Circ_emp)
  # hochkant stehendes Hexagon fest eingestellt Realistic ist oben
  #func. by: jhheine@googlemail.com 
  hexa <- defhexa$hexa
  seg <- defhexa$seg
  gr<-defhexa$gr # grösse der plotting area
  r=defhexa$r #radius des hexagons
  nseg=defhexa$nseg # segmente eigentlich hier konstant, da hexagon
  x.cent <- defhexa$x.cent  # zeichnen in der Mitte 
  y.cent <- defhexa$y.cent  # zeichnen in der Mitte
  # lcolor <- c("red","red","blue","blue","green","green") #farbe: hexag, beschriftunghexagon, empirisch,beschriftungempirisch, projektion, beschriftungprojektion
  # ltype  <- c(2,1,2) # linientyp: hexag, empirisch, projektion
  # seg <- TRUE
  # hexa <- TRUE
  #-----------------------------------
  H<-seq((0),((2*pi)), length.out=nseg) # 0 - 360 grad in 60 
  plot.new()
  plot.window(xlim=c(-gr,gr), ylim=c(-gr,gr),asp=1,main=main)
  #plot(x=c(-gr,gr), y=c(-gr,gr), type = "n", asp = 1, xlab = "", ylab = "",main=main)
  Hxx <- x.cent + r*sin( H )
  Hyy <- y.cent + r*cos( H )
  # [1] hexagon aussenlinien plotten ------------
  if(hexa==TRUE){ 
    lines(Hxx,Hyy, col=lcolor[1],lty=ltype[1],lwd=lwd) # hexagon aussenlinien plotten
  }
    
  # [1] optional segmente ploten--------------
    if (seg==TRUE){ 
      for (i in 1:(nseg-1)){
        lines(c(x.cent,Hxx[i]),c(y.cent,Hyy[i]), col=lcolor[1],lty=ltype[1],lwd=lwd) # 
      }}
    
    # [2] perfekt beschriftungen hinzufügen --------------------
    BP <- colnames(x$mat) # Beschriftung
    BPxx <- x.cent + (r+.4)*sin( H[1:6] )
    BPyy <- y.cent + (r+.4)*cos( H[1:6] )
    text(BPxx,BPyy, labels= BP, col=lcolor[2]) # hexagon beschriftungen plotten col=c("green","blue","yellow","pink","red","black")

  # [3] plotten der empirischen winkel ----------------------
  #x$empirisch$mat[1,] #empirisch x$mat[2,] #perfect
  rwink<-(x$mat[1, ]) #empirische winkel 
  Exx <- x.cent + r*sin((rwink))
  Eyy <- y.cent + r*cos((rwink))
  for (i in 1:6){
    lines(c(x.cent,Exx[i]),c(y.cent,Eyy[i]),col=lcolor[3],lty=ltype[2],lwd=lwd) # empirische linien plotten col=c("green","blue","yellow","pink","red","black")[i] col=lcolor[3]
  }  
  # [4] empirisch beschriftungen hinzufügen -----------------------
  BE <- colnames(x$mat)  # 
  BExx <- x.cent + (r-.7)*sin( (rwink+.1) ) # +.1-> stehen links daneben
  BEyy <- y.cent + (r-.7)*cos( (rwink+.1) ) #
  text(BExx,BEyy, labels= BE, col=lcolor[4]) # empirisch beschriftungen plotten col=c("green","blue","yellow","pink","red","black")
}
