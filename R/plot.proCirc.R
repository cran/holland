#' @exportS3Method plot proCirc
#' @keywords calculus
#' @method plot proCirc
#' @title S3 plotting method for object of class \code{"proCirc"}
#' @description plotting method for object of class 'proCirc'.
#' @details more to come ...
#' @param x object of class 'empCirc'.
#' @param main titel for the plot.
#' @param defhexa a list with plotting options for hexagon.
#' @param defemp a list with plotting options for empirical RIASEC angels.
#' @param defcon a list with plotting options for empirical construct angels
#' @param ... other parameters passed through
#' @return a plot visualizing the empirical RIASEC and (projected) additional construct angular locations within the Hexagon.
#'  
########################### hier die plot method #############################
#plot.proCirc<-function(x, hexa=TRUE,names.hexa= c("Rea.","Inv.","Art.","Soc.","Ent.","Con."),main=paste("plot for",x$empirisch$Mplus.datei,x$projektion$Mplus.datei,sep=" "),lwd=3,lty=c("dotted","solid","solid"),line.col=c("red","blue","black"),...)

# x <- cirk_all
# main=NULL
# defhexa= list(r=4, lwd=3, col="red", lty=2, cex=1, col.lab="red", gr=5, hexa=TRUE, seg=TRUE, nseg=7, col.seg="red", x.cent=0, y.cent=0)
# defemp= list(r=3, lwd=3, col="blue", lty=1, cex=.8, col.lab="blue")
# defcon= list(r=5, lwd=3, col="green", lty=1, cex=1, col.lab="green" )

plot.proCirc<-function(x,main=NULL, defhexa= list(r=4, lwd=3, col="grey", lty=1, cex=1, col.lab="grey", gr=5, hexa=TRUE, seg=TRUE, nseg=7, col.seg="grey", x.cent=0, y.cent=0), defemp= list(r=3, lwd=3, col="black", lty=2, cex=.8, col.lab="black"), defcon= list(r=4.5, lwd=3, col="darkgray", lty=3, cex=1, col.lab="darkgray" ), ...){
    if(is.null(main)){main<-paste("plot for",x$empirisch$Mplus.datei,x$projektion$Mplus.datei,sep=" ")}
    #if(main==""){main<-paste( "angular locations for ",deparse(substitute(x)),sep="")}
    #plot( 1:10,1:10, type="n",asp=1)
    #parameter übergabe:-------------------------
    # hochkant stehendes Hexagon fest eingestellt Realistic ist oben
  #func. by: jhheine@googlemail.com 
    r.hexa=defhexa$r #radius des hexagons
    lwd.hexa <- defhexa$lwd
    col.hexa <- defhexa$col
    lty.hexa <- defhexa$lty
    cex.hexa <- defhexa$cex 
    col.lab.hexa <- defhexa$col.lab
    gr<-defhexa$gr # grösse der plotting area
    hexa <- defhexa$hexa
    seg <- defhexa$seg
    nseg=defhexa$nseg # segmente eigentlich hier konstant, da hexagon
    x.cent <- defhexa$x.cent  # zeichnen in der Mitte 
    y.cent <- defhexa$y.cent  # zeichnen in der Mitte
    # empirische winkel:
    r.emp <- defemp$r # empirische winkel
    lwd.emp <- defemp$lwd
    col.emp <- defemp$col
    lty.emp <- defemp$lty
    cex.emp <- defemp$cex
    col.lab.emp <- defemp$col.lab
    # konstrukt winkel:
    r.con <- defcon$r # konstrukt winkel
    lwd.con <- defcon$lwd
    col.con <- defcon$col
    lty.con <- defcon$lty
    cex.con <- defcon$cex
    col.lab.con <-defcon$col.lab 

    #-----------------------------------
    H<-seq((0),((2*pi)), length.out=nseg) # 0 - 360 grad in 60 
    plot.new()
    plot.window(xlim=c(-gr,gr), ylim=c(-gr,gr),asp=1)
    Hxx <- x.cent + r.hexa*sin( H )
    Hyy <- y.cent + r.hexa*cos( H )
    # [1] hexagon aussenlinien plotten ------------
    if(hexa==TRUE){ 
    lines(Hxx,Hyy, col=col.hexa,lty=lty.hexa,lwd=lwd.hexa) # hexagon aussenlinien plotten
    }
    # [1] optional segmente ploten--------------
    if (seg==TRUE){ 
      for (i in 1:(nseg-1)){
        lines(c(x.cent,Hxx[i]),c(y.cent,Hyy[i]), col=col.hexa,lty=lty.hexa,lwd=lwd.hexa) }
      }
    
    # [2] perfekt beschriftungen hinzufügen --------------------
    BP <- colnames(x$empirisch$mat) # Beschriftung
    BPxx <- x.cent + (r.hexa+.4)*sin( H[1:6] )
    BPyy <- y.cent + (r.hexa+.4)*cos( H[1:6] )
    text(BPxx,BPyy, labels= BP, col=col.lab.hexa, cex = cex.hexa) # hexagon beschriftungen plotten col=c("green","blue","yellow","pink","red","black")

    # [3] plotten der empirischen winkel ----------------------
    #x$empirisch$mat[1,] #empirisch x$mat[2,] #perfect
    rwink<-(x$empirisch$mat[1, ]) #empirische winkel 
    Exx <- x.cent + r.emp*sin((rwink))
    Eyy <- y.cent + r.emp*cos((rwink))
    for (i in 1:6){
      lines(c(x.cent,Exx[i]),c(y.cent,Eyy[i]),col=col.emp,lty=lty.emp,lwd=lwd.emp) 
    }  
    # [4] empirisch beschriftungen hinzufügen -----------------------
    BE <- colnames(x$empirisch$mat)  # 
    BExx <- x.cent + (r.emp-.7)*sin( (rwink+.1) ) # +.1-> stehen links (innen) daneben
    BEyy <- y.cent + (r.emp-.7)*cos( (rwink+.1) ) #
    text(BExx,BEyy, labels= BE, col=col.lab.emp,cex = cex.emp) # empirisch beschriftungen plotten col=c("green","blue","yellow","pink","red","black")
    
    # [5] plotten der projektion winkel ----------------------
    pwink<-x$projektion$winkel.rad # projektion winkel start korrigiert
    Pxx <- x.cent + r.con*sin(pwink)
    Pyy <- y.cent + r.con*cos(pwink)
    for (i in 1:length(pwink)){
      lines(c(x.cent,Pxx[i]),c(y.cent,Pyy[i]),col=col.con ,lty=lty.con,lwd=lwd.con)
      }  
    # [6] projektion beschriftungen hinzufügen -----------------------
    PE <- names(x$projektion$winkel.rad)
    PExx <- x.cent + (r.con+.3)*sin( (pwink-0) ) # -.1-> stehen rechts (aussen) daneben
    PEyy <- y.cent + (r.con+.3)*cos( (pwink-0) ) 
    text(PExx,PEyy, labels= PE, col=col.lab.con,cex = cex.con) 
}
  
  