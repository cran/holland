### Elipse / Kreis -------------------------------------------------------------
# func. by: jhheine@googlemail.com 
i.eli_ines_tot <-function(x.cent = 0, y.cent = 0, xb=4, yh=4, nseg=720, col="grey", lwd=1, lty=1, ...){
  # plotten einer elipse (Kreis)
  #nseg=360
  xx <- x.cent + xb*sin( seq(0,2*pi, length.out=nseg) )
  yy <- y.cent + yh*cos( seq(0,2*pi, length.out=nseg) )
  lines(xx,yy,col=col,lwd=lwd,lty=lty, ...) 
}