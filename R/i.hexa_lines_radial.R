### Hexagon linien radial ------------------------------------------------------
# func. by: jhheine@googlemail.com 
# braucht die funktion 'rad()'
i.hexa_lines_radial <- function(x.cent = 0, y.cent = 0, r=4, s=0, col="grey", lwd=1, lty=1, ...){
  H<-seq( (rad(0+s)),(rad(300+s)), length.out=6)# ;H
  r <- rep(r,length.out=6)# recycling of r
  Hxx <- x.cent + r*sin( H ); Hxx
  Hyy <- y.cent + r*cos( H ); Hyy
  segments(x0 = rep(x.cent,6),y0 = rep(y.cent,6), x1 = Hxx,y1 = Hyy,col = col,lty = lty,lwd = lwd, ...)
}