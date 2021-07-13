### Hexagon linien circular ----------------------------------------------------
# func. by: jhheine@googlemail.com 
# braucht die funktion 'rad()'
i.hexa_lines_circular <- function(x.cent = 0, y.cent = 0, r=4, s=0, col="grey", lwd=1, lty=1, ...){
  H0<-seq( (rad(0+s)),(rad(300+s)), length.out=6)# ;H
  H1<-seq( (rad(60+s)),(rad(360+s)), length.out=6)# ;H
  r0 <- rep(r,length.out=7)[1:6]# recycling of r
  r1 <- rep(r,length.out=7)[2:7]# recycling of r
  H0x <- x.cent + r0*sin( H0 )
  H0y <- y.cent + r0*cos( H0 )
  H1x <- x.cent + r1*sin( H1 )
  H1y <- y.cent + r1*cos( H1 )
  segments(x0 = H0x,y0 = H0y,x1 = H1x,y1 = H1y,col = col,lty=lty,lwd=lwd, ...)
}