### Hexagon polygon fläche -----------------------------------------------------
# func. by: jhheine@googlemail.com 
# r <- 5 # radius
# s <- 0 # start in 1 grad Schritten
i.hexa_poly <- function(x.cent = 0, y.cent = 0, r=4, s=0, density = NULL, angle = 45, border = NULL, col = NA, lwd=1, lty = par("lty"), fillOddEven = FALSE, ...){
  H<-seq( (rad(0+s)),(rad(300+s)), length.out=6)# ;H
  r <- rep(r,length.out=6)# recycling of r
  Hxx <- x.cent + r*sin( H )#; Hxx
  Hyy <- y.cent + r*cos( H )#; Hyy
  polygon(x = Hxx,y = Hyy, density = density, angle = angle, border = border, col = col, lwd=lwd, lty = lty, fillOddEven = fillOddEven, ...)
}
