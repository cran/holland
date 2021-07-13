### Hexagon text radial ------------------------------------------------------
# func. by: jhheine@googlemail.com 
# r <- 5 # radius
# s <- 0 # start in 1 grad Schritten
i.hexa_text_radial <- function(x.cent = 0, y.cent = 0, r=4, s=0, labels=c("R","I","A","S","E","C"), adj=NULL, pos = NULL, offset = 0.5, vfont = NULL, cex = 1, col = NULL, font = NULL, ...){
  H<-seq( (rad(0+s)),(rad(300+s)), length.out=6)# ;H
  r <- rep(r,length.out=6)# recycling of r
  Hxx <- x.cent + r*sin( H )#; Hxx
  Hyy <- y.cent + r*cos( H )#; Hyy
  text(x = Hxx, y = Hyy, labels=labels, adj=adj, pos = pos, offset = offset, vfont = vfont, cex = cex, col = col, font = font, ...)
}
