### Segment von Elipse / Kreis -------------------------------------------------
# func. by: jhheine@googlemail.com 
# braucht die funktion 'rad()'
# seg # sektror in grad von bis nur positive werte
i.eli_ines_seg <-function(x.cent = 0, y.cent = 0, xb=4, yh=4, seg=c(0,30), nseg=720, col="grey", lwd=1, lty=1, ...){
  # plotten einer elipse (Kreis)
  # seg <- c(-10,20)
  #seg_ <- sapply(seg,function(x){if(sign(x)==-1){x <- 360+x}; x })
  sektor <- seq(from=seg[1],to=seg[2],by=360/nseg)
  #sektor <- do.call(c,sapply(seg,function(x){if(sign(x)==-1){r <- (360+x):360}else{r <- 1:x}; r }))
  # kreis <- seq(0,2*pi, length.out=nseg)
  xx <- (x.cent + xb*sin( rad(sektor) ))
  yy <- (y.cent + yh*cos( rad(sektor) ))
  lines(xx,yy,col=col,lwd=lwd,lty=lty, ...) 
}