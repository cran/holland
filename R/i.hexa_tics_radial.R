### Hexagon measure radial -----------------------------------------------------
# func. by: jhheine@googlemail.com 
# depends on 'i.eli_ines_seg'
# m <- .5 # Abstand der tic marks 
# pos <- c("c","l","r") # tic marks von der radialen linie center links rechts
# wid <- 6 # Breite der tic marks
i.hexa_tics_radial <- function(x.cent = 0, y.cent = 0, ri=.5, ro=4, s=0, m=NULL ,pos=c("c","l","r"), wid=6, col="grey", lwd=1, lty=1, ...){
  # H <- seq(0,360,length.out=7)+s
  type <- match.arg(pos)
  H <- seq( ( (0+s)),( (300+s)), length.out=6) #- sec/2# ;H
  ro <- rep(ro,length.out=6)# recycling of ro
  ri <- rep(ri,length.out=6)# recycling of ri
  wid <- rep(wid,length.out=6)# recycling of wid
  col <- rep(col,length.out=6)# recycling of col
  lwd <- rep(lwd,length.out=6)# recycling of lwd
  lty <- rep(lty,length.out=6)# recycling of lty
  if(type=="c"){
    H0 <- H - wid/2
    H1 <- H + wid/2
  }
  if(type=="l"){
    H0 <- H - wid
    H1 <- H 
  }
  if(type=="r"){
    H0 <- H 
    H1 <- H + wid
  }
  seg <- as.list(as.data.frame(t(cbind(H0,H1))))
  if(is.null(m)){
    mapply(FUN = function(SEG,R,COL, LWD, LTY){i.eli_ines_seg(x.cent=x.cent,y.cent=y.cent, xb=R, yh=R, seg=SEG, col=COL, lwd=LWD, lty=LTY) }, SEG=seg,R=as.list(ro),COL=as.list(col), LWD=as.list(lwd), LTY=as.list(lty) )
  }
  
  if(!is.null(m)){
    scaleM <- mapply(function(RI, RO){seq(RI,RO, by=m) }, RI=ri, RO=ro, SIMPLIFY = FALSE)
    
    for (d in 1:6){
      for (i in 1:length(scaleM[[d]])){
        
        i.eli_ines_seg(x.cent=x.cent,y.cent=y.cent, xb=scaleM[[d]][i], yh=scaleM[[d]][i], seg=seg[[d]], col=col[[d]], lwd=lwd[[d]], lty=lty[[d]])
      }
    }
    
  }
}