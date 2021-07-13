#' @title Plot an interest profile in the Hexagon
#' @keywords misc
#' @export plot_profile_holland
#' @description This function plots an interest profile in the Hexagon 
#' @details no details -- but see arguments.  
#' @param x numeric vector with length of six values; either raw scores or normed values
#' @param gr numeric optional specification of the (quadratic) plotting area.
#' @param x.cent numeric optional specification of the x center of the plot
#' @param y.cent numeric optional specification of the y center of the plot
#' @param center logical whether to re-scale the minimum profile value to be in the center of the Hexagon
#' @param r numeric optional specification of the radius (possibly vector with max length 6 -- will be recycled) of the hexagonal plot (must be then in the same measure scale as \code{x}).
#' @param s integer optional specification of the angular starting position (in degrees) when drawing the Hexagon (from 1 to 360). 
#' @param radial logical whether to plot radial (sector) lines within the Hexagon.
#' @param col.Hr character expression of colors (possibly vector with max length 6 -- will be recycled) for the radial Hexagon lines.
#' @param lwd.Hr numeric (possibly vector with max length 6 -- will be recycled) with line widths for the radial Hexagon lines.
#' @param lty.Hr numeric or character expression (possibly vector with max length 6 -- will be recycled) with line types for the radial Hexagon lines.
#' @param circular logical whether to plot a circular (border) line around the Hexagon.
#' @param col.Hc character expression of colors (possibly vector with max length 6 -- will be recycled) for colors for the circular (border) Hexagon line.
#' @param lwd.Hc numeric (possibly vector with max length 6 -- will be recycled) with line widths for the circular (border) Hexagon line.
#' @param lty.Hc numeric or character expression (possibly vector with max length 6 -- will be recycled) with line types for the circular (border) Hexagon line.
#' @param circle logical whether to plot a circle around the Hexagon.
#' @param col.C character expression for color of circle around Hexagon.
#' @param lwd.C numeric for line width of circle around Hexagon.
#' @param lty.C numeric or character expression for line type of circle around Hexagon.
#' @param measure logical whether to plot a (radial) measure scale (for each Hexagon dimension).
#' @param ri.M numeric (possibly vector with max length 6 -- will be recycled) with numeric value(s) for start (minimum) of the measure scale for each Hexagon dimension respectively (must be then in the same measure scale as \code{x}).
#' @param ro.M numeric (possibly vector with max length 6 -- will be recycled) with numeric value(s) for end (maximum) of the measure scale for each Hexagon dimension respectively (must be then in the same measure scale as \code{x}).
#' @param m numeric with single value for the distance of the tic marks of the measure scale.
#' @param pos.M character one of \code{c("c","l","r")} for the orientation of the tic marks with reference to the radial hexagon lines -- centered, left, right respectively.
#' @param wid.M numeric (possibly vector with max length 6 -- will be recycled) defining the widths of the tic marks in degree.
#' @param col.M character expression of colors (possibly vector with max length 6 -- will be recycled) for the color of the tic marks.
#' @param lwd.M numeric (possibly vector with max length 6 -- will be recycled) for the line widths of the tic marks.
#' @param lty.M numeric or character expression (possibly vector with max length 6 -- will be recycled) for the line types of the tic marks.
#' @param vector logical whether to plot the (resulting) total vector (as arrow) for the interest profile according to Eder, (1998).
#' @param length.V length of the edges of the total vector arrow head (see \code{\link{arrows}}).
#' @param angle.V angle from the shaft of the total vector arrow to the edge of the arrow head (see \code{\link{arrows}}).
#' @param code.V integer code, determining kind of total vector arrow to be drawn(see \code{\link{arrows}}).
#' @param col.V color of the total vector arrow (see \code{\link{arrows}}).
#' @param lty.V line type of the total vector arrow (see \code{\link{arrows}}).
#' @param lwd.V line wide of the total vector arrow (see \code{\link{arrows}}).
#' @param gridl logical whether to plot a circular grid lines at the positions of the tic marks for the measure scale around the Hexagon.
#' @param col.G character expression of colors (possibly vector with max length 6 -- will be recycled) for the color of the grid lines.
#' @param lwd.G numeric (possibly vector with max length 6 -- will be recycled) for the line widths of the grid lines.
#' @param lty.G numeric or character expression (possibly vector with max length 6 -- will be recycled) for the line types of the grid lines.
#' @param scalab logical whether to ad scale labeling with units. 
#' @param adj.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param pos.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param offset.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param vfont.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param cex.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param col.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param font.sl parameter for scale labeling control -- see \code{\link{text}}.
#' @param polyg whether to plot a polygon for the interest profile (see argument \code{x}).
#' @param density.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param angle.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param border.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param col.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param lwd.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param lty.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param fillOddEven.P parameter for polygon drawing control -- see \code{\link{polygon}}.
#' @param lab character vector (with max length 6 -- will be recycled) for labeling the Hexagon dimensions, by default names from \code{x} a taken ... 
#' @param s.la integer to control angular positions of labels (in degrees).
#' @param r.la numeric optional specification of the radial positions (possibly vector with max length 6 -- will be recycled) for the labels of the Hexagon dimensions (must be then in the same measure scale as \code{x}).
#' @param adj.la parameter for labeling control -- see \code{\link{text}}.
#' @param pos.la parameter for labeling control -- see \code{\link{text}}.
#' @param offset.la parameter for labeling control -- see \code{\link{text}}.
#' @param vfont.la parameter for labeling control -- see \code{\link{text}}.
#' @param cex.la parameter for labeling control -- see \code{\link{text}}.
#' @param col.la parameter for labeling control -- see \code{\link{text}}.
#' @param font.la parameter for labeling control -- see \code{\link{text}}.
#' @param ... parameters passed through 
#' @return a plot and if argument \code{vector=TRUE} (default) a numeric value indicating the orientation of the total vector for the interest profile in degrees.
#' @references Eder, F. (1998). Differenziertheit der Interessen als Prädiktor der Interessenentwicklung. In J. Abel & C. Tarnai (Hrsg.), \emph{Pädagogisch-psychologische Interessenforschung in Studium und Beruf} (S. 63–77). Münster: Waxmann.
#' @examples 
#' #### a fictional interest profile:
#' A <- c(95, 125, 122, 105, 100, 90)
#' names(A) <- c("R","I","A","S","E","C")
#' #### plot with default settings
#' plot_profile_holland(x=A)
#' #### easy way to change global scaling with argument 'gr'
#' ### additional change the color and thickness of the polygon line ...
#' plot_profile_holland(x=A,gr=3,border.P="darkblue",lwd.P = 2)
#' #### give the Hexagon a fixed scale range ...
#' plot_profile_holland(x=A,gr=10,ri.M=70,ro.M=130,lwd.P=2)
#' #### center the minimum and addition fix the scale range and step width ...
#' plot_profile_holland(x=A,gr=10,center=TRUE,ri.M=70,ro.M=130,m=10,lwd.P=2)
#' 
#' ############## More examples
#' \dontrun{
#' #### center the minimum without a fixed scale range but with a fixed step width ...
#' plot_profile_holland(x=A,gr=10,center=TRUE,m=5,col.P=NA,border.P="darkblue")
#' #### change position of the scale labels and polygon filling
#' plot_profile_holland(x=A,gr=3,pos.M="r",col.P="lightblue",density.P=10,
#' border.P="darkblue")
#' #### rotated (clockwise) by +90 degrees 
#' plot_profile_holland(x=A,gr=3,pos.M="r",s=90,col.P="lightblue",density.P=10,
#' border.P="darkblue")
#' # add grid lines
#' plot_profile_holland(x=A,gr=3,pos.M="r",gridl=(TRUE),col.P="lightblue",
#' density.P=10,border.P="darkblue",lwd.P=2)
#' # plot 'blank' Hexagon without any interests profile ... tic marks suppressed
#' # but with Hexagon size adapted to the scaling given in 'A' (argument x)
#' plot_profile_holland(x=A,gr=3,vector=FALSE,gridl=TRUE,col.G="lightblue",
#' measure=F,polyg=FALSE)
#' # but with Hexagon size adapted to fixed range step width
#' plot_profile_holland(x=A,gr=3,vector=FALSE,gridl=TRUE,ri.M=70,ro.M=130,m=10,
#' measure=F,polyg=FALSE)
#' # ... centered
#' plot_profile_holland(x=A,gr=3,center=TRUE,vector=FALSE,gridl=TRUE,ri.M=70,
#' ro.M=130,m=20,measure=F,polyg=FALSE)
#' # ... with surrounding circle
#' plot_profile_holland(x=A,gr=3,circle=TRUE,center=TRUE,vector=FALSE,gridl=TRUE,
#' ri.M=70,ro.M=130,m=20,measure=F,polyg=FALSE)
#' # etc. pp. ... try different styles ...
#' }
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
# x= A; gr=5; x.cent = 0; y.cent = 0; center=FALSE; r=NULL; s=0; radial=TRUE; col.Hr="grey";lwd.Hr=1; lty.Hr=1; circular=TRUE; col.Hc="grey";lwd.Hc=1; lty.Hc=1; circle=FALSE; col.C="grey"; lwd.C=1; lty.C=1; measure=TRUE; ri.M=NULL; ro.M=NULL; m=NULL; pos.M="c"; wid.M=10; col.M="grey"; lwd.M=1; lty.M=1; vector=TRUE; length.V = 0.25; angle.V = 30; code.V = 2; col.V = "black"; lty.V = 1; lwd.V = 1; gridl=FALSE; col.G="grey"; lwd.G=1; lty.G=1; scalab=TRUE; adj.sl=NULL; pos.sl = NULL; offset.sl = 0.5; vfont.sl = NULL; cex.sl = 1; col.sl = NULL; font.sl = NULL; polyg=TRUE; density.P = NULL; angle.P = 45; border.P = "black"; col.P = NA; lwd.P=1; lty.P = par("lty"); fillOddEven.P = FALSE; lab=NULL; s.la=7; r.la=NULL; adj.la=NULL; pos.la = NULL; offset.la = 0.5; vfont.la = NULL; cex.la = 1; col.la = NULL; font.la = NULL

# ri.M <- c(50, 80, 70, 50, 90, 120)
# ro.M <- c(80, 130, 130, 80, 130, 140)
# m <- 10

# ri.M <- 70
# ro.M <- 130
# m <- 10

plot_profile_holland<-function(x, gr=5, x.cent = 0, y.cent = 0, center=FALSE, r=NULL, s=0, radial=TRUE, col.Hr="grey",lwd.Hr=1, lty.Hr=1, circular=TRUE, col.Hc="grey",lwd.Hc=1, lty.Hc=1, circle=FALSE, col.C="grey", lwd.C=1, lty.C=1, measure=TRUE, ri.M=NULL, ro.M=NULL, m=NULL, pos.M="c", wid.M=10, col.M="grey", lwd.M=1, lty.M=1, vector=TRUE, length.V = 0.25, angle.V = 30, code.V = 2, col.V = "black", lty.V = 1, lwd.V = 1, gridl=FALSE, col.G="grey", lwd.G=1, lty.G=1, scalab=TRUE, adj.sl=NULL, pos.sl = NULL, offset.sl = 0.5, vfont.sl = NULL, cex.sl = 1, col.sl = NULL, font.sl = NULL, polyg=TRUE, density.P = NULL, angle.P = 45, border.P = "black", col.P = NA, lwd.P=1, lty.P = par("lty"), fillOddEven.P = FALSE, lab=NULL, s.la=7, r.la=NULL, adj.la=NULL, pos.la = NULL, offset.la = 0.5, vfont.la = NULL, cex.la = 1, col.la = NULL, font.la = NULL, ...){
  
  maxi <- max(x)
  mini <- min(x)
  
  if(!is.null(ro.M)){maxi <- max(ro.M)}
  if(!is.null(ri.M)){mini <- min(ri.M)}
  
  scalefact <- maxi*1.05/(gr/2) # um 5% abzuziehen
  rangesca <- maxi-mini
  #
  X <- x/scalefact
  
  if(!is.null(ri.M)){intercept <- ((min(x) - ri.M)/scalefact)}
  if(is.null(ri.M)){intercept <- 0 }
  
  if(center){X <- (X - (min(X)-intercept) ) * ( max(X)  / max(X - (min(X)-intercept)  ))  } # stretching X
  
  if(!is.null(r)){r=r/scalefact}
  if(is.null(r)){r=maxi/scalefact}
  
  
  #sapply(seq(.1,1.9,by=.1), function(x){1+diff( (c(1,x)))/4})
  i.plot_blank(gr = gr*((1+diff( (c(1,cex.la)))/8))) #(r+gr/scalefact*cex.la)*2
  if(radial){i.hexa_lines_radial(x.cent = x.cent, y.cent = y.cent, r = r, s = s, col = col.Hr, lwd = lwd.Hr, lty = lty.Hr)}
  if(circular){i.hexa_lines_circular(x.cent = x.cent, y.cent = y.cent, r = r, s = s, col = col.Hc, lwd = lwd.Hc, lty = lty.Hc )}
  
  if(circle){i.eli_ines_tot(x.cent = x.cent, y.cent = y.cent, xb=r, yh=r, nseg=720, col=col.C, lwd=lwd.C, lty=lty.C)}
  
  ## 'm.user' ist die Einheitenbeschriftung (Schrittweite) :
  if(!is.null(m)){
    m.user <- m
    m <- m/scalefact
    if(center){m <- r/(rangesca/m.user)}
  }
  if(is.null(m)){
    m.user <- rangesca/gr
    m <- m.user/scalefact
    if(center){m <- r/(rangesca/m.user)}
  }
  ## 'ri.M.user' ist die Einheitenbeschriftung (Minimum) :
  if(!is.null(ri.M)){
    ri.M.user <- ri.M
    ri.M <- ri.M/scalefact
    if(center){ri.M <- 0}
    }
  if(is.null(ri.M)){
    ri.M <- (min(X))#floor
    ri.M.user <- min(x) # ri.M*scalefact
    if(center){ri.M <- 0}
    }
  ## 'ro.M.user' ist die Einheitenbeschriftung (Maximum) :
  if(!is.null(ro.M)){
    ro.M.user <- ro.M
    ro.M <- ro.M/scalefact
    }
  if(is.null(ro.M)){
    ro.M <- (max(X))#ceiling
    ro.M.user <- max(x) # ro.M*scalefact
    }
  
  if(polyg){
    i.hexa_poly(x.cent = x.cent, y.cent = y.cent,r = X ,s = s, density = density.P, angle = angle.P, border = border.P, col = col.P, lwd=lwd.P, lty = lty.P, fillOddEven = fillOddEven.P)
  }
  
  
  if(measure){i.hexa_tics_radial(x.cent = x.cent, y.cent = y.cent, ri = ri.M, ro = ro.M, s = s, m = m, pos = pos.M, wid = wid.M, col = col.M, lwd = lwd.M, lty = lty.M )}
  
  if(gridl){
    #names(x)
    ### some recycling ... of values ...
    ri.M.user <- rep(ri.M.user,length.out=6)
    ri.M <- rep(ri.M,length.out=6)
    ro.M.user <- rep(ro.M.user,length.out=6)
    ro.M <- rep(ro.M,length.out=6)
    #### some complicated things to do ...
    L1 <- mapply(function(ANF, END, WEI){seq(from = ANF, to = END, by =WEI)}, ANF=ri.M.user, END=ro.M.user, WEI= m.user, SIMPLIFY = FALSE)
    lab.range <- seq(from = min(ri.M.user), to = max(ro.M.user), by = m.user)
    t0 <- cbind(ID=lab.range,lab.range)
    for (i in 1:length(L1)){
      t1 <- cbind(ID=L1[[i]],L1[[i]]);
      t0 <- cbind(t0, merge(x = t0,y = t1, by="ID", all.x=TRUE)[,ncol(t0)+(1)]);
    }
    Mlabels <- t0[,-c(1,2)] # user labels als matrix -- Spalten (RIASEC)
    Mlabels <- round(Mlabels) # aktuell keine durchreichung der rundung 
    Mlabels.pos <- (matrix(rep(lab.range,6),ncol = 6,byrow = FALSE))/scalefact #radiale positionen der user labels als matrix -- Spalten (RIASEC)
    if(center){
      Mlabels.pos <- (matrix(rep((seq(from=0, to=r, by=r/(nrow(Mlabels)-1))),6),ncol = 6,byrow = FALSE)) 
    }
    
    # ### correcting angular position ... in Abhängigkeit der Position der tic marks ...
    # if(pos.M=="c"){ s.cor <- s+(max(wid.M)/2)   }
    # if(pos.M=="l"){ s.cor <- s-(max(wid.M))   }
    # if(pos.M=="r"){ s.cor <- s+(max(wid.M))   }
    
    for (i in 1:nrow(Mlabels.pos)){
      i.hexa_lines_circular(x.cent = x.cent, y.cent = y.cent,r = Mlabels.pos[i,], s = s,col = col.G, lwd = lwd.G, lty = lty.G )
      #i.hexa_text_radial(x.cent = x.cent, y.cent = y.cent,r = Mlabels.pos[i,],s = s.cor,labels = Mlabels[i,],adj=adj.sl, pos = pos.sl, offset = offset.sl, vfont = vfont.sl, cex = cex.sl, col = col.sl, font = font.sl)
    } # scheifen ende 
  } # grid lines ende

  
  
  if(scalab){
    #names(x)
    ### some recycling ... of values ...
    # m.user <- rep(m.user,length.out=6)
    # m <- rep(m,length.out=6)
    ri.M.user <- rep(ri.M.user,length.out=6)
    ri.M <- rep(ri.M,length.out=6)
    ro.M.user <- rep(ro.M.user,length.out=6)
    ro.M <- rep(ro.M,length.out=6)
    #### some complicated things to do ...
    L1 <- mapply(function(ANF, END, WEI){seq(from = ANF, to = END, by =WEI)}, ANF=ri.M.user, END=ro.M.user, WEI= m.user, SIMPLIFY = FALSE)
    lab.range <- seq(from = min(ri.M.user), to = max(ro.M.user), by = m.user)
    t0 <- cbind(ID=lab.range,lab.range)
    for (i in 1:length(L1)){
      t1 <- cbind(ID=L1[[i]],L1[[i]]);
      t0 <- cbind(t0, merge(x = t0,y = t1, by="ID", all.x=TRUE)[,ncol(t0)+(1)]);
    }
    Mlabels <- t0[,-c(1,2)] # user labels als matrix -- Spalten (RIASEC)
    Mlabels <- round(Mlabels) # aktuell keine durchreichung der rundung 
    Mlabels.pos <- (matrix(rep(lab.range,6),ncol = 6,byrow = FALSE))/scalefact #radiale positionen der user labels als matrix -- Spalten (RIASEC)
    if(center){
      Mlabels.pos <- (matrix(rep((seq(from=0, to=r, by=r/(nrow(Mlabels)-1))),6),ncol = 6,byrow = FALSE)) 
      }
    
    ### correcting angular position ... in Abhängigkeit der Position der tic marks ...
    if(pos.M=="c"){ s.cor <- s+(max(wid.M)/2)   }
    if(pos.M=="l"){ s.cor <- s-(max(wid.M))   }
    if(pos.M=="r"){ s.cor <- s+(max(wid.M))   }
           
    for (i in 1:nrow(Mlabels.pos)){
      i.hexa_text_radial(x.cent = x.cent, y.cent = y.cent,r = Mlabels.pos[i,],s = s.cor,labels = Mlabels[i,],adj=adj.sl, pos = pos.sl, offset = offset.sl, vfont = vfont.sl, cex = cex.sl, col = col.sl, font = font.sl)
    } # scheifen ende 
  } # skalen labels ende
  
  
  if(is.null(lab)){
    if(is.null(names(x))){cat("no names found in x, R I A S E C assumed","\n"); lab <- c("R","I","A","S","E","C")}
    if(!is.null(names(x))){lab <- names(x)}
  }
  lab <- rep(lab,length.out=6) # replicate them ...
  if(is.null(r.la)){r.la <- r*((1+diff( (c(1,cex.la)))/8))*1.035 }#*cex.la*1.05 #(r+gr/scalefact*cex.la)*2 # (r+(gr/scalefact+gr/scalefact/r)*cex.la*2)
  i.hexa_text_radial(x.cent = x.cent, y.cent = y.cent, r=r.la, s = s, labels = lab, adj=adj.la, pos = pos.la, offset = offset.la, vfont = vfont.la, cex = cex.la, col = col.la, font = font.la)
  
  if(vector){
    gesvec_grad <- i.gesvector(x)
    erg <- gesvec_grad + s
    fac <- mean(X)
    arrows(x0 = x.cent, y0 = y.cent, x1 = sin(rad(erg))*fac,y1 = cos(rad(erg))*fac, length = length.V, angle = angle.V, code = code.V, col = col.V, lty = lty.V, lwd = lwd.V)
    return(gesvec_grad)
  }
  
  
} # funktionsende


# plotgröße : (r+gr/scalefact*cex.la)*2




