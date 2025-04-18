% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_profile_holland.R
\name{plot_profile_holland}
\alias{plot_profile_holland}
\title{Plot an interest profile in the Hexagon}
\usage{
plot_profile_holland(
  x,
  gr = 5,
  x.cent = 0,
  y.cent = 0,
  center = FALSE,
  r = NULL,
  s = 0,
  radial = TRUE,
  col.Hr = "grey",
  lwd.Hr = 1,
  lty.Hr = 1,
  circular = TRUE,
  col.Hc = "grey",
  lwd.Hc = 1,
  lty.Hc = 1,
  circle = FALSE,
  col.C = "grey",
  lwd.C = 1,
  lty.C = 1,
  measure = TRUE,
  ri.M = NULL,
  ro.M = NULL,
  m = NULL,
  pos.M = "c",
  wid.M = 10,
  col.M = "grey",
  lwd.M = 1,
  lty.M = 1,
  vector = TRUE,
  length.V = 0.25,
  angle.V = 30,
  code.V = 2,
  col.V = "black",
  lty.V = 1,
  lwd.V = 1,
  gridl = FALSE,
  col.G = "grey",
  lwd.G = 1,
  lty.G = 1,
  scalab = TRUE,
  adj.sl = NULL,
  pos.sl = NULL,
  offset.sl = 0.5,
  vfont.sl = NULL,
  cex.sl = 1,
  col.sl = NULL,
  font.sl = NULL,
  polyg = TRUE,
  density.P = NULL,
  angle.P = 45,
  border.P = "black",
  col.P = NA,
  lwd.P = 1,
  lty.P = par("lty"),
  fillOddEven.P = FALSE,
  lab = NULL,
  s.la = 7,
  r.la = NULL,
  adj.la = NULL,
  pos.la = NULL,
  offset.la = 0.5,
  vfont.la = NULL,
  cex.la = 1,
  col.la = NULL,
  font.la = NULL,
  ...
)
}
\arguments{
\item{x}{numeric vector with length of six values; either raw scores or normed values}

\item{gr}{numeric optional specification of the (quadratic) plotting area.}

\item{x.cent}{numeric optional specification of the x center of the plot}

\item{y.cent}{numeric optional specification of the y center of the plot}

\item{center}{logical whether to re-scale the minimum profile value to be in the center of the Hexagon}

\item{r}{numeric optional specification of the radius (possibly vector with max length 6 -- will be recycled) of the hexagonal plot (must be then in the same measure scale as \code{x}).}

\item{s}{integer optional specification of the angular starting position (in degrees) when drawing the Hexagon (from 1 to 360).}

\item{radial}{logical whether to plot radial (sector) lines within the Hexagon.}

\item{col.Hr}{character expression of colors (possibly vector with max length 6 -- will be recycled) for the radial Hexagon lines.}

\item{lwd.Hr}{numeric (possibly vector with max length 6 -- will be recycled) with line widths for the radial Hexagon lines.}

\item{lty.Hr}{numeric or character expression (possibly vector with max length 6 -- will be recycled) with line types for the radial Hexagon lines.}

\item{circular}{logical whether to plot a circular (border) line around the Hexagon.}

\item{col.Hc}{character expression of colors (possibly vector with max length 6 -- will be recycled) for colors for the circular (border) Hexagon line.}

\item{lwd.Hc}{numeric (possibly vector with max length 6 -- will be recycled) with line widths for the circular (border) Hexagon line.}

\item{lty.Hc}{numeric or character expression (possibly vector with max length 6 -- will be recycled) with line types for the circular (border) Hexagon line.}

\item{circle}{logical whether to plot a circle around the Hexagon.}

\item{col.C}{character expression for color of circle around Hexagon.}

\item{lwd.C}{numeric for line width of circle around Hexagon.}

\item{lty.C}{numeric or character expression for line type of circle around Hexagon.}

\item{measure}{logical whether to plot a (radial) measure scale (for each Hexagon dimension).}

\item{ri.M}{numeric (possibly vector with max length 6 -- will be recycled) with numeric value(s) for start (minimum) of the measure scale for each Hexagon dimension respectively (must be then in the same measure scale as \code{x}).}

\item{ro.M}{numeric (possibly vector with max length 6 -- will be recycled) with numeric value(s) for end (maximum) of the measure scale for each Hexagon dimension respectively (must be then in the same measure scale as \code{x}).}

\item{m}{numeric with single value for the distance of the tic marks of the measure scale.}

\item{pos.M}{character one of \code{c("c","l","r")} for the orientation of the tic marks with reference to the radial hexagon lines -- centered, left, right respectively.}

\item{wid.M}{numeric (possibly vector with max length 6 -- will be recycled) defining the widths of the tic marks in degree.}

\item{col.M}{character expression of colors (possibly vector with max length 6 -- will be recycled) for the color of the tic marks.}

\item{lwd.M}{numeric (possibly vector with max length 6 -- will be recycled) for the line widths of the tic marks.}

\item{lty.M}{numeric or character expression (possibly vector with max length 6 -- will be recycled) for the line types of the tic marks.}

\item{vector}{logical whether to plot the (resulting) total vector (as arrow) for the interest profile according to Eder, (1998).}

\item{length.V}{length of the edges of the total vector arrow head (see \code{\link{arrows}}).}

\item{angle.V}{angle from the shaft of the total vector arrow to the edge of the arrow head (see \code{\link{arrows}}).}

\item{code.V}{integer code, determining kind of total vector arrow to be drawn(see \code{\link{arrows}}).}

\item{col.V}{color of the total vector arrow (see \code{\link{arrows}}).}

\item{lty.V}{line type of the total vector arrow (see \code{\link{arrows}}).}

\item{lwd.V}{line wide of the total vector arrow (see \code{\link{arrows}}).}

\item{gridl}{logical whether to plot a circular grid lines at the positions of the tic marks for the measure scale around the Hexagon.}

\item{col.G}{character expression of colors (possibly vector with max length 6 -- will be recycled) for the color of the grid lines.}

\item{lwd.G}{numeric (possibly vector with max length 6 -- will be recycled) for the line widths of the grid lines.}

\item{lty.G}{numeric or character expression (possibly vector with max length 6 -- will be recycled) for the line types of the grid lines.}

\item{scalab}{logical whether to ad scale labeling with units.}

\item{adj.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{pos.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{offset.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{vfont.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{cex.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{col.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{font.sl}{parameter for scale labeling control -- see \code{\link{text}}.}

\item{polyg}{whether to plot a polygon for the interest profile (see argument \code{x}).}

\item{density.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{angle.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{border.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{col.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{lwd.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{lty.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{fillOddEven.P}{parameter for polygon drawing control -- see \code{\link{polygon}}.}

\item{lab}{character vector (with max length 6 -- will be recycled) for labeling the Hexagon dimensions, by default names from \code{x} a taken ...}

\item{s.la}{integer to control angular positions of labels (in degrees).}

\item{r.la}{numeric optional specification of the radial positions (possibly vector with max length 6 -- will be recycled) for the labels of the Hexagon dimensions (must be then in the same measure scale as \code{x}).}

\item{adj.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{pos.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{offset.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{vfont.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{cex.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{col.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{font.la}{parameter for labeling control -- see \code{\link{text}}.}

\item{...}{parameters passed through}
}
\value{
a plot and if argument \code{vector=TRUE} (default) a numeric value indicating the orientation of the total vector for the interest profile in degrees.
}
\description{
This function plots an interest profile in the Hexagon
}
\details{
no details -- but see arguments.
}
\examples{
#### a fictional interest profile:
A <- c(95, 125, 122, 105, 100, 90)
names(A) <- c("R","I","A","S","E","C")
#### plot with default settings
plot_profile_holland(x=A)
#### easy way to change global scaling with argument 'gr'
### additional change the color and thickness of the polygon line ...
plot_profile_holland(x=A,gr=3,border.P="darkblue",lwd.P = 2)
#### give the Hexagon a fixed scale range ...
plot_profile_holland(x=A,gr=10,ri.M=70,ro.M=130,lwd.P=2)
#### center the minimum and addition fix the scale range and step width ...
plot_profile_holland(x=A,gr=10,center=TRUE,ri.M=70,ro.M=130,m=10,lwd.P=2)

############## More examples
\dontrun{
#### center the minimum without a fixed scale range but with a fixed step width ...
plot_profile_holland(x=A,gr=10,center=TRUE,m=5,col.P=NA,border.P="darkblue")
#### change position of the scale labels and polygon filling
plot_profile_holland(x=A,gr=3,pos.M="r",col.P="lightblue",density.P=10,
border.P="darkblue")
#### rotated (clockwise) by +90 degrees 
plot_profile_holland(x=A,gr=3,pos.M="r",s=90,col.P="lightblue",density.P=10,
border.P="darkblue")
# add grid lines
plot_profile_holland(x=A,gr=3,pos.M="r",gridl=(TRUE),col.P="lightblue",
density.P=10,border.P="darkblue",lwd.P=2)
# plot 'blank' Hexagon without any interests profile ... tic marks suppressed
# but with Hexagon size adapted to the scaling given in 'A' (argument x)
plot_profile_holland(x=A,gr=3,vector=FALSE,gridl=TRUE,col.G="lightblue",
measure=F,polyg=FALSE)
# but with Hexagon size adapted to fixed range step width
plot_profile_holland(x=A,gr=3,vector=FALSE,gridl=TRUE,ri.M=70,ro.M=130,m=10,
measure=F,polyg=FALSE)
# ... centered
plot_profile_holland(x=A,gr=3,center=TRUE,vector=FALSE,gridl=TRUE,ri.M=70,
ro.M=130,m=20,measure=F,polyg=FALSE)
# ... with surrounding circle
plot_profile_holland(x=A,gr=3,circle=TRUE,center=TRUE,vector=FALSE,gridl=TRUE,
ri.M=70,ro.M=130,m=20,measure=F,polyg=FALSE)
# etc. pp. ... try different styles ...
}
}
\references{
Eder, F. (1998). Differenziertheit der Interessen als Prädiktor der Interessenentwicklung. In J. Abel & C. Tarnai (Hrsg.), \emph{Pädagogisch-psychologische Interessenforschung in Studium und Beruf} (S. 63–77). Münster: Waxmann.
}
\keyword{misc}
