\name{AIST_2005_F_1270}
\alias{AIST_2005_F_1270}
\docType{data}
\title{RIASEC correlations - female AIST norm sample}
\description{
a matrix of correlation coefficients based on \emph{N} = 1270 observations, obtaind from the german female norm sample of the AIST published in Bergman & Eder (2005).}
\usage{
  data(AIST_2005_F_1270)
}

\format{
  A 6 x 6 matrix object with named columns and rows.
}

\details{
can be written as a .dat text file using the function \code{\link{write_dat}()}.
}

\references{
Bergmann, C.,& Eder, F. (2005). \emph{AIST-R Allgemeiner Interessen-Struktur-Test - Revision}. Goettingen: Hogrefe.
}
\examples{
data(AIST_2005_F_1270)
dim(AIST_2005_F_1270)
AIST_2005_F_1270
}
\keyword{datasets}
