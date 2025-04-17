\name{AIST_2005_M_1226}
\alias{AIST_2005_M_1226}
\docType{data}
\title{RIASEC correlations - male AIST norm sample}
\description{
a matrix of correlation coefficients based on \emph{n} = 1226 observations, obtaind from the german male norm sample of the AIST published in Bergman & Eder (2005).}
\usage{
  data(AIST_2005_M_1226)
}

\format{
  A 6 x 6 matrix object with named columns and rows.
}

\details{
can be written as a .dat text file using the function \code{\link{write_dat}()}.
}

\references{
Bergmann, C., & Eder, F. (2005). \emph{AIST-R Allgemeiner Interessen-Struktur-Test - Revision}. Goettingen: Hogrefe.
}
\examples{
data(AIST_2005_M_1226)
dim(AIST_2005_M_1226)
AIST_2005_M_1226
}
\keyword{datasets}
