\name{AIST_2019_F_2015}
\alias{AIST_2019_F_2015}
\docType{data}
\title{RIASEC correlations - female AIST sample}
\description{
a matrix of correlation coefficients based on \emph{n} = 2015 observations, obtaind from the german female sample of the AIST published in Bergman & Eder (2019).}
\usage{
  data(AIST_2019_F_2015)
}

\format{
  A 6 x 6 matrix object with named columns and rows.
}

\details{
can be written as a .dat text file using the function \code{\link{write_dat}()}.
}

\references{
Bergmann, C. & Eder, F. (2019). \emph{AIST 3. Allgemeiner Interessen-Struktur-Test mit Umwelt-Struktur-Test (UST-3) -- Version 3}. Goettingen: Hogrefe.
}
\examples{
data(AIST_2019_F_2015)
dim(AIST_2019_F_2015)
AIST_2019_F_2015
}
\keyword{datasets}
