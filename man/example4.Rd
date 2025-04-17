\name{example4}
\alias{example4}
\docType{data}
\title{RIASEC and Big-Five correlations - not so perfect}
\description{
another fictional correlation matrix representing \emph{not so perfect} RIASEC (circumplex) correlations and correlations (with) five dimensions of personality (big-five). 
}
\usage{
  data(example3)
}

\format{
  A 11 x 11 matrix object with named columns and rows.
}

\details{
can be written as a .dat text file using the function \code{\link{write_dat}()}.
}

\references{
Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
}
\examples{
data(example4)
dim(example4)
example4
}
\keyword{datasets}
