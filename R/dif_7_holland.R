#' @title Seven differentiation indices for Holland profiles
#' @keywords differentiation
#' @export dif_7_holland
#' @description The function computes seven differentiation indices for Holland profiles as cited in Bergman (1993) and Eder (1998).  
#' @details The function finds seven different (see argument \code{ind}) differentiation indices as cited Bergman (1993) and Eder (1998) for the Holland-interest profile given in argument A, which is the person interest profile consisting of six values (either raw scores or norms) for each of the six dimensions of vocational interests.
#'
#' specific information on the indices of differentiation: \cr
#' 
#' Table 1: Differentiation indices for Holland profiles. \cr
#' Source: Bergmann, (1993, p. 267).
#' 
#' \tabular{rll}{
#'    Index \tab  Brief description \tab Author / Source \cr
#'    DI1 \tab Difference between highest and second highest interest score \tab (Frantz & Walsh, 1972)  \cr
#'    DI2 \tab Difference between highest and third highest interest score \tab  (Spokane & Walsh, I978)   \cr
#'    DI3 \tab Difference between highest score and the average of the second and fourth highest score \tab (Iachan, 1984) \cr
#'    DI4 \tab Difference between highest score and the average of the third and fifth highest score \tab (Iachan, 1984) \cr
#'    DI5 \tab Difference between highest and lowest score \tab (Holland, 1973)  \cr
#'    DI6 \tab Difference between highest and lowest score, standardized by the overall level of interest \tab (Peiser & Meir,1978)    \cr
#'    DI7 \tab Dispersion of interest scores \tab (Healy & Mourton, 1983) \cr
#'  }
#'
#' @param A a numeric vector with Holland score values for the interest profile of length = 6.
#' @param ind a character indicating which index (see table 1) to return.
#' @return a numeric with value for differentiation.
#' @references Holland, J.L. 1963. A theory of vocational choice. I. Vocational images and choice. \emph{Vocational Guidance Quarterly, 11}(4), 232–239.
#' @references Bergmann, C. (1993). Differenziertheit der Interessen und berufliche Entwicklung. \emph{Zeitschrift für Differentielle und Diagnostische Psychologie, 14}(4), 265–279.
#' @references Frantz, T. T. & Walsh, E. P. (1972). Exploration of Holland's theory of vocational choice in graduate school environments. \emph{Journal of Vocational Behaviour}, 2, 223-232.
#' @references Spokane, A. R. & Walsh, W. B. (1978). Occupational level and Holland's theory for employed men and women. \emph{Journal of Vocational Behaviour}, 12, 145-154.
#' @references Iachan, R. (1984). A family of differentiation indices. \emph{Psychometrika}, 49, 217-222.
#' @references Holland, J. L. (1973). \emph{Making vocational choices}. Englewood Cliffs, New Jersey: Prentice Hall Inc.
#' @references Peiser, C. & Meir, E. I. ( 1978). Congruency, consistency, and differentiation of vocational interests as predictors of vocational satisfaction and preference stability. \emph{Journal of Vocational Behaviour}, 12, 270-278.
#' @references Healy, C. C. & Mourton, D. L. (1983). Derivatives of the Self-Directed Search: Potential clinical and evaluative uses. \emph{Journal of Vocational Behavior, 23}(3), 318–328. https://doi.org/10.1016/0001-8791(83)90045-3 
#' @references Eder, F. (1998). Differenziertheit der Interessen als Prädiktor der Interessenentwicklung. In J. Abel & C. Tarnai (Hrsg.), \emph{Pädagogisch-psychologische Interessenforschung in Studium und Beruf} (S. 63–77). Münster: Waxmann.
#' @examples 
#' # fictional interest profile:
#' A <- c(70, 90, 120, 50, 60, 130)
#' names(A) <- c("R","I","A","S","E","C")
#' 
#' # differentiation according to Frantz & Walsh (1972)
#' dif_7_holland(A, ind = "DI1") 
#' 
#' # all of the differentiation indices
#' ind <- c("DI1","DI2","DI3","DI4","DI5","DI6","DI7")
#' sapply(ind, function(x)dif_7_holland(A,x),USE.NAMES = FALSE)
################################################################################
# A <- c(70, 90, 120, 50, 60, 130); names(A) <- c("R","I","A","S","E","C")
dif_7_holland<-function(A, ind = c("DI1","DI2","DI3","DI4","DI5","DI6","DI7")){
  #func. by: jhheine@googlemail.com 
  mittel<-function(v){sum(v)/length(v)}
  varianz<-function(v){ sum( (v-mittel(v))*(v-mittel(v)) ) /length(v)}
  type <- match.arg(ind)
  erg <- switch(type,
         DI1 = abs(diff(sort(A,decreasing = TRUE)[c(1,2)])),
         DI2 = abs(diff(sort(A,decreasing = TRUE)[c(1,3)])),
         DI3 = (sort(A,decreasing = TRUE)[1] - ( sort(A,decreasing = TRUE)[2] + sort(A,decreasing = TRUE)[4]) / 2),
         DI4 = (sort(A,decreasing = TRUE)[1] - ( sort(A,decreasing = TRUE)[3] + sort(A,decreasing = TRUE)[5]) / 2),
         DI5 = abs(diff(sort(A,decreasing = TRUE)[c(1,6)])),
         DI6 = (abs(diff(sort(A,decreasing = TRUE)[c(1,6)])) / sum(A)),
         DI7 = sqrt(varianz(A))   )
  names(erg) <- type
  return (erg)
}
