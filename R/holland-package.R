#' @title Statistics for the Framework of Holland's Theory of Vocational Choice
#' @name holland-package
#' @docType package
#' @importFrom graphics lines plot.new plot.window text polygon segments par arrows
#' @importFrom utils read.table write.table
#' @importFrom MplusAutomation readModels
#' @importFrom MplusAutomation extractModelParameters
#' @importFrom MplusAutomation extractModelSummaries
#' @importFrom MplusAutomation runModels
#' @importFrom mvtnorm rmvnorm
#' @description Offers a convenient way to compute parameters in the framework of vocational choice by J.L. Holland, (1997).
#' @details  The core of J.L. Holland's model of vocational interest orientations consists in the assumption of a vocational personality, which can be described with six basic dimensions. Based on this basic assumption, different theorems and (derived) constructs are part of the theory of vocational interest orientations (see Holland, 1997).
#' 
#' In its current version, the package '\code{holland}' provides three main functional areas that allow for some statistical analysis from the theory of vocational interest orientation of J. L. Holland (see Holland, 1997). 
#' 
#' One functional area is related to the concept of \emph{congruence} between a person's interest orientation and a particular vocational environment. For this, the package (currently) offers ten R-functions with which different indices for the congruence can be calculated (see all functions starting with '\code{con_}', e.g. \code{\link{con_oneletter_holland}}). 
#' 
#' The second function area is related to the concept of \emph{differentiation}, which is currently only covered with the function  \code{\link{dif_7_holland}} to compute seven different indices of differentiation.
#' 
#' The last function area addresses the so-called \emph{calculus} hypothesis, according to which the six interest orientations are arranged in the form of a hexagonal structure. The package 'holland' offers, among other functions, three (wrapper) functions, which are directly addressed to the user. Within the calculus hypothesis the arrangement of empirical data can be determined (cf. function \code{\link{Circ_emp}}) and their fit to the hexagonal structure can be determined (cf. function \code{\link{Circ_test}}). Furthermore, other construct domains (e.g., big-five personality) with their dimensions can be projected into the hexagonal structure (cf. Function \code{\link{Circ_pro}}). These three functions are based on the method of structural equation modeling proposed by Nagy et al. (2009), which was implemented as Mplus syntax. The application of the three functions therefore requires an installation of the commercial software Mplus (cf. also \code{\link{MplusAutomation}}).
#'   
#'     
#'         
#' @author \itemize{\item{Joerg-Henrik Heine <jhheine@@googlemail.com>}}
#' @references Holland, J.L. (1997). \emph{Making vocational choices. A theory of vocational personalities and work environments}. Lutz, FL: Psychological Assessment Resources.
#' #' @references Nagy, G., Marsh, H. W., Luedtke, O., & Trautwein, U. (2009). Representing circles in our minds: Confirmatory factor analysis of circumplex structures and profiles. In T. Teo & M. S. Khine (Hrsg.), \emph{Structural Equation Modeling in Educational Research: Concepts and applications} (S. 287 - 315). Rotterdam Boston Taipei: Sense Publishers.
#' @seealso \code{\link[utils]{utils}}, \code{\link[MplusAutomation]{MplusAutomation}}
#' @examples
#' #######################################
#' # see description of functions ...
NULL
