% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars.R
\name{fwd_stepwise}
\alias{fwd_stepwise}
\title{Forward Stepwise Selection for MARS Model}
\usage{
fwd_stepwise(y, x, control = mars.control())
}
\arguments{
\item{y}{Numeric vector representing the response variable.}

\item{x}{Model matrix of predictor variables.}

\item{control}{A \code{mars.control} object specifying parameters for the model fitting process, including \code{Mmax} (the maximum number of basis functions, excluding the intercept), \code{d} (degree of interaction terms), and \code{trace} (boolean flag for tracing model building progress). For detailed configuration, refer to \code{?mars.control}.}
}
\value{
Returns a list with components:
\itemize{
\item{y}{Column vector of the response variable.}
\item{B}{Matrix containing the selected basis functions. Each column represents a basis function constructed as products of hinge functions, characterized by the sign, predictor number, and split point.}
\item{Bfuncs}{List detailing the construction of each basis function in \code{B}. Each list element corresponds to a column in \code{B} and may contain multiple rows, each describing a hinge function (including sign, variable, and split point).}
}
}
\description{
Implements Algorithm 2 by Friedman to construct a matrix of basis functions for a MARS model through forward stepwise selection. The function iteratively selects pairs of hinge functions that minimize the Lack of Fit (LOF) measure, using the Generalized Cross-Validation (GCV) criterion, to build the model's basis functions.
}
\examples{
# Example of forward stepwise selection on the concrete dataset
fwd_result <- fwd_stepwise(y = concrete$ConcreteCompressiveStrength,
                           x = concrete[,-1],
                           control = mars.control(Mmax = 8, d = 3, trace = FALSE))

}
\references{
Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics, Vol. 19, No. 1, pp. 1-67. Available at: \href{https://www.jstor.org/stable/2241837}{JSTOR}

STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}
}
