% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.mars.R
\name{print.mars}
\alias{print.mars}
\title{Print Summary of a MARS Model Object}
\usage{
\method{print}{mars}(object)
}
\arguments{
\item{object}{A \code{mars} model object obtained from fitting a MARS model using
the \code{mars} function.}
}
\value{
This function invisibly returns the \code{mars} object, primarily focusing
on side effects (printing to the console). It does not return any values directly.
}
\description{
Provides a concise summary of a fitted MARS model, including the model's
formula, coefficients, and a brief overview of the basis functions used. This
function enhances the interpretability of the model by displaying critical
information in a user-friendly format.
}
\examples{
# Assuming 'mm' is a fitted MARS model:
print(mm)

}
\references{
Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals
of Statistics, Vol. 19, No. 1, pp. 1-67. Available at:
\href{https://www.jstor.org/stable/2241837}{JSTOR}.

Further examples and resources can be found in the STAT 360 GitHub repository
STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.
}
\seealso{
\code{\link{mars}} for model fitting, \code{\link{summary.mars}} for
a more detailed summary.
}
