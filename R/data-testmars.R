#' Synthetic Test Dataset for Package
#'
#' This synthetic dataset has been crafted to demonstrate the capabilities of the KTPmars package, particularly for testing Multivariate Adaptive Regression Splines (MARS) modeling. It comprises 100 observations with 10 explanatory variables. Notably, the response variable is primarily influenced by the first two explanatory variables, providing a straightforward scenario for illustrating MARS model fitting and predictive accuracy.
#'
#' @format A data frame with 100 observations and 11 features, including:
#' \describe{
#'   \item{y}{Numeric: The response variable, simulated to depend significantly on x1 and x2.}
#'   \item{x1 to x10}{Numeric: Explanatory variables. While x1 and x2 have a direct impact on the response variable, x3 through x10 serve as noise variables to emulate real-world data complexities.}
#' }
#'
#' @usage
#' data(marstestdata)
#'
#' @details
#' The `marstestdata` set serves as a controlled environment for exploring the functionality and performance of MARS models. By design, only x1 and x2 are meaningful predictors, a setup that aids in understanding the model's feature selection process and how it manages irrelevant variables.
#'
#' @references
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}
"marstestdata"

