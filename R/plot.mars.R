#' Visualize Model Diagnostics for mars Objects
#'
#' Generates diagnostic plots for evaluating the fit of a MARS model. This method
#' creates three standard plots: Residuals vs. Fitted values, Histogram of Residuals,
#' and a Normal Q-Q plot of Residuals, which are crucial for assessing the model's
#' assumptions and performance.
#'
#' @param object An object of class \code{'mars'}, typically the result of fitting a MARS model
#' using the \code{\link{mars}} function.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#' of producing plots.
#' @export
#'
#' @examples
#' # Assuming 'mm' is a fitted 'mars' model object
#' plot(mm)
#'
#' @importFrom graphics plot hist qqnorm qqline
#'
#' @seealso \code{\link{mars}} for model fitting, \code{\link{residuals}} for extracting residuals.
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. [Link](https://www.jstor.org/stable/2241837)
#'
#' Additional examples and materials on MARS can be found in the STAT 360 GitHub repository
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# This function generates diagnostic plots for a MARS model, helping in the assessment of the model's fit.
plot.mars <- function(object) {
# Extract fitted values and residuals from the MARS model object
  fitted <- fitted(object) # Fitted values are the model's predictions on the training data
  resid <- residuals(object) # Residuals are the differences between observed and fitted values
# Set up plotting area to display three plots in one row
  par(mfrow=c(1,3))
# Plot 1: Residuals vs. Fitted Values
# This plot helps in checking the homoscedasticity assumption (constant variance of residuals).
  plot(fitted, resid, col = "black", pch = 20, main = "Residuals vs. Fitted Values", xlab="Fitted Values",
       ylab="Residuals")
  abline(h = 0, col = "black")
# Plot 2: Histogram of Residuals
# This plot gives an idea about the distribution of residuals.
  hist(resid, col = "red", main = "Histogram of Residuals", xlab="Residuals")
# Plot 3: Normal Q-Q Plot of Residuals
# This plot helps to assess if residuals follow a normal distribution, which is an assumption in many regression models.
  qqnorm(resid, col = "red", main = "Normal Q-Q Plot of Residuals", ylab="Residuals")
  qqline(resid, col = "black") # Adds a reference line to the QQ plot for comparison
}

