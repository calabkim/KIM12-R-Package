#' Summary of a MARS Model
#'
#' Provides a comprehensive overview of a Multivariate Adaptive Regression Splines (MARS) model,
#' encompassing statistical summaries, model coefficients, and information about the basis functions
#' utilized. This facilitates a deeper understanding of the model's structure and performance.
#'
#' @param object A `mars` object produced by fitting a MARS model using the `mars()` function.
#'
#' @return The function does not return a value per sec but prints a detailed summary
#' of the MARS model to the console, akin to the summary provided for linear models (`lm`),
#' including specifics about the basis functions integrated into the model.
#' @export
#'
#' @examples
#' # Assuming 'mm' is a fitted MARS model from concrete compressive strength data:
#' summary(mm)
#'
#' @seealso \code{\link{mars}} for fitting MARS models, and \code{\link{print.mars}} for a concise summary.
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals
#' of Statistics, Vol. 19, No. 1, pp. 1-67. Available at:
#' [JSTOR](https://www.jstor.org/stable/2241837).
#'
#' For additional examples and documentation on MARS, visit the STAT 360 GitHub repository
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# Summarizes the MARS model by displaying the linear model summary and detailed basis function information
summary.mars<-function(object){
# Print the summary of the underlying linear model
# This includes statistical measures such as residuals, coefficients, and R-squared values among others
  print(summary.lm(object))
# Iterate over each basis function defined in the MARS model
  for (i in 1:length(names(object$B))){
# Print the name of each basis function, starting with the intercept
    cat(names(object$B)[[i]], ":","\n")
# The first basis function is always the intercept, so it is treated separately
    if(i == 1){
      cat("Intercept", "\n")
# Skip the remaining operations in the loop and move to the next iteration
      next
    }
# Retrieve the information about the current basis function from Bfuncs
    Bf = object$Bfuncs[[i]]
# Loop through each component of the basis function
    for(j in 1:nrow(Bf))
# Print details of the component including sign, split variable, and split point
      cat("Component ",j,":", "\n",
          "","Sign:",Bf[j,1],"\n", # Indicates the direction of the hinge function
          "","Split Variable:",Bf[j,2],"\n", # The predictor variable used for splitting
          "","Split Point:",Bf[j,3],"\n") # The value at which the split occurs
  }
}
