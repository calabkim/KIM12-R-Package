#' Print Summary of a MARS Model Object
#'
#' Provides a concise summary of a fitted MARS model, including the model's
#' formula, coefficients, and a brief overview of the basis functions used. This
#' function enhances the interpretability of the model by displaying critical
#' information in a user-friendly format.
#'
#' @param object A `mars` model object obtained from fitting a MARS model using
#' the \code{mars} function.
#'
#' @return This function invisibly returns the `mars` object, primarily focusing
#' on side effects (printing to the console). It does not return any values directly.
#' @export
#'
#' @examples
#' # Assuming 'mm' is a fitted MARS model:
#' print(mm)
#'
#' @seealso \code{\link{mars}} for model fitting, \code{\link{summary.mars}} for
#' a more detailed summary.
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals
#' of Statistics, Vol. 19, No. 1, pp. 1-67. Available at:
#' [JSTOR](https://www.jstor.org/stable/2241837).
#'
#' Further examples and resources can be found in the STAT 360 GitHub repository
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.


print.mars<-function(object){
  print(object$call) # print the matched call
  cat("Coefficients:","\n")
  print(object$coefficients) # print the coefficients
  for (i in 1:length(names(object$B))){ # loop over Basis functions and print
    # hinge function information for each of them
    cat(names(object$B)[[i]], ":","\n")
    if(i == 1){
      cat("Intercept", "\n")
      next
    }
    Bf = object$Bfuncs[[i]]
    for(j in 1:nrow(Bf))
      cat("Component ",j,":", "\n",
          "","Sign:",Bf[j,1],"\n",
          "","Split Variable:",Bf[j,2],"\n",
          "","Split Point:",Bf[j,3],"\n")
  }
}

