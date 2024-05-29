#' ANOVA for MARS Model Objects
#'
#' @description
#' This function extends the generic `anova` method for objects of class \code{'mars'}.
#' It provides detailed ANOVA statistics for the fitted MARS model, similar to the traditional
#' ANOVA output for linear models (`lm`). Additionally, it includes specific details about the
#' basis functions used in the model, enhancing understanding of the model structure and fit.
#'
#' @param object An object of class \code{'mars'}, typically resulting from a call to \code{\link[mars]{mars()}}.
#'
#' @return Returns a comprehensive ANOVA table for the MARS model, including statistics for each
#' basis function. The output is analogous to the ANOVA output for linear models but tailored
#' to the specifics of the MARS model framework.
#' @export
#'
#' @examples
#' # Fit a MARS model to the concrete dataset
#' mm <- mars(ConcreteCompressiveStrength ~ ., data = concrete, control = mars.control(Mmax = 10, d = 3, trace = FALSE))
#' # View the ANOVA summary for the fitted model
#' output <- anova(mm)
#'
#' @references
#' 1. Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics, 19(1), pp. 1-67.
#' Available online: [JSTOR](https://www.jstor.org/stable/2241837)
#'
#' 2. STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}


anova.mars <- function(object) {
# Fit a linear model using the basis functions and response variable from the MARS object.
# The -1 in the formula removes the intercept as the MARS model includes it within the basis functions.
  fit <- lm(y~.-1,data=data.frame(y=object$y,object$B))
# Print the ANOVA table for the linear model to summarize the analysis of variance.
  print(anova(fit))
# Iterate over each basis function present in the MARS model.
  for (i in 1:length(names(object$B))){
# Print the name of each basis function.
    cat(names(object$B)[[i]], ":","\n")
# The first basis function is always the intercept, so handle it separately.
    if(i == 1){
      cat("Intercept", "\n")
      next # Skip to the next iteration.
    }
# Extract the hinge function details for the current basis function.
    Bf = object$Bfuncs[[i]]
# For each row in the hinge function details (representing components of the basis function),
# print the sign, split variable, and split point.
    for(j in 1:nrow(Bf))
      cat("Component ",j,":", "\n",
          "","Sign:",Bf[j,1],"\n", # Sign of the hinge function (+1 or -1).
          "","Split Variable:",Bf[j,2],"\n", # Predictor variable for the hinge.
          "","Split Point:",Bf[j,3],"\n") # The value at which the hinge splits.
  }
}

