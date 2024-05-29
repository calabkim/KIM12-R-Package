#' Prediction for mars Model Objects
#'
#' Generates predictions from a fitted Multivariate Adaptive Regression Splines (MARS) model.
#' This function uses the basis functions and model coefficients to predict the response
#' variable for new data. If no new data is provided, predictions are made for the data used
#' in fitting the `mars` object, equivalent to the fitted values.
#'
#' @param object A `mars` object resulting from fitting a MARS model using the \code{\link{mars}} function.
#' @param newdata An optional data frame containing the predictors for which predictions are desired.
#'                If omitted, predictions are made for the original data used to fit the model.
#'
#' @return A numeric vector containing the predicted values for the response variable.
#' @export
#'
#' @examples
#' # Assuming 'mm' is a fitted 'mars' model
#' predictions <- predict(mm, newdata = some_new_data_frame)
#'
#' @seealso \code{\link{mars}} to fit MARS models, \code{\link{residuals}} to obtain residuals.
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. [Link](https://www.jstor.org/stable/2241837)
#'
#' Additional resources on MARS model fitting, including examples, are available in the STAT 360
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# This function predicts responses using the Multivariate Adaptive Regression Splines (MARS) model
predict.mars <- function(object,newdata) {
# Check if new data is provided; if not, use the model's basis functions
  if(missing(newdata) || is.null(newdata)) {
# Convert the basis function matrix to a matrix for multiplication
    B <- as.matrix(object$B)
  }
  else {
# If new data is provided, process it to match the model's structure
# Extract terms from the model formula, excluding the response variable
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
# Create a model frame based on the processed terms
    mf <- model.frame(tt,newdata)
# Extract terms again to ensure the structure matches that of the training data
    mt <- attr(mf, "terms")
# Create a model matrix from the new data, excluding the intercept
    X <- model.matrix(mt, mf)[,-1]
# Generate the basis function matrix for the new data
    B <- make_B(X,object$Bfuncs)
  }
# Retrieve the model's coefficients
  beta <- object$coefficients
# Multiply the basis function matrix by the coefficients to produce predictions
  drop(B %*% beta)
}

#' Construct Basis Function Matrix for 'mars' Model
#'
#' Utilizes the basis function information (`Bfuncs`) derived from a fitted 'mars' model
#' and the predictor data to construct the basis function matrix essential for making predictions.
#'
#' @param X The model matrix containing predictors only. This matrix should correspond to
#'          the new data for which predictions are intended, or the original data used to
#'          fit the model if predictions on the training data are desired.
#' @param Bfuncs A list detailing the basis functions as derived from a 'mars' model object.
#'               This list encapsulates the model's structural components, including
#'               interactions and transformations applied to the original predictors.
#'
#' @return Returns a matrix where each column represents a basis function derived from
#'         the input predictors, ready to be used for predictions with a 'mars' model's coefficients.
#' @export
#'
#' @examples
#' # Assuming tmars is a fitted mars model:
#' tr <- terms(tmars$formula)
#' mf <- model.frame(tr, concrete, na.action = na.exclude)
#' X <- model.matrix(tr, mf)[, -1, drop = FALSE]  # Exclude the intercept
#' B <- make_B(X, tmars$Bfuncs)
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. Available at [JSTOR](https://www.jstor.org/stable/2241837).
#'
#' Additional examples and materials on MARS and the use of `make_B` can be found in the
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# This function generates a matrix of basis functions for new data based on the model's basis function definitions
make_B<-function(X,Bfuncs){
# Initialize an output matrix with dimensions based on the new data and number of basis functions
  output <- data.frame(matrix(NA,nrow = nrow(X), ncol = length(Bfuncs)))
# Set the first column to 1 for the intercept
  output[[1]] <- rep(1,nrow(X))
# Iterate over each set of basis function definitions except the first (intercept)
  for(i in 2:(length(Bfuncs))){
# Initialize a product of 1 for multiplying hinge functions
    sumpdt = 1
# Iterate over each hinge function in the current set of basis function definitions
    for(k in 1:nrow(Bfuncs[[i]])){
# Multiply the current product by the value of the hinge function applied to the relevant predictor and knot
      sumpdt = sumpdt * h(s=Bfuncs[[i]][k,1],x=X[,Bfuncs[[i]][k,2]],
                          t=Bfuncs[[i]][k,3])
    }
# Assign the product of hinge functions to the corresponding column in the output matrix
    output[[i]]<-sumpdt
  }
# Convert the output data frame to a matrix and return it
  output<-as.matrix(output)
  return(output)
}
