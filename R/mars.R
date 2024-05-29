#' Fit Multivariate Adaptive Regression Splines (MARS) Model
#'
#' @description
#' Implements Friedman's Multivariate Adaptive Regression Splines (MARS) to model complex relationships between a response variable and multiple predictors. MARS models are capable of capturing non-linearities and interactions between variables through piecewise linear or polynomial functions.
#'
#' @param formula A `formula` object, similar to those used in `lm()`, specifying the model to be fitted. The formula must include the response variable and one or more predictors.
#' @param data A `data.frame` containing the dataset over which the model is to be fitted, including both the response and the predictor variables.
#' @param control A `mars.control` object specifying the tuning parameters for the MARS algorithm: `Mmax` (maximum number of basis functions), `d` (degree of interaction), and `trace` (logical flag for step-by-step tracing of model selection). Defaults are set based on Friedman's recommendations. For detailed configuration, refer to `?mars.control`.
#'
#' @return Returns an object of class `mars`, containing comprehensive details of the fitted model, analogous to `lm` objects but enriched with specific MARS attributes:
#' \itemize{
#'   \item{call}{The original function call.}
#'   \item{formula}{The model formula used.}
#'   \item{y}{Response variable vector.}
#'   \item{B}{Matrix of basis functions selected during model fitting.}
#'   \item{Bfuncs}{Details on the construction of each basis function.}
#'   \item{x_names}{Names of predictor variables.}
#'   \item{coefficients}{Model coefficients.}
#'   \item{residuals}{Residuals from the fitted model.}
#'   \item{effects}{Orthogonal effects.}
#'   \item{rank}{Rank of the model.}
#'   \item{fitted.values}{Fitted values.}
#'   \item{assign}{Assignment of coefficients.}
#'   \item{qr}{QR decomposition of the model matrix.}
#'   \item{df.residual}{Degrees of freedom for residuals.}
#'   \item{xlevels}{Levels of factors used in fitting.}
#'   \item{terms}{Terms object from the model fitting.}
#'   \item{model}{The model frame used.}
#' }
#'
#' @export
#'
#' @examples
#' # Fitting a MARS model to predict concrete compressive strength
#' mm <- mars(ConcreteCompressiveStrength ~ ., data = concrete)
#'
#' @importFrom stats lm
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics, 19(1), pp. 1-67. [Link](https://www.jstor.org/stable/2241837)
#'
#' STAT 360 GitHub Repository by Becky Lin: [STAT360](https://github.com/Becky07/STAT360)
#'
#' R Documentation on Linear Models (`lm`): [lm in R](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm)
#'
#' R Documentation on Model Effects: [effects in R](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/effects)
#'
#' R Documentation on Model Terms: [terms in R](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/terms)


mars <- function(formula,data,control = mars.control()) {
# Record the function call for reproducibility.
  cc <- match.call()
# Create a model frame based on the input formula and data. This will include the response variable and predictors.
  mf <- model.frame(formula,data)
# Extract the response variable from the model frame.
  y <- model.response(mf)
# Retrieve the terms attribute from the model frame, which contains the structure of the model specified by the formula.
  mt <- attr(mf, "terms")
# Create a model matrix from the terms object, excluding the intercept (the first column).
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
# Store the names of the predictor variables for later use.
  x_names <- colnames(x)
# Ensure the control parameters are valid and properly formatted.
  control <- validate_mars.control(control)
# Perform the forward stepwise selection to identify potential basis functions.
  fwd <- fwd_stepwise(y,x,control)
# Refine the selected basis functions using backward elimination.
  bwd <- bwd_stepwise(fwd,control)
# Fit a linear model using the refined basis functions. The intercept is explicitly excluded (-1) because MARS handles it internally.
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B))
# Compile the results, including the original function call, formula, response variable, selected basis functions, their construction details, and the final fitted model.
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
# Assign a class to the output list for method dispatching. The 'mars' class allows for the use of specialized methods, such as print or predict.
  class(out) <- c("mars",class(fit))
# Return the final MARS model object.
  out
}

#' Forward Stepwise Selection for MARS Model
#'
#' @description
#' Implements Algorithm 2 by Friedman to construct a matrix of basis functions for a MARS model through forward stepwise selection. The function iteratively selects pairs of hinge functions that minimize the Lack of Fit (LOF) measure, using the Generalized Cross-Validation (GCV) criterion, to build the model's basis functions.
#'
#' @param y Numeric vector representing the response variable.
#' @param x Model matrix of predictor variables.
#' @param control A `mars.control` object specifying parameters for the model fitting process, including `Mmax` (the maximum number of basis functions, excluding the intercept), `d` (degree of interaction terms), and `trace` (boolean flag for tracing model building progress). For detailed configuration, refer to `?mars.control`.
#'
#' @return Returns a list with components:
#' \itemize{
#'   \item{y}{Column vector of the response variable.}
#'   \item{B}{Matrix containing the selected basis functions. Each column represents a basis function constructed as products of hinge functions, characterized by the sign, predictor number, and split point.}
#'   \item{Bfuncs}{List detailing the construction of each basis function in `B`. Each list element corresponds to a column in `B` and may contain multiple rows, each describing a hinge function (including sign, variable, and split point).}
#' }
#' @export
#'
#' @examples
#' # Example of forward stepwise selection on the concrete dataset
#' fwd_result <- fwd_stepwise(y = concrete$ConcreteCompressiveStrength,
#'                            x = concrete[,-1],
#'                            control = mars.control(Mmax = 8, d = 3, trace = FALSE))
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics, Vol. 19, No. 1, pp. 1-67. Available at: [JSTOR](https://www.jstor.org/stable/2241837)
#'
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}
#'
#'
fwd_stepwise <- function(y,x,control=mars.control()){
# Ensure the maximum number of basis functions (Mmax) is at least 2 for the model to be meaningful.
  if(control$Mmax < 2) {
    warning("\nMmax was less than 2, therefore adjusted to 2\n")
    control$Mmax <- 2
  }
# Initialize variables for the algorithm.
  N <- length(y) # The number of observations in the response variable.
  n <- ncol(x) # The number of predictors.
# Initialize the matrix for basis functions (B) and a list to hold details of these basis functions (Bfuncs).
  B <- init_B(N,control$Mmax)
# Begin the forward stepwise selection process.
  Bfuncs <- vector(mode="list", length = control$Mmax+1)
  for(i in 1:(control$Mmax/2)) {
    M = (2*i)-1 # Calculate the current number of basis functions.
    lof_best <- Inf # Initialize the best lack of fit (LOF) score.
# Iterate over existing basis functions to find the best new pair to add.
    for(m in 1:M) {
      uni <- setdiff(1:n,Bfuncs[[m]][,2]) # Exclude variables already used in the current basis function.
# Iterate over remaining variables to find the best split.
      for(v in uni){
        tt <- split_points(x[,v],B[,m]) # Find potential split points for the variable.
# Evaluate each potential split point.
        for(t in tt) {
# Create temporary basis functions for evaluation.
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(s=+1,x=x[,v],t=t),
                             Btem2=B[,m]*h(s=-1,x=x[,v],t=t))
# Calculate the lack of fit (LOF) for the model with the temporary basis functions.
          gdat <- data.frame(y=y,Bnew)
# If this model has a lower LOF, remember it as the best so far.
          lof <- LOF(form = y~.-1,data = gdat,control=control)
          if(lof < lof_best) { # Keep track of the best split.
            lof_best <- lof
            note <- c(m=m,v=v,t=t)
          }
        }
      }
    }
# Update the basis function matrix with the best new pair found.
    mstar <- note["m"]
    vstar <- note["v"]
    tstar <- note["t"]
    B[,M+1] <- c(B[,mstar])*h(x=x[,vstar], s=-1, t=tstar)
    B[,M+2] <- c(B[,mstar])*h(x=x[,vstar], s=+1, t=tstar)
# Update the Bfuncs list with details of the new basis functions.
    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]],c(s=-1, v=vstar, t=tstar))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]],c(s=1, v=vstar, t=tstar))
  }
# Finalize the names of the basis function columns.
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  temp <- Bfuncs[-1]
  temp <- lapply(temp,`dimnames<-`,list(NULL,c("s","v","t")))
  Bfuncs <- c(Bfuncs[1],temp)
# Return the selected basis functions and their details.
  return(list(y=y,B=B,Bfuncs = Bfuncs))
}

#' Backward stepwise function based on Algorithm 3 by Freifdman
#'
#' @description
#' Base on the output of forward selection, perform backward selection using
#' basis functions to reduce over-fitting.
#'
#' @param fwd output of forward stepwise function, it is a list of vector of
#' response, Basis function matrix and Bfuncs which recorded the information of
#' basis functions.
#' @param control an object of class 'mars.control': a list of Mmax, d, and trace,
#' see helper file mars.control() for more information.
#'
#' @return a list containing the following information:
#' \itemize{
#' \item{y:}{ the column vector of response variable.}
#' \item{B:}{ a matrix that contains information of basis functions reduced from
#' the B in forward selection to avoid overfitting. Each column of basis functions are
#' products of hinge functions with information of sign, number of predictor and
#' split point given in Bfuncs in order.}
#' \item{Bfuncs:}{ a list containing information on how each basis function
#' is created, each element of Bfuncs correspond to the hinge functions that are
#' used to create each column of B. Inside each element of Bfuncs, there could be
#' null, 1 or multiples rows. Each row represents information of each hinge function.}
#' }
#'
#' @export
#'
#' @examples
#' fwdtest <- fwd_stepwise(y=concrete$ConcreteCompressiveStrength,x=concrete[,-1],
#' control = mars.control(8,3,FALSE))
#'
#' bwdtest <- bwd_stepwise(fwdtest,control = mars.control(8,3,FALSE))
#'
#' @references
#'
#' i)
#'
#' Paper: Multivariate Adaptive Regression Splines
#'
#' Author(s): Jerome H. Friedman
#'
#' Source: The Annals of Statistics , Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#'
#' Published by: Institute of Mathematical Statistics Stable
#'
#' URL: https://www.jstor.org/stable/2241837
#'
#' ii)
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}
#'
#'
bwd_stepwise<-function(fwd,control){
# 'fwd' is a list containing the model's response variable 'y',
# the matrix of basis functions 'B', and the list 'Bfuncs' detailing how each
# basis function was constructed.

# Initialize key variables for backward elimination.
  Mmax<-ncol(fwd$B)-1 # Calculate the maximum number of basis functions initially considered.
  Jstar<-2:(Mmax+1)  # Indices of basis functions to potentially keep in the model.
  Kstar<-Jstar # A copy of Jstar to work with in the algorithm.
  dat <- data.frame(y=fwd$y,fwd$B) # Data frame combining response and basis functions.
  lofstar<-LOF(form = y~.-1,data = dat,control=control) # Calculate the initial LOF with all basis functions.
# Begin backward selection by iteratively removing the least effective basis functions.
   for(M in (Mmax+1):2){ # Iterate from the full model down to a minimal model.
    b<-Inf # Set a high initial value for the best LOF score found in this iteration.
    L<-Kstar # Candidate basis functions for removal.
    if(control$trace) cat("L",L,"\n") # Optionally print the set of candidate functions for removal.
# Test removal of each candidate basis function for its impact on model LOF.
    for(m in L){
      K <- setdiff(L,m)  # Temporarily remove a basis function.
      dat2 <- data.frame(y=fwd$y,fwd$B[,c(1,K)]) # Recreate model data without the basis function.
      lof <- LOF(form = y~.-1,data = dat2,control=control) # Calculate LOF without the basis function.
# Update best LOF score and record basis functions set if this is the best combination found so far.
        if(lof<b) # Update the set of basis functions for the next iteration.
          {
        b<-lof  # Update the best LOF score.
        Kstar <- K # Update the set of basis functions for the next iteration.
      }
      if(lof<lofstar){
        lofstar<-lof # Update the overall best LOF score if this combination is the best seen across all iterations.
        Jstar <- K # Update the final set of basis functions based on the best LOF score.
      }
    }
  }
# Finalize the model by selecting the basis functions that minimize the LOF.
  Jstar <- c(1,Jstar)  # Include the intercept in the final set of basis functions.
# Return a list containing the final model's components: response variable, basis functions, and their details.
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}

#' Initialize Basis Function Matrix for MARS Model
#'
#' @description
#' Initializes a matrix intended to store basis functions during the forward
#' stepwise selection process of a MARS model fitting. The matrix is pre-filled
#' with a column of ones to represent the intercept and NA values for placeholders
#' of potential basis functions to be determined.
#'
#' @param N The total number of observations in the dataset.
#' @param Mmax The maximum number of basis functions (excluding the intercept)
#' to be considered during the model fitting process. This value is dictated by
#' the 'Mmax' parameter in a `mars.control` object.
#'
#' @return Returns a matrix where the first column is filled with ones to
#' represent the intercept, and the remaining columns are initialized with NA
#' values, awaiting the assignment of calculated basis functions. The dimensions
#' of the matrix are N rows by `Mmax` + 1 columns.
#' @export
#'
#' @examples
#' # Initializing a basis function matrix for 100 observations with up to 10 basis functions
#' Bmatrix <- init_B(N = 100, Mmax = 10)
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' 19(1), pp. 1-67. [Link to the article](https://www.jstor.org/stable/2241837).
#'
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}

init_B <- function(N,Mmax) {
# Initialize a data frame 'dd' with 'N' rows and 'Mmax + 1' columns filled with NA values.
# This matrix will store the basis functions, including the intercept term.
  dd <- data.frame(matrix(NA,nrow = N, ncol = Mmax + 1))
# The first column is set to 1s, representing the intercept term in the model.
  dd[[1]] <- rep(1,N)
# Name the columns of 'dd' as B0, B1, ..., BMmax, where each 'B' represents a basis function.
  names(dd)<-paste0("B",0:Mmax)
# Return the initialized matrix of basis functions.
  return(dd)
}

#' Calculate Lack of Fit (LOF) Using Generalized Cross-Validation (GCV)
#'
#' @description
#' Computes the Lack of Fit (LOF) metric, applying the Generalized Cross-Validation (GCV)
#' criterion for a given linear model. The GCV criterion is defined as:
#' \deqn{RSS \times N / (N - \tilde{C}(M))^2}, where \eqn{RSS} is the residual sum of
#' squares, \eqn{N} is the number of observations, and \eqn{\tilde{C}(M)} is the effective
#' degrees of freedom, calculated as the sum of the diagonal elements of the hat matrix
#' (hat-values) plus the product of \eqn{d} and the number of model coefficients excluding
#' the intercept.
#'
#' @param form A `formula` object for the linear model, specifying the response variable
#' and predictors. It must include `-1` to explicitly exclude the intercept, aligning with
#' MARS model conventions.
#' @param data A `data.frame` containing both the response and predictor variables as
#' specified in `form`.
#' @param control A `mars.control` object containing tuning parameters for the MARS model
#' fitting process: `Mmax` (maximum number of basis functions), `d` (penalty parameter for
#' model complexity), and `trace` (boolean flag to enable tracing of the fitting process).
#'
#' @return The function returns the LOF metric calculated using the GCV criterion for the
#' specified model and data.
#' @export
#'
#' @examples
#' lof_value <- LOF(
#'   form = ConcreteCompressiveStrength ~ . - 1,
#'   data = concrete,
#'   control = mars.control()
#' )
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of
#' Statistics, 19(1), pp. 1-67. [DOI: https://doi.org/10.1214/aos/1176347963](https://www.jstor.org/stable/2241837)
#'
#' For further examples and insights into MARS model fitting, refer to the STAT 360
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.
#'
LOF <- function(form,data,control) {
# Fit a linear model (lm) using the provided formula 'form' and data 'data'.
# The formula includes '-1' to exclude the intercept since it's explicitly handled in the basis functions.
  mod <- lm(form,data)
# Calculate the residual sum of squares (RSS) from the fitted model.
  rss <- sum((mod$res)^2)
# Get the number of observations 'N' and the number of basis functions 'M' (excluding the intercept).
  N = nrow(data)
  M = length(mod$coefficients)-1
# Calculate the sum of the diagonal of the hat matrix (hatvalues), which measures the model's leverage.
  cm = sum(diag(hatvalues(mod)))
# Retrieve the 'd' parameter from the 'mars.control' object, which affects the smoothing of the model.
  d = control$d
# Calculate an adjusted measure 'tildacm' that incorporates the 'd' parameter and the number of basis functions.
  tildacm = cm + (d*M)
# Compute the lack of fit (LOF) measure using the Generalized Cross-Validation (GCV) criterion.
# This formula penalizes the model based on its complexity (number of basis functions) and smoothness.
  out = rss * (N/((N-tildacm)^2))
# Return the LOF measure.
  return(out)
}

#' Apply Hinge Function to Predictor Variable
#'
#' @description
#' The hinge function is a fundamental component in constructing basis functions for Multivariate Adaptive Regression Splines (MARS). It modifies the predictor variable values based on a specified split point and direction (sign). This function facilitates non-linear mapping of the input variable, enabling piecewise linear modeling in MARS.
#'
#' @param x Numeric vector representing the predictor variable.
#' @param s Numeric scalar indicating the direction of the hinge function: +1 for values greater than the split point and -1 for values less than the split point.
#' @param t Numeric scalar specifying the split point (knot) where the hinge function effect begins.
#'
#' @return Returns a numeric vector where each element is adjusted according to the hinge function: elements greater (or less) than the split point are modified based on the sign `s`; elements not meeting the condition are set to zero.
#' @export
#'
#' @examples
#' # Apply hinge function to the 'Cement' predictor in the concrete dataset
#' # with a split point at 300 and positive direction
#' adjusted_values <- h(x = concrete$Cement, s = 1, t = 300)
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics, 19(1), pp. 1-67. [DOI: https://doi.org/10.1214/aos/1176347963](https://www.jstor.org/stable/2241837)
#'
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# Defines a hinge function that is a fundamental component of the MARS model.
# This function returns the positive part of a linear transformation of 'x'.
h <- function(x,s,t) {
# x: input variable
# s: sign, determines the direction of the hinge (1 for positive, -1 for negative)
# t: knot, the point at which the hinge activates
  return(pmax(0,s*(x-t)))
}

#' Identify Unique Split Points for MARS Model
#'
#' @description
#' Determines unique and valid split points for a given predictor variable within
#' the context of Multivariate Adaptive Regression Splines (MARS). Split points
#' are critical for constructing basis functions that capture non-linear patterns
#' in the data. This function filters out the maximum value to avoid redundant
#' splits at the boundary.
#'
#' @param xvar Numeric vector representing the predictor variable from which
#' split points are to be identified.
#' @param Bm Numeric vector representing the mth column of the basis function
#' matrix. This parameter is used to ensure that the chosen split points are
#' valid within the context of the current model fitting process.
#'
#' @return Returns a sorted and unique vector of valid split points derived from
#' `xvar`, excluding its maximum value to ensure meaningful partitioning of the
#' predictor space.
#' @export
#'
#' @examples
#' # Example of obtaining split points from the 'Cement' variable
#' # after an initial step in forward stepwise selection
#' fwd_result <- fwd_stepwise(y = concrete$ConcreteCompressiveStrength, x = concrete[,-1],
#'                            control = mars.control(Mmax = 6, d = 3, trace = FALSE))
#' split_pts <- split_points(concrete$Cement, fwd_result$B[, 3])
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. [Link to the article](https://www.jstor.org/stable/2241837)
#'
#' Additional insights and examples on MARS and its implementation can be found in the
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.
#'

# Identifies potential knots (split points) for creating hinge functions based on a specific basis function.
split_points <- function(xvar,Bm) {
# 'xvar': the predictor variable
# 'Bm': the current basis function, used to determine which observations are active (non-zero)
  temp <- xvar[Bm>0] # Select the active parts of 'xvar' based on 'Bm'
  result <- sort(unique(temp)) # Remove duplicates and sort
  result <- result[-length(result)] # Exclude the last value to prevent redundant splits
  return(result)
}


#-----------------------------------------------------------------------------------------
# This section defines tools for working with 'mars.control' objects.
# 'mars.control' objects specify the configuration for the MARS model fitting procedure.
#-----------------------------------------------------------------------------------------

#' Constructor for 'mars.control' objects
#'
#' Creates a 'mars.control' object containing parameters that dictate how the MARS model
#' will be fitted. This function allows users to specify their preferences for the
#' maximum number of basis functions (Mmax), the penalty parameter (d) for model complexity
#' in the GCV criterion, and whether to trace the model fitting process (trace).
#'
#' @param control A list specifying the values for Mmax, d, and trace.
#'                Mmax controls the maximum number of basis functions, including both
#'                main effects and interactions. The d parameter is used in the calculation
#'                of the generalized cross-validation (GCV) score, penalizing model complexity.
#'                Setting trace to TRUE enables printing of detailed output during the model
#'                fitting process, which can help in understanding the selection of basis functions.
#'
#' @return Returns an object of class 'mars.control', ready to be used in MARS model fitting.
#' @export
#'
#' @examples
#' # Creating a mars.control object with specific parameters
#' mc <- new_mars.control(list(Mmax=10, d=3, trace=TRUE))
#'
#' @references
#'
#' i) Paper: Multivariate Adaptive Regression Splines
#'    Author(s): Jerome H. Friedman
#'    Source: The Annals of Statistics, Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#'    Published by: Institute of Mathematical Statistics Stable
#'    URL: https://www.jstor.org/stable/2241837
#'
#' ii) STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.
#'
#' The constructor simplifies the process of setting up MARS model parameters, while the
#' validator ensures the provided values are appropriate for model fitting, improving
#' usability and robustness of the MARS modelling process.

# Constructs a 'mars.control' object from a provided list of control parameters.
new_mars.control <- function(control) {
# 'control': list of control parameters
  structure(control,class="mars.control") # Assign 'mars.control' class to the list
}

#' Constructor for 'mars.control' Objects
#'
#' Constructs a `mars.control` object, specifying parameters for the fitting process of a Multivariate Adaptive Regression Splines (MARS) model. This function allows users to easily configure the key parameters that control the behavior of MARS model fitting.
#'
#' @param control A list specifying the configuration for MARS model fitting. The list can include:
#' \itemize{
#'   \item{\code{Mmax}}{Maximum number of basis functions to be used in the model, excluding the intercept. Should be an integer.}
#'   \item{\code{d}}{Degree of interaction allowed in the model. Typically set to 1 (additive model) or 2 (includes interactions).}
#'   \item{\code{trace}}{Logical flag indicating whether to trace the model fitting process. Useful for debugging or learning purposes.}
#' }
#' It's essential to provide these parameters correctly to ensure the MARS model fits as expected.
#'
#' @return Returns an object of class \code{'mars.control'}, which encapsulates the configuration for fitting a MARS model.
#' @export
#'
#' @examples
#' # Creating a mars.control object with specified parameters
#' mc <- new_mars.control(list(Mmax = 10, d = 3, trace = TRUE))
#'
#' @seealso \code{\link{mars}} for how the `mars.control` object is utilized in fitting MARS models.
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. [DOI: https://doi.org/10.1214/aos/1176347963](https://www.jstor.org/stable/2241837)
#'
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# Validates the 'mars.control' object to ensure parameters are correctly specified.
validate_mars.control <- function(control) {
# Check that 'Mmax' is an integer, 'd' is numeric, and 'trace' is logical
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
# Adjust 'Mmax' if it's less than 2 or not an even integer
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
# Return the validated or adjusted 'mars.control' object
  control
}

#' Construct 'mars.control' Object for MARS Model Fitting
#'
#' Constructs a `mars.control` object specifying the configuration parameters for
#' Multivariate Adaptive Regression Splines (MARS) model fitting. This includes settings
#' for the maximum number of basis functions, the parameter for Generalized Cross-Validation
#' (GCV) calculation, and an option to enable tracing of the model fitting process.
#'
#' @param Mmax The maximum number of basis functions to include in the model, excluding
#' the intercept. Must be an even integer greater than or equal to 2. Default is 2.
#' @param d Degree of penalty in the GCV criterion, controlling the trade-off between
#' model fit and complexity. Jerome H. Friedman suggests d = 3 as a generally effective
#' value. Default is 3.
#' @param trace Logical flag to enable verbose output during the model fitting process,
#' particularly useful for tracking the addition of candidate basis functions and their
#' impact on the LOF (Lack of Fit) measured by GCV. Default is FALSE.
#'
#' @return Returns a `mars.control` object, a list containing the specified settings
#' for MARS model fitting.
#' @export
#'
#' @examples
#' # Default settings
#' mc_default <- mars.control()
#'
#' # Customized settings
#' mc_custom <- mars.control(Mmax = 10, d = 3, trace = TRUE)
#'
#' @references
#' Friedman, J. H. (1991). "Multivariate Adaptive Regression Splines." The Annals of Statistics,
#' Vol. 19, No. 1, pp. 1-67. [Link](https://www.jstor.org/stable/2241837)
#'
#' For more information and examples on MARS model fitting, see the STAT 360 course materials
#' STAT 360 & STAT361 course materials: \url{https://github.com/grace84/SFUStat360Projects}.

# Creates a 'mars.control' object with default or specified control parameters.
mars.control <- function(Mmax = 2,d = 3,trace = FALSE) {
# Convert Mmax to an integer and create a list of control parameters
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
# Validate the control parameters and return a new mars.control object
  control <- validate_mars.control(control)
  new_mars.control(control)
}
