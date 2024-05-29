## code to prepare `marstestdata` dataset goes here

# Set a seed for reproducibility of random data
set.seed(123)
# Initialize parameters for the simulation
N <- 100; n <- 10 # Number of observations and Number of predictors
knot1 <- -0.5; knot2 <- 0; knot3 <- 0.5 # Knot positions for the hinge functions
beta1 <- 3; beta2 <- 5 # Coefficients for the hinge functions
error.SD <- 0.1 # Standard deviation of the error term
# Generate random predictor values, creating an N by n matrix of predictors
x <- matrix(rnorm(N*n),ncol=n)
# Define the hinge function to be used in generating the response variable
h <- function(x,s,t) {
  return(pmax(0,s*(x-t)))
}
# Calculate the linear predictor using specified hinge functions and coefficients
lin.pred <- beta1*h(x[,1],+1,knot1) + beta2*h(x[,2],-1,knot2)*h(x[,1],+1,knot3)
# Generate the response variable by adding normally distributed errors to the linear predictor
y <- lin.pred + rnorm(n,sd=error.SD)
# Combine the response variable and predictors into a single data frame
marstestdata <- data.frame(cbind(y,x))
# Assign meaningful column names to the data frame
names(marstestdata) <- c("y",paste0("x",1:n))
# Save the generated dataset in the package's data directory for later use
# If a dataset with the same name exists, it will be overwritten
usethis::use_data(marstestdata, overwrite = TRUE)

