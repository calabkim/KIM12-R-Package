# Example script demonstrating the use of mars() function and its methods
# within the package. This script covers three examples, each illustrating
# the use of mars() with different datasets and the application of five S3 methods
# for class 'mars': summary(), print(), predict(), anova(), and plot().

# Ensure the package is loaded. Uncomment the following lines if you're
# running this script in a development environment.
# library(devtools)
# load_all()

# Example 1: Concrete Strength Prediction

# Loading the concrete dataset
data(concrete)

# Creating a mars.control object with 10 basis functions
mc <- mars.control(Mmax = 10)

# Fitting a MARS model to the concrete dataset
concretemars <- mars(ConcreteCompressiveStrength~., data=concrete, control=mc)

# Displaying a summary of the model, highlighting key statistical measures and basis functions
summary(concretemars)

# Printing the model, focusing on the formula and coefficients
print(concretemars)

# Making predictions using the model. This step demonstrates how to use the model for predictions
predict(concretemars)

# Performing ANOVA on the model to understand the variance explained by each predictor
anova(concretemars)

# Plotting diagnostic plots for model assessment
plot(concretemars)

# Examples 2 (Fish Toxicity) and 3 (Yacht Hydrodynamics) follow a similar structure,
# demonstrating the versatility of the mars() function across different datasets.
# The mars.control() parameters are adjusted according to the specifics of each dataset,
# showcasing the customizability of the MARS modeling process.
# For brevity, comments on repetitive steps are omitted in these examples.

# Example 2: Fish Toxicity Prediction

# Load the 'fishToxicity' dataset available in the package
data(fishToxicity)
# Create a mars.control object with maximum 8 basis functions and trace enabled to monitor the selection process
mc <- mars.control(Mmax=8, trace=TRUE)
# Fit the Multivariate Adaptive Regression Splines (MARS) model to the 'fishToxicity' dataset
# The model predicts 'LC50' based on all other variables in the dataset, guided by the control parameters set earlier
toxicmars <- mars(LC50~., data=fishToxicity, control=mc)
# Display a summary of the fitted 'toxicmars' model, including statistical information and basis function details
summary(toxicmars)
# Print the model object, showing its structure and components such as the formula used, basis functions, and coefficients
print(toxicmars)
# Predict the response variable 'LC50' using the fitted 'toxicmars' model. This uses the model's basis functions and coefficients for prediction
predict(toxicmars)
# Perform an ANOVA (Analysis of Variance) to examine the significance of the basis functions included in the 'toxicmars' model
anova(toxicmars)
# Perform an ANOVA (Analysis of Variance) to examine the significance of the basis functions included in the 'toxicmars' model
plot(toxicmars)

# Example 3: Yacht Hydrodynamics

# The process repeats for a different dataset 'yacht', with similar steps to fit, summarize, print, predict, analyze, and plot the 'yachtmars' model
# This time, the model is tasked with predicting 'RRPUWOD' based on all other variables in the 'yacht' dataset, with up to 10 basis functions

# Load the 'yacht' dataset
data(yacht)
# Set control parameters for the MARS model fitting process, specifying a maximum of 10 basis functions and enabling trace
mc <- mars.control(Mmax=10, trace=TRUE)
# Fit the MARS model to the 'yacht' dataset, predicting 'RRPUWOD' from all other variables
yachtmars <- mars(RRPUWOD~., data=yacht, control=mc)
# Display a summary of the 'yachtmars' model
summary(yachtmars)
# Print the 'yachtmars' model object
print(yachtmars)
# Predict the response variable 'RRPUWOD' using the 'yachtmars' model
predict(yachtmars)
# Perform an ANOVA for the 'yachtmars' model
anova(yachtmars)
# Plot diagnostic graphs for the 'yachtmars' model
plot(yachtmars)

