## code to prepare `yacht` dataset goes here

# Load data from library to read Excel files
library(readxl)
# Read the yacht dataset from the specified Excel file into a dataframe
dd <- read_excel("C:/Users/82108/Desktop/SFU course/stat360/KIM12/rawdata/yacht data.xlsx")
# Rename columns to more descriptive names for easier reference
names(dd)<-c("LPOTCOB", "PrismaticCoefficient", "LengthDisplacementRatio",
             "BeamDraughtRatio", "LengthBeamRatio", "FroudeNumber",
             "RRPUWOD")
# Select the first 100 rows of the dataset for analysis to manage the dataset size or focus on a subset
dd <- dd[1:100,]
# Assign the modified dataframe to a new variable for clarity and to signify its readiness for use
yacht <- dd
# Reorder columns in the yacht dataframe for analysis or presentation preferences
yacht <- yacht[c("RRPUWOD","LPOTCOB", "PrismaticCoefficient",
                 "LengthDisplacementRatio", "BeamDraughtRatio",
                 "LengthBeamRatio", "FroudeNumber")]
# Convert the yacht dataset to a dataframe, ensuring it's in the appropriate format for R functions
yacht <-data.frame(yacht)
# Save the yacht dataset within the package's data directory for later use or distribution
# The `overwrite = TRUE` option allows replacing any existing dataset with the same name
usethis::use_data(yacht, overwrite = TRUE)

