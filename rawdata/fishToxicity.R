## code to prepare `fishToxicity` dataset goes here

# Loading the data from excel file
library(readxl)
# Read the Excel file containing fish toxicity data from a specified path
dd <- read_excel("C:/Users/82108/Desktop/SFU course/stat360/KIM12/rawdata/fish_toxicity.xlsx")
# Assign meaningful column names to the dataset for easier reference
names(dd)<-c("CIC0", "SM1_Dz_Z", "GATS1i","NdsCH", "NdssC", "MLOGP", "LC50")
# Select only the first 100 rows for analysis, possibly for initial exploration or due to resource constraints
dd <- dd[1:100,]
# Store the filtered dataset into a new variable for further manipulation or analysis
fishToxicity <- dd
# (This step appears to reorder columns to a specific preference, but since the order is unchanged, it may be unnecessary)
fishToxicity <- fishToxicity[c("LC50","CIC0", "SM1_Dz_Z", "GATS1i","NdsCH",
                               "NdssC", "MLOGP")]
# Ensure the dataset is in a data frame format, although it's likely already a data frame from `read_excel`
fishToxicity <-data.frame(fishToxicity)
# Save the prepared dataset into the package's data directory, allowing for easy access and reuse within the package
# This action will overwrite any existing dataset of the same name
usethis::use_data(fishToxicity, overwrite = TRUE)


