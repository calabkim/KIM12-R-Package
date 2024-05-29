## code to prepare `concrete` dataset goes here

# Reading Excel files
library(readxl)
# Read the Excel file containing concrete data
dd <- read_excel("C:/Users/82108/Desktop/SFU course/stat360/KIM12/rawdata/Concrete_Data.xls")
# Rename columns for better readability and to match expected format
names(dd)<-c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water",
             "Superplasticizer", "CoarseAggregate", "FineAggregate",
             "Age","ConcreteCompressiveStrength")
# Select the first 100 rows for analysis to limit the dataset size or for preliminary analysis
dd <- dd[1:100,]
# Store the selected data into a new variable for further processing
concrete <- dd
# Rearrange columns if necessary, here it seems redundant since columns are already in order
concrete <- concrete[c("ConcreteCompressiveStrength","Cement",
                       "BlastFurnaceSlag", "FlyAsh", "Water",
                       "Superplasticizer", "CoarseAggregate", "FineAggregate",
                       "Age")]
# Convert the modified data into a data frame, though `dd` is likely already a data frame from `read_excel`
concrete<-data.frame(concrete)
# Save the prepared dataset into the package's data directory, overwriting any existing file of the same name
usethis::use_data(concrete, overwrite = TRUE)

