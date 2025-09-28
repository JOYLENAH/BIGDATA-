#BABIRYE JOY LENAH
#b28490
#assignment2


# Install DuckDB if not yet installed
install.packages("duckdb")

# Load DuckDB library
library(DBI)
library(duckdb)

# Create or connect to an in-directory database called "Assignment2"
con <- dbConnect(duckdb(), dbdir = "Assignment2.duckdb", read_only = FALSE)

# Confirm the connection works
con

#question2
# Point to the folder where you saved your datasets
setwd("C:/Users/DIL/Documents/Assignment2")
# Check that R can see the datasets
list.files()

install.packages("readr")   # install if not already installed
library(readr)              # load into memory


# Question3,List of the dataset file names
File_Name = c(
  "UNCTAD_DE.csv",
  "UN_EGDI.csv",
  "ILO_EMP.csv",
  "WB_SSGD.csv",
  "ITU_DH.csv",
  "GDIP_DVC.csv",
  "WB_ID4D.csv",
  "IMF_GENDER_EQUALITY.csv",
  "WIPO_ICT.csv",
  "ITU_ICT.csv"
)
# Loop through your file names and write them into the database
for (f in File_Name) {
  data <- read.csv(f)                                # read file
  table_name <- tools::file_path_sans_ext(f)         # remove .csv
  dbWriteTable(con, table_name, data, overwrite = TRUE)
}
dbListTables(con)


#question4
#mining to see missimg datasets
for (tbl in dbListTables(con)) {
  cat("\n---- Missing Data Summary for", tbl, "----\n")
  
  # Load table from DuckDB
  data <- dbReadTable(con, tbl)
  
  # Count missing values per column
  print(colSums(is.na(data)))
}
#question 5 ,descripitive statistics
#we start by defining a helper function which will summarize any numeric variable

# Function to generate descriptive statistics
desc_stats <- function(x) {
  c(
    Count = length(x),
    Missing = sum(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
}
#applying one variable for each dataset

for (tbl in dbListTables(con)) {
  cat("\n---- Descriptive Stats for", tbl, "----\n")
  
  # Load dataset from DuckDB
  data <- dbReadTable(con, tbl)
  
  # Pick the first numeric column automatically
  num_cols <- sapply(data, is.numeric)
  
  if (any(num_cols)) {
    first_num_col <- names(data)[which(num_cols)[1]]
    cat("Variable analyzed:", first_num_col, "\n")
    
    stats <- desc_stats(data[[first_num_col]])
    print(stats)
  } else {
    cat("⚠️ No numeric variables found in this dataset\n")
  }
}




