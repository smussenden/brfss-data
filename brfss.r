# Remove all previous objects
rm(list=ls(all=TRUE))

# Load libraries
library('tidyverse')
library('stringr')
library('lubridate')
library('jsonlite')

# Read in data
Y2015 <- read_csv("data/2015.csv", col_names= TRUE)
View(Y2015)
glimpse(Y2015)

# Get names of columns stored as a dataframe
Y2015_codes <- data.frame(colnames(Y2015))
View(Y2015_codes)

# Make a CSV to store the names dataframe
file.create("data/Y2015_codes.csv")

# put our final result predictions into the empty csv file 
write_csv(Y2015_codes, path="data/Y2015_codes.csv", na = "NA", append = FALSE) 
