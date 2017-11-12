# Remove all previous objects from the data set
rm(list=ls(all=TRUE))

# Load libraries
library('tidyverse')
library('stringr')
library('lubridate')

# Read in the 2015 data 
Y2015 <- read_csv("data/2015.csv", col_names= TRUE)
View(Y2015)

# Create a subset of the data that only contains data that only contains computed variables by finding columns with underscores at the start https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf

data <- Y2015 %>%
  select(starts_with("_"))

# Then rename the columns to get rid of the stupid underscores that are screwing stuff up and make them lowercase. 
names(data) <- names(data) %>%
  tolower() %>%
  str_replace_all("_", "")
View(data)

# First, filter only rows that contain binge drinking 1 (no), 2 (yes) (removing the 9 and other values). Then remove five columns that have lots of NA values to get a workable dataset. And then omit any rows with NA values. 
binge <- data %>% 
  filter(rfbing5 == c(1,2)) %>%
  select(-cllcpwt,-crace1,-cprace,-flshot6,-pneumo2) %>%
  na.omit()

# Create a table to count NAs by column to get a nice list of most problematic columns, which helped me figure out what to take out in the columns above. Don't actually need to run this again, but here in case I need it again.
na_count <-sapply(binge, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

# Now let's split our data into a test set and train set
# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled 
n <- nrow(binge)
shuffled <- binge[sample(n),]

# Split the data in train and test
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# EXPLORATORY ANALYSIS - RASHMI FIGURING OUT

# LOGISTIC REGRESSION MODELS --  SEAN FIGURING OUT

# DECISION TREES -- GRACE FIGURING OUT

# SUPPORT VECTOR MACHINE -- ALEX FIGURING OUT

