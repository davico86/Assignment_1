#Load library for analysis
library(dplyr)
library(tidyr)

#Set working directory
setwd("~/Documents/R_Training/Assignments/Assignment_2_12April2016/")


# Part 0: Read csv file:
cat("\014")
titanic3.df <- data.frame(read.csv(file="titanic3.csv", stringsAsFactors = FALSE))

titanic3.df

#Part1: Find missing value in embark column and replace it with "S"
vec_size <- length(titanic3.df$embarked)
idx <- -1
for (i in 1:vec_size)
{
  if (titanic3.df$embarked[i] == "")
  { 
    idx <- i
    print(i)
    titanic3.df$embarked[i] <- "S"
  }
}
#print(idx)

titanic3.df$embarked

#Part2: Filling in missing age values:
#1.
avg_age <- mean(titanic3.df$age, na.rm = TRUE)
vec_size <- length(titanic3.df$age)

for (i in 1:vec_size)
{
  if (is.na(titanic3.df$age[i]))
  {
    titanic3.df$age[i] <- avg_age
  }
}

print(titanic3.df$age)

#2. The second choice would have been by either using the mode or the
# median. Most preferably the mode. This way we can avoid any outlier
#influence in the assuming the age of the perope whose age was unknown.

#Part3: Fill in with "NA" all empry slots in boat column
vec_size <- length(titanic3.df$boat)

for (i in 1:vec_size)
{
  if (titanic3.df$boat[i] == "")
  {
    titanic3.df$boat[i] <- "NA"
  }
}

print(titanic3.df$boat)

#Part4: Determine whether a passenger was found or was not found
