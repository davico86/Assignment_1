#Load library for analysis
library(dplyr)
library(tidyr)

#Set working directory
setwd("~/Documents/R_Training/Assignments/Assignment_1_05April2016/")

# Read csv file:
mydata <- read.csv(file="refine.csv", stringsAsFactors = FALSE)

#Place data in table data frame: 
brands.data <- data.frame(mydata)

brands.data$company
#Find the subindices in which the "phillips" brand is in,
#either correctly, or incorrectly spelled 
vec <- grep("*s", brands.data$company, ignore.case = TRUE)

#subindices in brands.data$company for which pattern was found
vec_size <- length(vec)

#iterate over the vector where "phillips" brand was  found and 
#impose value = "phillips"
for (i in 1: vec_size)
{
  brands.data[vec[i], 1] <- "phillips"
}
#Show effects of change
brands.data$company

#Part 1: Change brand names to standard spelling
#Same procedure for van houten
vec1 <- grep("n$", brands.data$company, ignore.case = TRUE)

#subindices in brands.data$company for which pattern was found
vec1_size <- length(vec1)

#iterate over the vector where "van houten" brand was  found and 
#impose value = "van houten"
for (i in 1: vec1_size)
{
  brands.data[vec1[i], 1] <- "van houten"
}
#Show effects of change
brands.data$company


vec2 <- grep("*o$", brands.data$company, ignore.case = TRUE)

#subindices in brands.data$company for which pattern was found
vec2_size <- length(vec2)

#iterate over the vector where "akzo" brand was  found and 
#impose value = "akzo"
for (i in 1: vec2_size)
{
  brands.data[vec2[i], 1] <- "akzo"
}
#Show effect of change
brands.data$company

vec3 <- grep("*0$", brands.data$company, ignore.case = TRUE)

#subindices in brands.data$company for which pattern was found
vec3_size <- length(vec3)

#iterate over the vector where "akzo" brand was  found and 
#impose value = "akzo"
for (i in 1: vec3_size)
{
  brands.data[vec3[i], 1] <- "akzo"
}
#Show effect of change
brands.data$company


vec4 <- grep("*r$", brands.data$company, ignore.case = TRUE)

#subindices in brands.data$company for which pattern was found
vec4_size <- length(vec4)

#iterate over the vector where "unilever" brand was  found and 
#impose value = "unilever"
for (i in 1: vec4_size)
{
  brands.data[vec4[i], 1] <- "unilever"
}
#Show effect of change
brands.data$company

#Part two: separate product by code and number
brands.data.1 <-data.frame(separate(brands.data, Product.code...number, into=c("code","number"), sep = "-", remove = TRUE, convert = TRUE))

#Show data result
brands.data.1

#Part three: add column displaying product type given code
# p = Smartphone ; v = TV ; x = Laptop ; q = Tablet 
brands.data.1$Product_name <- "0"
for (i in 1: length(brands.data.1$company))
{
  if (brands.data.1$code[i] == "x")
  {
    brands.data.1$Product_name[i] <- "Laptop"
  }
  if (brands.data.1$code[i] == "p")
  {
    brands.data.1$Product_name[i] <- "Smartphone"
  }
  if (brands.data.1$code[i] == "v")
  {
    brands.data.1$Product_name[i] <- "TV"
  }
  if (brands.data.1$code[i] == "q")
  {
    brands.data.1$Product_name[i] <- "Tablet"
  }
}

#Part five: Put together address, city, country into column: full_address 
brands.data.2 <- data.frame(unite(brands.data.1, full_address, address, city, country, sep = ","))
brands.data.2

#Part six: Include column with company code:
# 0001 = phillips
# 0010 = akzo
# 0100 = van houten 
# 1000 = unilever
brands.data.2$Company_code <- "0"
for (i in 1: length(brands.data.1$company))
{
  if (brands.data.2$company[i] == "phillips")
  {
    brands.data.2$Company_code[i] <- "0001"
  }
  if (brands.data.2$company[i] == "akzo")
  {
    brands.data.2$Company_code[i] <- "0010"
  }
  if (brands.data.2$company[i] == "van houten")
  {
    brands.data.2$Company_code[i] <- "0100"
  }
  if (brands.data.2$company[i] == "unilever")
  {
    brands.data.2$Company_code[i] <- "1000"
  }
}

brands.data.2

#Part six: Include column with product:
# 0011 = Smartphone
# 0110 = Tablet
# 0101 = Laptop 
# 1001 = TV

brands.data.2$Product_code <- "0"
for (i in 1: length(brands.data.2$Product_code))
{
  brands.data.2$Product_name[i]
  if (brands.data.2$Product_name[i] == "Smartphone")
  {
    brands.data.2$Product_code[i] <- "0011"
  }
  if (brands.data.2$Product_name[i] == "Tablet")
  {
    brands.data.2$Product_code[i] <- "0110"
  }
  if (brands.data.2$Product_name[i] == "Laptop")
  {
    brands.data.2$Product_code[i] <- "0101"
  }
  if (brands.data.2$Product_name[i] == "TV")
  {
    brands.data.2$Product_code[i] <- "1001"
  }
}

brands.data.2

#Print data into "refine_clean.csv"
write.table(brands.data.2, file = "refine_clean.csv")