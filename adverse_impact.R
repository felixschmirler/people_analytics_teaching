#A few simple example for analysing adverse impact in recruitment

# Load necessary library
library(tidyverse) #a million handy functions for data analysis and visualisation
library(stats) #basic package for all sorts of regression models

# Create the dataset ---- 
gender <- c(rep("Female", 340), rep("Male", 538)) # Gender
hired <- c(rep(1, 35), rep(0, 340 - 35), rep(1, 70), rep(0, 538 - 70)) # Hired status: 1 = hired

# Combine into a data frame
gender_data <- data.frame(Gender = gender, Hired = hired)


#Perform Chi-Square test ----

# Create a summary table 
gender_table <- table(gender_data) %>% as.matrix()

#chi-square test 
chisq.test(gender_table)

