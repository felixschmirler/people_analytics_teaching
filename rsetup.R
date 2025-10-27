#Script to test your R setup and explore the different HR datasets

#Setup ----

#Load Packages
#install.packages("tidyverse")
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load Datasets
applicant_data <- read_csv("hr_data/applicant_data.csv")
assessmentcenter_data <- read_csv("hr_data/assessmentcenter_data.csv")
employeesurvey_data <- read_csv("hr_data/employeesurvey_data.csv")
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 
feedback_data <- read_csv("hr_data/feedback.csv")
personality_data <- read_csv("hr_data/personality_data.csv")
survey_comments <- read_csv("hr_data/survey_comments.csv")

#Applicant Data ----


#Exploration (Messy!) ----