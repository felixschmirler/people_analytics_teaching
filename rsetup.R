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

#Applicant Data (load and view data test run) ----
applicant_data
class(applicant_data)
head(applicant_data)
str(applicant_data)
summary(applicant_data)
view(applicant_data)

#base R (no pipe operator) - can get hard to navigate
view(filter(applicant_data, status == "hired"))

#tidyverse (with pipe operator) - makes code easier to read and adapt later
applicant_data %>%
  filter(status == "hired") %>%
  view()

#create new objects in R
hires <- applicant_data %>%
  filter(status == "hired") 

#write objects to file 
write_csv(hires, "hr_data/hires.csv")

#you can also remove objects and files 
rm(hires)
file.remove("hr_data/hires.csv")

#HR System Data (dplyr test run) ----

#filter() - subsets of your data by categorical variables or numeric variables
#how many employees do we have?
hrsystem_data %>%
  filter(employee_status == Current) %>% 
  view()

#how many new starters did we have since April last year that are still here?
hrsystem_data %>%
  filter(start_date > as_date("2024-04-01"), employee_status == Current) %>% #date formats can be tricky! e.g. dmy()
  view()

#count() - think table
hrsystem_data %>% 
  count(employee_status)

hrsystem_data %>% 
  count(employee_status, gender)

#select() - less variables are easier to navigate (also when you want to join them with other data)
hrsystem_data %>% 
  select(job_level, country, department, tenure_years) %>% 
  view()

#arrange() - sort
hrsystem_data %>% 
  select(job_level, country, department, tenure_years) %>%
  arrange(-tenure_years) %>% view()

#mutate() - create new variables
hrsystem_data %>%
  mutate(
    department_joblevel = paste(department, "-", job_level)
  ) %>% view()

hrsystem_data <- hrsystem_data %>%
  mutate(
    department_joblevel = paste(department, "-", job_level)
  ) 

#summarise()
mean(hrsystem_data$tenure_years)
median(hrsystem_data$tenure_years)
sd(hrsystem_data$tenure_years)

hrsystem_data %>% 
  summarise(
    average_tenure = mean(tenure_years),
    median_tenure = median(tenure_years),
    sd_tenure = sd(tenure_years)
  )


#group_by()
hrsystem_data %>%
  group_by(country) %>%
  summarise(
    average_tenure = mean(tenure_years),
    median_tenure = median(tenure_years),
    sd_tenure = sd(tenure_years)
  ) %>%
  ungroup()


#Exploration (Messy!) ----

#weird code from chat gpt
# install.packages(c("lavaan", "semPlot"))  # if needed
library(lavaan)
library(semPlot)

# Example data that ships with lavaan
data("HolzingerSwineford1939")

# 1) Confirmatory factor analysis (measurement model)
cfa_model <- '
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

cfa_fit <- cfa(cfa_model, data = HolzingerSwineford1939, std.lv = TRUE)
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)

# Optional: visualise the CFA
semPaths(cfa_fit, "std", whatLabels = "std", edge.label.cex = .9, curvePivot = TRUE)

# 2) Structural model (latent regressions on top of the measurement model)
sem_model <- '
  # Measurement
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9

  # Structural paths
  textual ~ visual
  speed   ~ visual + textual

  # (Optional) Allow latent covariances not implied by regressions
  # visual ~~ speed
'

sem_fit <- sem(sem_model, data = HolzingerSwineford1939, std.lv = TRUE)
summary(sem_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Modification indices (to see potential model improvements)
modindices(sem_fit, sort. = TRUE, minimum.value = 10)

# Plot the SEM (standardised estimates)
semPaths(sem_fit, "std", whatLabels = "std", edge.label.cex = .9, curvePivot = TRUE)

