#Script to Explore the different HR Datasets

#Setup ----

#Load Packages
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load Datasets
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 
applicant_data <- read_csv("hr_data/applicant_data.csv")
feedback_data <- read_csv("hr_data/feedback.csv")
assessementcenter_data <- read_csv("hr_data/assessmentcenter_data.csv")

#visualisations ----

#let's look at tenure distribution
hist(hrsystem_data$tenure_years)

#distribution
hrsystem_data %>%
  ggplot(aes(x = tenure_years)) +
  geom_histogram()  #alternative
#geom_density() #alternative
#geom_boxplot() #alternative

#distribution by employee status
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  geom_histogram(position = "identity", alpha = 0.5)  

#better to look at this with a density plot to understand proportions 
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  geom_density(position = "identity", alpha = 0.5) 

#let's look at a factor with more levels - a bit crowded
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = job_level)) +
  geom_density(position = "identity", alpha = 0.5) 

#facet wrap can help
hrsystem_data %>%
  #filter(department != "Production") %>% #data looks bimodal because of Production, let's look at data without Production
  ggplot(aes(x = tenure_years)) +
  geom_density(position = "identity", alpha = 0.5) +
  facet_wrap(~job_level)

#not normally distributed, maybe boxplot can help
hrsystem_data %>%
  filter(department != "Production") %>%
  ggplot(aes(x = tenure_years)) +
  geom_boxplot(position = "identity", alpha = 0.5) +
  facet_wrap(~job_level)


#let's plot the number of employees by country
hrsystem_data %>% 
  ggplot(aes(x = country)) +
  geom_bar()

#lets split by employee status
hrsystem_data %>% 
  ggplot(aes(x = country, fill = employee_status)) +
  geom_bar()

#this is the same as count + geom_col
hrsystem_data %>%
  count(country, employee_status) %>% #view()
  ggplot(aes(country))

#we can also produce barplots with geom_col but we need to summarise the data ourselves first



#Aufgaben
#   Wie unterscheidet sich die Verteilung der Mitarbeiter auf die verschiedenen Job Level in den verschiedenen Abteilungen?



#   Wie sind die verschiedenen Departments auf die verschiedenen Länder verteilt?
hrsystem_data %>% 
  filter(employee_status == "Current") %>%
  count(country, department)

hrsystem_data %>% 
  filter(employee_status == "Current") %>%
  ggplot(aes(country, fill = department)) +
  geom_bar()


#   Wie unterscheidet sich die typische Tenure zwischen Ländern, Departments und Job Leveln?
#   Wie ist die Geschlechterverteilung in den verschiedenen Departments und job levels?
#   Gibt es beim einstellen neuer Mitarbeiter einen Bias?
#   Welchen Score braucht man in dem Sales Assessment Center ungefähr um eingestellt zu werden? 
#   Ist das Assessment Center ein valides Instrument, um Performance vorherzusagen? 
#   Hat ein Geschlecht bessere Performance Bewertungen?





