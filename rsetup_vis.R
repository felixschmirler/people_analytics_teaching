#Script to test your R setup and explore hr_system data

#0. Setup ----

#Load Packages
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load data
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 

#1. hr_system data  ----
#there are a few functions but first pass I would probably inspect with view()
head(hrsystem_data)
str(hrsystem_data)
summary(hrsystem_data)
view(hrsystem_data)

##2. Explore company structure with dplyr ----

#create new object in R with current employees only
current <- hrsystem_data %>%
  filter(employee_status == "Current") %>%
  #filter(employee_status == "Current" & tenure_years > 10) %>% #booleans
  #filter(str_detect(employee_status, "Current")) %>% #regex-based filter with stringr
  select(employee_id, department, job_level, country, gender, tenure_years)

#write objects to file 
write_csv(current, "hr_data/current_employees.csv")

#you can also remove objects and files 
#rm(current)
file.remove("hr_data/current_employees.csv")

#mutate() - create new variables
current %>%
  mutate(
    department_joblevel = paste(department, "-", job_level),
    long_term = tenure_years > 10,
    long_term2 = if_else(tenure_years > 10, "long term", "recent")
  ) %>% view()

current <- current %>%
  mutate(
    department_joblevel = paste(department, "-", job_level),
    long_term2 = if_else(tenure_years > 10, "long term", "recent")
  ) 

#count() - think table
current %>% 
  count(long_term2)

current %>% 
  count(long_term2, gender)

#summarise()
mean(current$tenure_years)
median(current$tenure_years)
sd(current$tenure_years)

current %>% 
  summarise(
    number = n(),
    average_tenure = mean(tenure_years),
    median_tenure = median(tenure_years),
    sd_tenure = sd(tenure_years)
  )

#group_by()
current %>%
  group_by(country) %>%
  summarise(
    number = n(),
    average_tenure = mean(tenure_years),
    median_tenure = median(tenure_years),
    sd_tenure = sd(tenure_years)
  ) %>%
  ungroup()

##2. Explore company structure with ggplot2 ----

#pie charts & stacked barcharts

#barchart
current %>% 
  ggplot(aes(x = long_term2)) +
  geom_bar()

#count() - think table
current %>% 
  count(long_term2) %>%
  ggplot(aes(x = long_term2, y = n)) +
  geom_col() 

#stacked barcharts
current %>%
  ggplot(aes(x = long_term2, fill = country)) +
  geom_bar(position = "fill")

#barcharts for numeric mean comparisons
hrsystem_data %>%
  group_by(employee_status) %>%
  summarise(mean_tenure = mean(tenure_years, na.rm = TRUE)) %>%
  ggplot(aes(x = employee_status, y = mean_tenure)) +
  geom_col() +
  theme_minimal()

#boxplots, histograms and density plots 

#let's look at tenure distribution
hist(hrsystem_data$tenure_years)

#distribution
hrsystem_data %>%
  ggplot(aes(x = tenure_years)) +
  geom_histogram()  
#geom_density() #alternative
#geom_boxplot() #alternative

#distribution by employee status
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  #geom_histogram(position = "identity", alpha = 0.5) #+
  #geom_density(alpha = 0.5)
  geom_boxplot(alpha = 0.5) +
  coord_flip()

