#Script to Explore the different HR Datasets

#Setup ----

#Load Packages
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load Datasets
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 
applicant_data <- read_csv("hr_data/applicant_data.csv")
feedback_data <- read_csv("hr_data/feedback.csv")

#HR System Dataset ----

#Filter
hrsystem_data %>%
  filter(update_status == "active") %>%
  view()

#equivalent, Schreibweise in base R, wird unübersichtlich
view(filter(hrsystem_data, update_status == "active"))

#how many new starters did we have in the last 12 months
hrsystem_data %>%
  filter(update_status == "active", start_date > as_date("2024-04-01")) %>% #auch booleans möglich z.B. & oder |
  view()

#how many promotions did we have in the last 12 months?
hrsystem_data %>%
  filter(update_status == "active", 
         event_type == "Promotion", 
         event_date > as_date("2024-04-01")) %>% 
  view() #wait, that looks like a lot!

hrsystem_data %>%
  filter(update_status == "active", 
         event_type == "Promotion", 
         event_date > as_date("2024-04-01"),
         employee_status == "Current") %>% 
  view() #forgot to filter out leavers, makes sense but still a lot

promotions_24_25 <- hrsystem_data %>%
  filter(update_status == "active", 
         event_type == "Promotion",
         event_date > as_date("2024-04-01"),
         employee_status == "Current")

write_csv(promotions_24_25, "hr_data/promotions_24_25.csv") #write data to file

#Select
promotions_24_25 %>% view() #confusing, too many variables

promotions_24_25 %>% 
  select(employee_id, event_date, country, job_level, department, start_date, tenure_years) %>% view()

#arrange
promotions_24_25 %>% 
  select(employee_id, event_date, country, job_level, department, start_date, tenure_years) %>% 
  arrange(event_date, country, department) %>% 
  view()

#mutate
promotions_24_25 %>% 
  select(employee_id, event_date, country, job_level, department, start_date, tenure_years) %>% 
  arrange(event_date, country, department) %>% 
  mutate(
    new_starter = if_else(start_date > (Sys.Date() - 90), "New Starter", "")
  ) %>% 
  view()

promotions_24_25_new <- promotions_24_25 %>% 
  select(employee_id, event_date, country, job_level, department, start_date, tenure_years) %>% 
  arrange(event_date, country, department) %>% 
  mutate(
    new_starter = if_else(start_date > (Sys.Date() - 90), "New Starter", NA)
  ) 

write_csv(promotions_24_25_new, "hr_data/promotions_24_25.csv") #overwrite existing file

#summarise
promotions_24_25_new %>% 
  summarise(
    average_tenure = mean(tenure_years),
    n_newstarter = sum(new_starter == "New Starter", na.rm = TRUE)
  ) %>% 
  view()
  
#group_by
promotions_24_25_new %>% 
  group_by(country) %>%
  summarise(
    average_tenure = mean(tenure_years),
    n_newstarter = sum(new_starter == "New Starter", na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  view()

#clean up promotions example
rm(promotions_24_25)
rm(promotions_24_25_new)
file.remove("hr_data/promotions_24_25.csv")

#pivot
hrsystem_data %>% 
  pivot_wider(id_cols = employee_id, 
              names_from = event_no, 
              values_from = event_type) %>%
  view()

hrsystem_data %>%
  pivot_wider(id_cols = c(employee_id, country, department, job_level) , 
              names_from = event_no, 
              values_from = event_type) %>%
  view()

hrsystem_data %>%
  pivot_wider(id_cols = c(employee_id, country, department, job_level) , 
              names_from = event_no, 
              values_from = c(event_type, event_date)) %>%
  view()

#explore visualisations
#only look at active employee updates 
#active updates includes the latest update to a file, also for leavers
hr_system_active <- hrsystem_data %>%
  filter(update_status == "active")

#let's look at tenure distribution
hist(hr_system_active$tenure_years)

hr_system_active %>%
  ggplot(aes(x = tenure_years)) +
  #geom_histogram()  #alternative
  #geom_density() #alternative
  geom_boxplot() #alternative

hr_system_active %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  geom_histogram(position = "identity", alpha = 0.5)  

#let's plot the number of employees by country
hr_system_active %>% 
  ggplot(aes(x = country)) +
  geom_bar()

#lets split by employee status
hr_system_active %>% 
  ggplot(aes(x = country, fill = employee_status)) +
  geom_bar()

#ATS (Applicant Tracking System) Data ----

#360 Degree Feedback Data ----

#Combining Datasets ----

#join

#what was the feedback  for those  who received a promotion in the last 12 months?
names(promotions_24_25_new)
names(feedback_data)

promotions_24_25_new %>%
  left_join(feedback_data) %>% 
  view()
#if names don't happen to match -> left_join(feedback_data, by = c("employee_id", "employee_id"))

#Messy code ----