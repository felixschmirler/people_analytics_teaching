# This R script creates synthetic datasets for teaching in people analytics 
# The case study is a European manufacturing organization with ~10,000 employees

library(tidyverse)
library(magrittr)
library(GGally)

#Core HR System Dataset ----
set.seed(187)
hrsystem_data <- 
  tibble(
    employee_id = paste0("emp", 100001:130000), #generate random employee IDs
    job_level = sample(c("Entry Level", "Specialist", "Manager/ Senior Specialist", "Director/ Head of", "Leadership"), 30000, replace = TRUE, prob = c(0.50, 0.325, 0.175, 0.019, 0.001)), #generate job level data
    country = sample(c("Germany", "France", "Italy", "Spain", "Netherlands"), size = 30000, replace = TRUE, prob = c(0.5, 0.15, 0.1, 0.1, 0.15)), #generate country data
    department = sample(c("Consulting", "Sales", "Customer Service", "R&D", "Marketing", "Finance", "HR"), size = 30000, replace = TRUE, prob = c(0.40, 0.15, 0.10, 0.10, 0.05, 0.04, 0.02)), #generate department data
    start_date = sample(seq(as.Date("2015-01-01"), as.Date("2025-04-015"), by = "day"), size = 30000, replace = TRUE),
    start_year = floor_date(start_date, "year"),
    start_month = floor_date(start_date, "month")
  ) 

# #define job - level specific tenurein years
# job_level_tenure_targets <- c(
#   "Entry" = 2,
#   "Specialist" = 3,
#   "Manager/Senior Specialist" = 4,
#   "Director/Head of" = 5,
#   "Executive" = 8
# ) #add department specific and country specific tenure later
# tenure_targets <- job_level_tenure_targets * 365

hrsystem_data %<>% 
  rowwise() %>%
  mutate(
    leaving_date = case_when(
      job_level == "Entry Level" ~ as_date(round(rnorm(1, mean = start_date + 730, sd = 200))),
      job_level == "Specialist" ~ as_date(round(rnorm(1, mean = start_date + 1095, sd = 300))),
      job_level == "Manager/ Senior Specialist" ~ as_date(round(rnorm(1, mean = start_date + 1460, sd = 400))),
      job_level == "Director/ Head of" ~ as_date(round(rnorm(1, mean = start_date + 1825, sd = 500))),
      job_level == "Leadership" ~ as_date(round(rnorm(1, mean = start_date + 2920, sd = 800))),
      TRUE ~ NA
    ),
    leaving_year = floor_date(leaving_date, "year"),
    leaving_month = floor_date(leaving_date, "month"),
    tenure = interval(start_date, leaving_date),
    tenure_years = round(time_length(tenure, "years"), 2)
  ) %>%
  ungroup()
  
hrsystem_data %<>% 
  mutate(
    employee_status = if_else(leaving_date > as.Date("2025-04-15"), "Current", "Leaver"),
    leaving_date = if_else(leaving_date > as.Date("2025-04-15"), NA_Date_, leaving_date)
  ) 

hrsystem_data_long <- hrsystem_data %>% 
  mutate(
    n_events = round(runif(n(), min = 0, max = 1) * tenure_years)
  ) %>% 
  filter(employee_status == "Current" | leaving_date >= as_date("2020-01-01")) %>%  
  select(-start_year, -start_month, -leaving_year, -leaving_month, -tenure) %>%
  uncount(n_events) 

hrsystem_data_long %<>% 
  rowwise() %>%
  mutate(
    event_date = ifelse(!is.na(leaving_date), as.Date(sample(as.numeric(start_date):as.numeric(leaving_date), 1), origin = "1970-01-01"), 
                         as.Date(sample(as.numeric(start_date):as.numeric(as.Date("2025-04-15")), 1), origin = "1970-01-01")
                         ) %>% as.Date(),
    event_type = sample(c("Promotion", "Salary Increase", "Parental Leave", "Department Change"), size = 1, replace = TRUE, prob = c(0.55, 0.15, 0.03, 0.07))
  ) %>%
  ungroup()

hrsystem_data_long %<>%
  mutate(
    event_id = replicate(nrow(hrsystem_data_long), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) %>% 
  arrange(employee_id, event_date) %>% 
  group_by(employee_id) %>% 
  mutate(
    event_no = row_number(),
    update_status = if_else(row_number() == n(), "active", "historic")
    ) 
  
write_csv(hrsystem_data_long, "hr_data/hrsystem_data.csv")


#construction side ----
hrsystem_data %>%
  filter(employee_status == "Current" & start_date > as_date("2024-04-01")) %>% 
  count(employee_status)
,
active_profile = "Active"


event_id = replicate(1, paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))

#5% promotion rate
#2.5% salary increases
#if leaver, last event -> left
 
hrsystem_data %>% group_by(job_level) %>% summarise(av_ten = mean(tenure_years))
hrsystem_data %>% count(employee_status, job_level)  

 hrsystem_data %>% 
   #group_by(job_level) %>% 
  ggplot(aes(tenure_years, fill = employee_status)) +
    geom_density(position = "identity", alpha = 0.4, bins = 100)  
  
  
hrsystem_data %>% 
  filter(leaving_date >= as_date("2020-01-01"))
  select(-leaving_year, -leaving_month) 



#recruitment data ----
#very simple simulation of recruitment data, demographic variables will be added at a later stage and some randomness will be introduced to the number of candidates per stage  
  
  
#helper function to generate ids
generate_ids <- function(n, prefix="emp") {
    sprintf("%s%05d", prefix, seq(1, n))
  }  
  

hires4h <- hrsystem_data %>% filter(start_date >= as_date("2020-01-01")) %>%  select(employee_id, job_level, department) 


#add a few fields to the dataset 

#stage4 hires and rejected
hires4h %<>%
  mutate(
    status = "hired",
    stage = "Stage 4: Team Interview",
    applicant_id = replicate(nrow(hires4h), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = "")),
    job_id = replicate(nrow(hires4h), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = "")),
  ) 

hires4r <- hires4h %>%
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires4h), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires4hr <- 
  rbind(hires4h, hires4r)

#stage 3 progressed and rejected
hires3p <- hires4hr  %>% 
  mutate(
    stage = "Stage 3: HM Interview",
    status = "progressed"
  )


hires3r <- hires3p  %>% 
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires3p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 


hires3pr <- 
  rbind(hires3p, hires3r)


#stage 2 progressed and rejected
hires2p <- hires3pr  %>% 
  mutate(
    stage = "Stage 2: Phone Interview",
    status = "progressed"
  )


hires2r <- hires2p  %>% 
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires2p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 


hires2pr <- 
  rbind(hires2p, hires2r)

#stage 1 progressed and rejected
hires1p <- hires2pr  %>% 
  mutate(
    stage = "Stage 1: CV Screening",
    status = "progressed"
  )


hires1ra <- hires1p  %>% 
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1rb <- hires1p  %>% 
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1rc <- hires1p  %>% 
  mutate(
    employee_id = "",
    job_level = "",
    department = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1pr <- 
  rbind(hires1p, hires1ra, hires1rb, hires1rc)


#combine data from all steps

applicant_data <- rbind(hires4hr, hires3pr, hires2pr, hires1pr) 
applicant_data %<>% 
  select(job_id, applicant_id, stage, status, employee_id)
write_csv(applicant_data, "hr_data/applicant_data.csv")

#360 degree feedback data ----
leadership_roles <- hrsystem_data %>%
  filter(job_level %in% c("Manager/ Senior Specialist", "Director/ Head of") & employee_status == "Current") %>%
  slice_sample(prop = 0.6)

competencies <- c("Inspire with Purpose", "Drive for Results", "Own the Outcome", 
                  "Speak Up and Listen", "Lead with Empathy", "Grow Others",
                  "Bridge Across Boundaries", "Act with Integrity", "Make it Simple",
                  "Lead Change Fearlessly", "Learn and Adapt Fast", "Champion Diversity") #missing autonomy support, and setting boundaries
all_vars <- c(paste("Self:", competencies), paste("Manager:", competencies), paste("Peer:", competencies), paste("Reports:", competencies))

#self ratings round(rnorm(12, 4.4, 0.1), 2) 
#manager ratings round(rnorm(12, 3.9, 0.1), 2) 
#peer ratings round(rnorm(12, 4.1, 0.1), 2) 
#report ratings round(rnorm(12, 4.1, 0.1), 2) 

feedback <- leadership_roles %>% 
  select(employee_id) %>%
  mutate(
    `Self: Inspire with Purpose` = rnorm(nrow(leadership_roles), 4.34 , 0.4) %>% round(0) %>% pmin(5),       
    `Self: Drive for Results` =  rnorm(nrow(leadership_roles), 4.51 , 0.4) %>% round(0) %>% pmin(5),         
    `Self: Own the Outcome` = rnorm(nrow(leadership_roles), 4.42  , 0.4) %>% round(0) %>% pmin(5),               
    `Self: Speak Up and Listen` = rnorm(nrow(leadership_roles), 4.43  , 0.4) %>% round(0)  %>% pmin(5),         
    `Self: Lead with Empathy` = rnorm(nrow(leadership_roles), 4.32  , 0.4) %>% round(0) %>% pmin(5),           
    `Self: Grow Others` = rnorm(nrow(leadership_roles), 4.30   , 0.4) %>% round(0)  %>% pmin(5),               
    `Self: Bridge Across Boundaries` = rnorm(nrow(leadership_roles), 4.35  , 0.4) %>% round(0)  %>% pmin(5),     
    `Self: Act with Integrity` = rnorm(nrow(leadership_roles), 4.37  , 0.4) %>% round(0) %>% pmin(5),        
    `Self: Make it Simple` = rnorm(nrow(leadership_roles), 4.33 , 0.4) %>% round(0) %>% pmin(5),        
    `Self: Lead Change Fearlessly` = rnorm(nrow(leadership_roles), 4.47 , 0.4) %>% round(0) %>% pmin(5),      
    `Self: Learn and Adapt Fast` = rnorm(nrow(leadership_roles), 4.29 , 0.4) %>% round(0) %>% pmin(5),        
    `Self: Champion Diversity` = rnorm(nrow(leadership_roles), 4.42 , 0.4) %>% round(0) %>% pmin(5),     
    
    `Manager: Inspire with Purpose` = rnorm(nrow(leadership_roles), 3.92 , 0.4) %>% round(0) %>% pmin(5),    
    `Manager: Drive for Results` = rnorm(nrow(leadership_roles), 4.05 ) %>% round(0) %>% pmin(5),       
    `Manager: Own the Outcome` = rnorm(nrow(leadership_roles), 3.74 , 0.4) %>% round(0) %>% pmin(5),         
    `Manager: Speak Up and Listen` = rnorm(nrow(leadership_roles), 3.96 , 0.4 ) %>% round(0) %>% pmin(5),     
    `Manager: Lead with Empathy` = rnorm(nrow(leadership_roles), 3.84 , 0.4) %>% round(0) %>% pmin(5),       
    `Manager: Grow Others` = rnorm(nrow(leadership_roles), 4.00  , 0.4) %>% round(0) %>% pmin(5),             
    `Manager: Bridge Across Boundaries` = rnorm(nrow(leadership_roles), 3.86  , 0.4) %>% round(0) %>% pmin(5),
    `Manager: Act with Integrity` = rnorm(nrow(leadership_roles), 3.95 , 0.4) %>% round(0) %>% pmin(5),      
    `Manager: Make it Simple` = rnorm(nrow(leadership_roles), 3.92  , 0.4) %>% round(0) %>% pmin(5),          
    `Manager: Lead Change Fearlessly` = rnorm(nrow(leadership_roles), 3.97  , 0.4) %>% round(0) %>% pmin(5),  
    `Manager: Learn and Adapt Fast` = rnorm(nrow(leadership_roles), 3.93  , 0.4) %>% round(0) %>% pmin(5),    
    `Manager: Champion Diversity` = rnorm(nrow(leadership_roles), 3.74, 0.4) %>% round(0) %>% pmin(5),      
    
    `Peers: Inspire with Purpose` = rnorm(nrow(leadership_roles), 4.09  , 0.4)  %>% pmin(5),    
    `Peers: Drive for Results` = rnorm(nrow(leadership_roles), 4.15  )  %>% pmin(5),       
    `Peers: Own the Outcome` = rnorm(nrow(leadership_roles), 3.99 , 0.4)  %>% pmin(5),         
    `Peers: Speak Up and Listen` = rnorm(nrow(leadership_roles), 4.23 , 0.4)  %>% pmin(5),     
    `Peers: Lead with Empathy` = rnorm(nrow(leadership_roles), 4.28  , 0.4)  %>% pmin(5),       
    `Peers: Grow Others` = rnorm(nrow(leadership_roles), 4.11 , 0.4)  %>% pmin(5),             
    `Peers: Bridge Across Boundaries` = rnorm(nrow(leadership_roles), 4.14  , 0.4) %>% pmin(5) ,
    `Peers: Act with Integrity` = rnorm(nrow(leadership_roles), 4.12 , 0.4) %>% pmin(5) ,      
    `Peers: Make it Simple` = rnorm(nrow(leadership_roles), 3.98  , 0.4)  %>% pmin(5),          
    `Peers: Lead Change Fearlessly` = rnorm(nrow(leadership_roles), 4.04    , 0.4)  %>% pmin(5),  
    `Peers: Learn and Adapt Fast` = rnorm(nrow(leadership_roles), 4.19   , 0.4)  %>% pmin(5),    
    `Peers: Champion Diversity` = rnorm(nrow(leadership_roles), 3.97, 0.4)  %>% pmin(5),  
    
    `Reports: Inspire with Purpose` = rnorm(nrow(leadership_roles), 4.11  , 0.4)  %>% pmin(5),    
    `Reports: Drive for Results` = rnorm(nrow(leadership_roles), 4.20  )  %>% pmin(5),       
    `Reports: Own the Outcome` = rnorm(nrow(leadership_roles), 4.13  , 0.4)  %>% pmin(5),         
    `Reports: Speak Up and Listen` = rnorm(nrow(leadership_roles), 3.86  , 0.4  %>% pmin(5)) ,     
    `Reports: Lead with Empathy` = rnorm(nrow(leadership_roles), 4.19 , 0.4)  %>% pmin(5),       
    `Reports: Grow Others` = rnorm(nrow(leadership_roles), 4.04 , 0.4)  %>% pmin(5),             
    `Reports: Bridge Across Boundaries` = rnorm(nrow(leadership_roles), 4.16  , 0.4)  %>% pmin(5),
    `Reports: Act with Integrity` = rnorm(nrow(leadership_roles), 3.97  , 0.4)  %>% pmin(5),      
    `Reports: Make it Simple` = rnorm(nrow(leadership_roles), 4.24  , 0.4)  %>% pmin(5),          
    `Reports: Lead Change Fearlessly` = rnorm(nrow(leadership_roles), 4.11   , 0.4)  %>% pmin(5),  
    `Reports: Learn and Adapt Fast` = rnorm(nrow(leadership_roles), 4.15    , 0.4)  %>% pmin(5),    
    `Reports: Champion Diversity` = rnorm(nrow(leadership_roles), 4.13 , 0.4)  %>% pmin(5)
  )
 

for (i in 2:49) {
  hist(feedback[[i]])
}



#ggpairs(feedback[-1])
feedback %>% summarise()

write_csv(feedback, "hr_data/feedback.csv")

#under construction
generate_feedback_scores <- function(n) {
  rnorm(n, mean = 4, sd = 0.5) %>% pmin(5) %>% pmax(1)
}