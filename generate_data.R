# This R script creates synthetic datasets for teaching in people analytics 
# The case study is a European manufacturing organization with ~10,000 employees

library(tidyverse)
library(magrittr)

#Core HR System Dataset ----
#load example proportions
props <- read_csv("proportions_dataset_example.csv")
props %<>% 
  mutate(
    department_joblvl = paste0(department, "-", job_level)
  )
dpt_lvl <- props$department_joblvl
new_prop <- props$prop_new_2038
departments <- props %>% select(department) %>% distinct()
country_prop <- props %>% select(department, matches("prop_depfrom")) %>% distinct() %>% t() 
colnames(country_prop) <- country_prop[1, ]      # assign first row as row names
country_prop <- country_prop[-1, ]   
country_prop %<>% as_tibble()
av_tenure <- props %>% select(department_joblvl, tenure_av)


set.seed(187)
simulate_year <- function(year) {
  n <- rnorm(1, 2038, 50)  
  tibble(
    start_date = sample(seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by = "day"), size = n, replace = TRUE),
    department_joblvl = sample(dpt_lvl, size = n, replace = TRUE, prob = new_prop)
  )
}


# Simulate over 25 years:
hrsystem_data <- map_df(2000:2024, simulate_year)

#temp helper column with average expected tenure 
hrsystem_data %<>% left_join(av_tenure)    


hrsystem_data %<>% #generate country data
  separate(department_joblvl, sep = "-", into = c("department", "job_level")) %>% 
  mutate(
    country = case_when(
      department == "Customer Service" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                                size = length(department == "Customer Service"), replace = TRUE, 
                                                prob = country_prop$`Customer Service`),
      department == "Finance" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                       size = length(department == "Finance"), replace = TRUE, 
                                       prob = country_prop$Finance),
      department == "HR" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                  size = length(department == "HR"), replace = TRUE, 
                                  prob = country_prop$HR),
      department == "Marketing" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                         size = length(department == "Marketing"), replace = TRUE, 
                                         prob = country_prop$Marketing),
      department == "Production" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                          size = length(department == "Production"), replace = TRUE, 
                                          prob = country_prop$Production),
      department == "R&D" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                   size = length(department == "R&D"), replace = TRUE, 
                                   prob = country_prop$`R&D`),
      department == "Sales" ~ sample(c("Germany", "Czechia", "France", "Italy", "Netherlands"), 
                                     size = length(department == "Sales"), replace = TRUE, 
                                     prob = country_prop$Sales)
      
    )
  ) 
  

hrsystem_data %<>% #generate tenure and leaving data 
  rowwise() %>%
  mutate(
    tenure = round(rnorm(1, mean = tenure_av, sd = (tenure_av/4)), 2),
    tenure = if_else(tenure < 0, 0, tenure),
    leaving_date = start_date + tenure * 365,
    employee_status = if_else(leaving_date > as_date("2024-12-31"), "Current", "Leaver"),
    leaving_date = if_else(leaving_date > as_date("2024-12-31"), NA_Date_, leaving_date)
  ) %>% 
  ungroup() 
  
#add start and leaving year/ month for simpler plotting + employee id
hrsystem_data %<>%
  arrange(start_date) %>% 
  mutate(
    employee_id = paste0("emp", 1:nrow(hrsystem_data) + 10000), #generate random employee IDs
    start_year = floor_date(start_date, "year"),
    start_month = floor_date(start_date, "month"),
    leaving_year = floor_date(leaving_date, "year"),
    leaving_month = floor_date(leaving_date, "month"),
    tenure_years = if_else(employee_status == "Current", interval(start_date, as_date("2024-12-31")), interval(start_date, leaving_date)),
    tenure_years = round(time_length(tenure_years, "years"), 1),
    update_status = "active"
  )

#generate employee id
hrsystem_data %<>% 
  filter(job_level != "Leadership") %>%
  #filter(leaving_date > as_date("2009-12-31") | employee_status == "Current") %>% 
  select(-tenure_av, -tenure) 

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")


#under construction - come back to at later stage, job history needs to be different for different departments etc.

# #create a random job history
# hrsystem_data_long <- hr_data %>% 
#   mutate(
#     n_events = round(runif(n(), min = 0, max = 1) * tenure)
#   ) %>% 
#   filter(employee_status == "Current" | leaving_date >= as_date("2020-01-01")) %>%  
#   select(-start_year, -start_month, -leaving_year, -leaving_month, -tenure_av) %>%
#   uncount(n_events) 
# 
# hrsystem_data_long %<>% 
#   rowwise() %>%
#   mutate(
#     event_date = ifelse(!is.na(leaving_date), as.Date(sample(as.numeric(start_date):as.numeric(leaving_date), 1), origin = "1970-01-01"), 
#                         as.Date(sample(as.numeric(start_date):as.numeric(as.Date("2025-04-15")), 1), origin = "1970-01-01")
#     ) %>% as.Date(),
#     event_type = sample(c("Promotion", "Salary Increase", "Parental Leave", "Department Change"), size = 1, replace = TRUE, prob = c(0.55, 0.15, 0.03, 0.07))
#   ) %>%
#   ungroup()
# 
# hrsystem_data_long %<>%
#   mutate(
#     event_id = replicate(nrow(hrsystem_data_long), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
#   ) %>% 
#   arrange(employee_id, event_date) %>% 
#   group_by(employee_id) %>% 
#   mutate(
#     event_no = row_number(),
#     update_status = if_else(row_number() == n(), "active", "historic")
#   ) 


#recruitment data ----
#very simple simulation of recruitment data, demographic variables will be added at a later stage and some randomness will be introduced to the number of candidates per stage  
  
  
#helper function to generate ids
generate_ids <- function(n, prefix="emp") {
    sprintf("%s%05d", prefix, seq(1, n))
  }  
  
#load hrsystem data
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv")
hires4h <- hrsystem_data %>% 
  #filter(start_date >= as_date("2015-01-01")) %>%  
  select(employee_id, job_level, department, start_month) 


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
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1rb <- hires1p  %>% 
  mutate(
    employee_id = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1rc <- hires1p  %>% 
  mutate(
    employee_id = "",
    status = "rejected",
    applicant_id = replicate(nrow(hires1p), paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = ""))
  ) 

hires1pr <- 
  rbind(hires1p, hires1ra, hires1rb, hires1rc)


#combine data from all steps

applicant_data <- rbind(hires4hr, hires3pr, hires2pr, hires1pr) 


#add different hiring stages for production 
applicant_data %<>% 
  mutate(
    status = case_when(
      department == "Production" & job_level == "Entry Level/ Manual" & str_detect(employee_id, "emp") & str_detect(stage, "Stage 3") ~ "hired",
      department == "Production" & job_level == "Entry Level/ Manual" & !str_detect(employee_id, "emp") & str_detect(stage, "Stage 3") ~ "rejected",
      TRUE ~ status
    ),
    stage = case_when(
      department == "Production" & job_level == "Entry Level/ Manual" & str_detect(stage, "Stage 3") ~ "Stage 3: On-site interview",
      TRUE ~ stage
    )
  ) %>% 
  filter(!(department == "Production" & job_level == "Entry Level/ Manual" & str_detect(stage, "Stage 4"))) 

#add assessment centre stage for sales
applicant_data %<>% 
  mutate(
    stage = case_when(
      department == "Sales" & job_level == "Entry Level/ Manual" & str_detect(stage, "Stage 4") ~ "Stage 4: Assessment Center",
      TRUE ~ stage
    )
  ) 

#add gender data

gender_probs_hire <- tribble(
  ~department,        ~job_level,                 ~p_woman,
  
  # Customer Service
  "Customer Service", "Entry Level/ Manual",        0.65,
  "Customer Service", "Specialist/ Supervisor",     0.60,
  "Customer Service", "Manager/ Senior Specialist", 0.55,
  "Customer Service", "Director/ Head of",          0.50,
  "Customer Service", "Leadership",                0.45,
  
  # Finance
  "Finance", "Entry Level/ Manual",        0.50,
  "Finance", "Specialist/ Supervisor",     0.45,
  "Finance", "Manager/ Senior Specialist", 0.40,
  "Finance", "Director/ Head of",                 0.35,
  "Finance", "Leadership",          0.30,
  
  # HR
  "HR", "Entry Level/ Manual",        0.70,
  "HR", "Specialist/ Supervisor",     0.65,
  "HR", "Manager/ Senior Specialist", 0.60,
  "HR", "Director/ Head of",                0.55,
  "HR", "Leadership",          0.50,
  
  # Marketing
  "Marketing", "Entry Level/ Manual",        0.60,
  "Marketing", "Specialist/ Supervisor",     0.55,
  "Marketing", "Manager/ Senior Specialist", 0.50,
  "Marketing", "Director/ Head of",                0.45,
  "Marketing", "Leadership",          0.40,
  
  # Production
  "Production", "Entry Level/ Manual",        0.25,
  "Production", "Specialist/ Supervisor",     0.20,
  "Production", "Manager/ Senior Specialist", 0.15,
  "Production", "Director/ Head of",               0.10,
  "Production", "Leadership",           0.05,
  
  # R&D
  "R&D", "Entry Level/ Manual",        0.30,
  "R&D", "Specialist/ Supervisor",     0.25,
  "R&D", "Manager/ Senior Specialist", 0.20,
  "R&D", "Director/ Head of",                 0.15,
  "R&D", "Leadership",         0.10,
  
  # Sales
  "Sales", "Entry Level/ Manual",        0.40,
  "Sales", "Specialist/ Supervisor",     0.35,
  "Sales", "Manager/ Senior Specialist", 0.30,
  "Sales", "Director/ Head of",                 0.25,
  "Sales", "Leadership",           0.20
)



gender_probs_apply <- tribble(
  ~department,        ~job_level,                 ~p_woman,
  
  # Customer Service
  "Customer Service", "Entry Level/ Manual",        0.70,
  "Customer Service", "Specialist/ Supervisor",     0.65,
  "Customer Service", "Manager/ Senior Specialist", 0.60,
  "Customer Service", "Director/ Head of",               0.55,
  "Customer Service", "Leadership",           0.50,
  
  # Finance
  "Finance", "Entry Level/ Manual",        0.55,
  "Finance", "Specialist/ Supervisor",     0.50,
  "Finance", "Manager/ Senior Specialist", 0.45,
  "Finance",  "Director/ Head of",               0.40,
  "Finance", "Leadership",          0.35,
  
  # HR
  "HR", "Entry Level/ Manual",        0.75,
  "HR", "Specialist/ Supervisor",     0.70,
  "HR", "Manager/ Senior Specialist", 0.65,
  "HR", "Director/ Head of",                   0.60,
  "HR", "Leadership",       0.55,
  
  # Marketing
  "Marketing", "Entry Level/ Manual",        0.65,
  "Marketing", "Specialist/ Supervisor",     0.60,
  "Marketing", "Manager/ Senior Specialist", 0.55,
  "Marketing", "Director/ Head of",               0.50,
  "Marketing", "Leadership",           0.45,
  
  # Production
  "Production", "Entry Level/ Manual",        0.30,
  "Production", "Specialist/ Supervisor",     0.25,
  "Production", "Manager/ Senior Specialist", 0.20,
  "Production", "Director/ Head of",                  0.15,
  "Production", "Leadership",        0.10,
  
  # R&D
  "R&D", "Entry Level/ Manual",        0.35,
  "R&D", "Specialist/ Supervisor",     0.30,
  "R&D", "Manager/ Senior Specialist", 0.25,
  "R&D", "Director/ Head of",                0.20,
  "R&D", "Leadership",          0.15,
  
  # Sales
  "Sales", "Entry Level/ Manual",        0.45,
  "Sales", "Specialist/ Supervisor",     0.40,
  "Sales", "Manager/ Senior Specialist", 0.35,
  "Sales", "Director/ Head of",               0.30,
  "Sales", "Leadership",           0.25
)



temp_gender <- applicant_data %>% select(applicant_id, department, job_level, employee_id) %>% distinct() %>% arrange(applicant_id)
temp_gender_list <- temp_gender %>% mutate(temp_emp = str_detect(employee_id, "emp")) %>% group_split(temp_emp)
temp_gender_hired <- temp_gender_list[[2]]
temp_gender_applied <- temp_gender_list[[1]]
temp_gender_hired %<>% left_join(gender_probs_hire)
temp_gender_applied %<>% left_join(gender_probs_apply)
temp_gender_combined <- rbind(temp_gender_hired, temp_gender_applied) %>% select(-temp_emp)
temp_gender_combined %<>%
  mutate(
    gender = if_else(runif(n()) < p_woman, "Female", "Male"),
    gender = if_else(runif(n()) < 0.005, "Other", gender)
  ) %>%
  select(applicant_id, employee_id, gender)

#join gender and hr data
hrsystem_data %<>% left_join(temp_gender_combined) 

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")

#join gender and applicant data
applicant_data %<>% left_join(temp_gender_combined)

#write to file
write_csv(applicant_data, "hr_data/applicant_data.csv")


#assessment center dataset ----
assessment_center <- applicant_data %>% 
  filter(department == "Sales", job_level == "Entry Level/ Manual", stage == "Stage 4: Assessment Center") %>%
  rowwise() %>%
  mutate(
    assessment_center = case_when(
      str_detect(employee_id, "emp") ~ round(rnorm(1, mean = 4, sd = 0.25), 1),
      !str_detect(employee_id, "emp") ~ round(rnorm(1, mean = 3.5, sd = 0.25), 1)
      ),
    assessment_center = if_else(assessment_center > 5, 5, assessment_center),
    assessment_center = if_else(status == "hired" & assessment_center < 3.75, round(rnorm(1, mean = 3.85, sd = 0.1), 1),
                                if_else(!status == "hired" & assessment_center > 3.75, round(rnorm(1, mean = 3.65, sd = 0.1), 1), assessment_center))
    ) %>% 
  ungroup()

#write to file
write_csv(assessment_center, "hr_data/assessmentcenter_data.csv")

#create performance data for sales reps that has a slight correlation with assessment center scores
performance_assessment <- assessment_center %>% 
  mutate(
    performance_25 = case_when(
      str_detect(employee_id, "emp") ~ ntile(assessment_center, 5)
    )
  ) 

performance_assessment %<>% 
  filter(str_detect(employee_id, "emp")) %>% 
  select(employee_id, assessment_center, performance_25)

performance_assessment %<>% 
  mutate(
    performance_25_new = (performance_25 + rnorm(n(), mean = 3.5, sd = 2))/ 2,  # add Gaussian noise
    performance_25_new = (performance_25_new + rnorm(n(), mean = 3.5, sd = 2))/ 2,  # add Gaussian noise
    performance_25_new = pmin(pmax(performance_25_new, 2), 5),              # clamp to 2–5 range
    performance_25_new = round(performance_25_new),
    performance_25 = performance_25_new
      ) 

hist(performance_assessment$performance_25_new)
performance_assessment %>% count(performance_25_new)
cor(performance_assessment$assessment_center, performance_assessment$performance_25_new, use = "complete.obs")

hrsystem_data %<>% left_join(performance_assessment %>% select(employee_id, performance_25)) 
#create performance data for the rest of the company
hrsystem_data %<>%
  mutate(
    performance_25 = ifelse(is.na(performance_25), sample(c(2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5), length(is.na(performance_25)), replace = TRUE), performance_25)
  ) 

hrsystem_data %<>% 
  mutate(
    performance_25 = case_when(
      job_level != "Entry Level/ Manual" & department == "Sales" ~ sample(c(2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5), length(job_level != "Entry Level/ Manual" & department == "Sales"), replace = TRUE),
      TRUE ~ performance_25
    )
  ) 

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")


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
 


write_csv(feedback, "hr_data/feedback.csv")

#reduce size of datasets ----
applicant_data %<>% 
  filter(start_month >= as_date("2020-01-01"))
#write to file
write_csv(applicant_data, "hr_data/applicant_data.csv")

#to do list ----
#for next week
#check 360 data again to see what I tried to do there
#look at hiring data again to make sure the data reflects internal hiring (if not too complicated)
#psychometric data - for graduate programmes? (potentially a separate dataset that cant be linked to anything else, including effects for low resilience, high conscienciousness, low openness, high aggreeableness, medium extraversion)
#create employee survey (and maybe pulse survey?-probs too much)
#outcome data (sales, customer service, billed hours?)
#turnover by wellbeing score
#differences depending on performance
#leave out interview data and wait for recommendation from students in presentation
#reduce range for both datasets

#maybe later
#blue collar department (distribution)
#find a way to model contingency of starters following leavers for senior leaders
#qualitative comments?
#skewed normal distributions for e.g. tenure
#5% promotion rate
#2.5% salary increases
#if leaver, last event -> left


#done
#adverse impact against women 
#add department differences
#group differences for country and department (across all datasets)
#add gender data
#scores for assessment stage? / maybe just general fit score on assessment?
#performance data skew (range restriction for validity of assessments?)

#comments



#under construction ----


#old code ----


set.seed(187) #for recproducibility
hrsystem_data <- 
  tibble(
    employee_id = paste0("emp", 100001:130000), #generate random employee IDs
    country = sample(c("Germany", "Czechia", "France", "Italy",  "Netherlands"), size = 30000, replace = TRUE, prob = c(0.5, 0.2, 0.1, 0.1, 0.1)), #generate country data
  )

hrsystem_data %<>% #generate department data by country and job level by department
  mutate(
    department = case_when(
      country == "Germany" ~ sample(c("Production", "Sales", "Customer Service", "R&D", "Marketing", "Finance", "HR"), 
                                    size = length(country == "Germany"), replace = TRUE, 
                                    prob = c(0.40, 0.20, 0.13, 0.10, 0.03, 0.03, 0.02)),
      country == "Czechia" ~ sample(c("Production", "Sales", "Customer Service", "R&D", "Marketing", "Finance", "HR"), 
                                    size = length(country == "Germany"), replace = TRUE, 
                                    prob = c(0.64, 0.08, 0.05, 0.10, 0.01, 0.01, 0.01)),
      country %in% c("France", "Italy",  "Netherlands") ~ sample(c("Production", "Sales", "Customer Service", "R&D", "Marketing", "Finance", "HR"), 
                                                                 size = length(country == "Germany"), replace = TRUE, 
                                                                 prob = c(0.0, 0.40, 0.26, 0.12, 0.10, 0.01, 0.01))
    ),
    job_level = case_when(
      department == "Production" ~ sample(c("Entry Level/ Manual", "Specialist/ Supervisor", "Manager/ Senior Specialist", "Director/ Head of", "Leadership"), 
                                          size = length(department == "Production"), replace = TRUE, 
                                          prob = c(0.70, 0.20, 0.08, 0.009, 0.001)), 
      department != "Production" ~ sample(c("Entry Level/ Manual", "Specialist/ Supervisor", "Manager/ Senior Specialist", "Director/ Head of", "Leadership"), 
                                          size = length(department != "Production"), replace = TRUE, 
                                          prob = c(0.48, 0.325, 0.175, 0.019, 0.001)), 
    )
  ) 


hrsystem_data %>% count(department) %>% mutate(n_p = n/300)



hrsystem_data %>% #generate start dates
  mutate(
    typical_
    # start_date = case_when(
    #   department == "Production" ~ sample(c("Entry Level/ Manual", "Specialist/ Supervisor", "Manager/ Senior Specialist", "Director/ Head of", "Leadership"), 
    #                                       size = length(department == "Production"), replace = TRUE, 
    #                                       prob = c(0.70, 0.20, 0.08, 0.009, 0.001)), 
    #   department != "Production" ~ sample(c("Entry Level/ Manual", "Specialist/ Supervisor", "Manager/ Senior Specialist", "Director/ Head of", "Leadership"), 
    #                                       size = length(department != "Production"), replace = TRUE, 
    #                                       prob = c(0.48, 0.325, 0.175, 0.019, 0.001)), 
    # )
  ) %>% view()


start_date = sample(seq(as.Date("2015-01-01"), as.Date("2025-04-015"), by = "day"), size = 30000, replace = TRUE),
start_year = floor_date(start_date, "year"),
start_month = floor_date(start_date, "month")
) 

# #define job - level specific tenure in years
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
    leaving_date = case_when(
      == "Entry Level" ~ as_date(round(rnorm(1, mean = start_date + 730, sd = 200))),
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


#construction side 

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

