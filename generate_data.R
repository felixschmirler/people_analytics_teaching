# This R script creates synthetic datasets for teaching in people analytics 
# The case study is a European manufacturing organization with ~10,000 employees

library(tidyverse)
library(magrittr)
library(corrplot)
library(effectsize)
library(httr)
library(jsonlite)

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
    employee_id = paste0("emp", 1:nrow(hrsystem_data) + 100000), #generate random employee IDs
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
  
set.seed(187)  
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

#write to file
write_csv(applicant_data, "hr_data/applicant_data.csv")

#add gender data to applicant and hr data ----

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
hrsystem_data %<>% left_join(temp_gender_combined %>% select(employee_id, gender)) 

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")

#join gender and applicant data
applicant_data %<>% left_join(temp_gender_combined)

#write to file
write_csv(applicant_data, "hr_data/applicant_data.csv")

#assessment center dataset ----
set.seed(187)
assessmentcenter_data <- applicant_data %>% 
  filter(department == "Sales", job_level == "Entry Level/ Manual", stage == "Stage 4: Assessment Center") %>%
  rowwise() %>%
  mutate(
    assessmentcenter_score = case_when(
      str_detect(employee_id, "emp") ~ round(rnorm(1, mean = 4, sd = 0.25), 1),
      !str_detect(employee_id, "emp") ~ round(rnorm(1, mean = 3.5, sd = 0.25), 1)
      ),
    assessmentcenter_score = if_else(assessmentcenter_score > 5, 5, assessmentcenter_score),
    assessmentcenter_score = if_else(status == "hired" & assessmentcenter_score < 3.75, round(rnorm(1, mean = 3.85, sd = 0.1), 1),
                                if_else(!status == "hired" & assessmentcenter_score > 3.75, round(rnorm(1, mean = 3.65, sd = 0.1), 1), assessmentcenter_score))
    ) %>% 
  ungroup()

#write to file
write_csv(assessmentcenter_data, "hr_data/assessmentcenter_data.csv")

#create performance data for sales reps that has a slight correlation with assessment center scores
performance_assessment <- assessmentcenter_data %>% 
  mutate(
    performance_24 = case_when(
      str_detect(employee_id, "emp") ~ ntile(assessmentcenter_score, 5)
    )
  ) 

performance_assessment %<>% 
  filter(str_detect(employee_id, "emp")) %>% 
  select(employee_id, assessmentcenter_score, performance_24)

performance_assessment %<>% 
  mutate(
    performance_24_new = (performance_24 + rnorm(n(), mean = 3.5, sd = 2))/ 2,  # add Gaussian noise
    performance_24_new = (performance_24_new + rnorm(n(), mean = 3.5, sd = 2))/ 2,  # add Gaussian noise
    performance_24_new = pmin(pmax(performance_24_new, 2), 5),              # clamp to 2–5 range
    performance_24_new = round(performance_24_new),
    performance_24 = performance_24_new
      ) 

hist(performance_assessment$performance_24_new)
performance_assessment %>% count(performance_24_new)
cor(performance_assessment$assessmentcenter_score, performance_assessment$performance_24_new, use = "complete.obs")

hrsystem_data %<>% left_join(performance_assessment %>% select(employee_id, performance_24)) 

#personality data ----
set.seed(187)
personality_hired <- tibble(
  Test_id = paste0("test", 1:nrow(assessmentcenter_data %>% filter(status == "hired")) + 100000), 
  Adjustment = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 68, sd = 10) %>% pmin(100),  # mittel
  Ambition = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 86, sd = 10) %>% pmin(100),       # hoch
  Sociability = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 73, sd = 10) %>% pmin(100),    # mittel
  Interpersonal_Sensitivity = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 80, sd = 10) %>% pmin(100), # hoch
  Prudence = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 85, sd = 10) %>% pmin(100),       # hoch
  Inquisitive = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 55, sd = 10) %>% pmin(100),    # mittel-niedrig
  Learning_Approach = rnorm(nrow(assessmentcenter_data %>% filter(status == "hired")), mean = 78, sd = 10) %>% pmin(100), # mittel-niedrig
  Status = "hired"
)

personality_rejected <- tibble(
  Test_id = paste0("test", 1:nrow(assessmentcenter_data %>% filter(status != "hired")) + 100000 + nrow(assessmentcenter_data %>% filter(status == "hired"))), 
  Adjustment = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 67, sd = 10) %>% pmin(100),  # mittel
  Ambition = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 78, sd = 10) %>% pmin(100),       # hoch
  Sociability = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 65, sd = 10) %>% pmin(100),    # mittel
  Interpersonal_Sensitivity = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 77, sd = 10) %>% pmin(100), # hoch
  Prudence = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 73, sd = 10) %>% pmin(100),       # hoch
  Inquisitive = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 57, sd = 10) %>% pmin(100),    # mittel-niedrig
  Learning_Approach = rnorm(nrow(assessmentcenter_data %>% filter(status != "hired")), mean = 77, sd = 10) %>% pmin(100), # mittel-niedrig
  Status = "rejected",
)

personality_data <- rbind(personality_hired, personality_rejected)
write_csv(personality_data, "hr_data/personality_data.csv") 


#create performance data for the rest of the company ----
hrsystem_data %<>%
  mutate(
    performance_24 = ifelse(is.na(performance_24), sample(c(2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5), length(is.na(performance_24)), replace = TRUE), performance_24)
  ) 

hrsystem_data %<>% 
  mutate(
    performance_24 = case_when(
      job_level != "Entry Level/ Manual" & department == "Sales" ~ sample(c(2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5), length(job_level != "Entry Level/ Manual" & department == "Sales"), replace = TRUE),
      TRUE ~ performance_24
    )
  ) 

hrsystem_data %<>% 
  mutate(
    performance_24 = if_else(employee_status == "Current", performance_24, NA_integer_)
  )

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")



#360 degree feedback data ----
set.seed(187)
leadership_roles <- hrsystem_data %>%
  filter(job_level %in% c("Manager/ Senior Specialist", "Director/ Head of") & employee_status == "Current") %>%
  slice_sample(prop = 0.6)


# | #  | Competency (Corporate Name)  | Academic Behavioural Focus                                                       |
# | -- | ---------------------------- | -------------------------------------------------------------------------------- |
# | 1  | **Inspire with Purpose**     | Visionary leadership; communicating meaningful goals (Podsakoff et al., 1990)    |
# | 2  | **Drive for Results**        | Goal orientation; performance focus (Locke & Latham, 2002)                       |
# | 3  | **Own the Outcome**          | Accountability; ownership mindset (Grant, 2008)                                  |
# | 4  | **Speak Up and Listen**      | Psychological safety; openness to voice (Edmondson, 1999)                        |
# | 5  | **Lead with Empathy**        | Emotional intelligence; supportive leadership (Kabat-Zinn, 2003)                 |
# | 6  | **Grow Others**              | Coaching and development; servant leadership (Greenleaf, 1977)                   |
# | 7  | **Bridge Across Boundaries** | Cross-functional collaboration; boundary-spanning leadership (Ernst & Yip, 2009) |
# | 8  | **Act with Integrity**       | Ethical leadership; fairness and consistency (Brown & Treviño, 2006)             |
# | 9  | **Make it Simple**           | Clarity, prioritisation, decision-making under complexity (Simon, 1947)          |
# | 10 | **Lead Change Fearlessly**   | Change orientation; resilience; transformation leadership (Kotter, 1996)         |
# | 11 | **Learn and Adapt Fast**     | Learning orientation; agility; growth mindset (Dweck, 2006)                      |
# | 12 | **Champion Diversity**       | Inclusive leadership; bias reduction; fairness (Nishii, 2013)                    |


#create common base variable and common source of variation from rating to simulate correlations
feedback <- leadership_roles %>% 
  select(employee_id) %>%
  mutate(
    general= rnorm(nrow(leadership_roles), 4 , 0.6) %>% pmin(5), 
    self = (general + rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) + rnorm(nrow(leadership_roles), 4.4 , 0.6) %>% pmin(5))/3, #assuming slightly lower accuracy for self ratings and therefore lower correlation with other ratings, more nuance to be added at a later stage
    manager = (general + rnorm(nrow(leadership_roles), 3.9 , 0.6)  %>% pmin(5))/2,
    peers = (general + rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5))/2,
    reports = (general + rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5))/2
  )

#self ratings round(rnorm(12, 4.4, 0.1), 2) 
#manager ratings round(rnorm(12, 3.9, 0.1), 2) 
#peer ratings round(rnorm(12, 4.1, 0.1), 2) 
#report ratings round(rnorm(12, 4.1, 0.1), 2) 


#create scale specific scores
feedback %<>%  
  mutate(
    `Self: Inspire with Purpose` = ((self + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6)  %>% pmin(5) )/3)  %>% round(0),       
    `Self: Drive for Results` =  ((self + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3)  %>% round(0),         
    `Self: Own the Outcome` = ((self + 2 * rnorm(nrow(leadership_roles), 4.4  , 0.6) %>% pmin(5) )/3)  %>% round(0),             
    `Self: Speak Up and Listen` = ((self + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6) %>% pmin(5) )/3)  %>% round(0),       
    `Self: Lead with Empathy` = ((self + 2 * rnorm(nrow(leadership_roles), 4.2  , 0.6)%>% pmin(5) )/3)  %>% round(0),         
    `Self: Grow Others` = ((self + 2 * rnorm(nrow(leadership_roles), 4.0   , 0.6) %>% pmin(5) )/3)  %>% round(0),             
    `Self: Bridge Across Boundaries` = ((self + 2 * rnorm(nrow(leadership_roles), 3.6  , 0.6) %>% pmin(5) )/3)  %>% round(0),   
    `Self: Act with Integrity` = ((self + 2 * rnorm(nrow(leadership_roles), 4.4  , 0.6) %>% pmin(5) )/3)  %>% round(0),      
    `Self: Make it Simple` = ((self + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6) %>% pmin(5) )/3)  %>% round(0),      
    `Self: Lead Change Fearlessly` = ((self + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6) %>% pmin(5) )/3)  %>% round(0),    
    `Self: Learn and Adapt Fast` = ((self + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6) %>% pmin(5) )/3)  %>% round(0),      
    `Self: Champion Diversity` = ((self + 2 * rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5) )/3)  %>% round(0),   
    
    `Manager: Inspire with Purpose` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6)  %>% pmin(5) )/3)  %>% round(0),  
    `Manager: Drive for Results` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.4, 0.6 )  %>% pmin(5) )/3)  %>% round(0),     
    `Manager: Own the Outcome` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3)  %>% round(0),       
    `Manager: Speak Up and Listen` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6 )  %>% pmin(5) )/3)  %>% round(0),   
    `Manager: Lead with Empathy` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5) )/3)  %>% round(0),     
    `Manager: Grow Others` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3)  %>% round(0),           
    `Manager: Bridge Across Boundaries` = ((manager + 2 * rnorm(nrow(leadership_roles), 3.6  , 0.6)  %>% pmin(5) )/3)  %>% round(0),  
    `Manager: Act with Integrity` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3)  %>% round(0),    
    `Manager: Make it Simple` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3)  %>% round(0),        
    `Manager: Lead Change Fearlessly` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.4  , 0.6)  %>% pmin(5) )/3)  %>% round(0),
    `Manager: Learn and Adapt Fast` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.4  , 0.6)  %>% pmin(5) )/3)  %>% round(0),  
    `Manager: Champion Diversity` = ((manager + 2 * rnorm(nrow(leadership_roles), 4.2, 0.6)  %>% pmin(5) )/3)  %>% round(0),    
    
    `Peers: Inspire with Purpose` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3) ,    
    `Peers: Drive for Results` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.4, 0.6 )  %>% pmin(5) )/3) ,       
    `Peers: Own the Outcome` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3) ,         
    `Peers: Speak Up and Listen` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6)  %>% pmin(5) )/3) ,     
    `Peers: Lead with Empathy` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.2  , 0.6)  %>% pmin(5) )/3) ,       
    `Peers: Grow Others` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6)  %>% pmin(5) )/3) ,             
    `Peers: Bridge Across Boundaries` = ((peers + 2 * rnorm(nrow(leadership_roles), 3.6  , 0.6) %>% pmin(5) )/3) ,
    `Peers: Act with Integrity` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3) ,      
    `Peers: Make it Simple` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3) ,          
    `Peers: Lead Change Fearlessly` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.4    , 0.6)  %>% pmin(5) )/3) ,  
    `Peers: Learn and Adapt Fast` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.4   , 0.6)  %>% pmin(5) )/3) ,    
    `Peers: Champion Diversity` = ((peers + 2 * rnorm(nrow(leadership_roles), 4.2, 0.6)  %>% pmin(5) )/3) ,  
    
    `Reports: Inspire with Purpose` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3) ,    
    `Reports: Drive for Results` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.4, 0.6  )  %>% pmin(5) )/3) ,       
    `Reports: Own the Outcome` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.4  , 0.6)  %>% pmin(5) )/3) ,         
    `Reports: Speak Up and Listen` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3) ,     
    `Reports: Lead with Empathy` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5) )/3) ,       
    `Reports: Grow Others` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.0 , 0.6)  %>% pmin(5) )/3) ,             
    `Reports: Bridge Across Boundaries` = ((reports + 2 * rnorm(nrow(leadership_roles), 3.6  , 0.6)  %>% pmin(5) )/3) ,
    `Reports: Act with Integrity` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.4 , 0.6)  %>% pmin(5) )/3) ,      
    `Reports: Make it Simple` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.0  , 0.6)  %>% pmin(5) )/3) ,          
    `Reports: Lead Change Fearlessly` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.4   , 0.6)  %>% pmin(5) )/3) ,  
    `Reports: Learn and Adapt Fast` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.4    , 0.6)  %>% pmin(5) )/3) ,    
    `Reports: Champion Diversity` = ((reports + 2 * rnorm(nrow(leadership_roles), 4.2 , 0.6)  %>% pmin(5) )/3) 
  )
 
t.test(feedback$`Self: Inspire with Purpose`, feedback$`Self: Drive for Results`)
cohens_d(feedback$`Self: Lead with Empathy`, feedback$`Self: Drive for Results`)


#explore generated data
feedback[-1] %>% colMeans()
corm <- cor(feedback[-1])
#corrplot.mixed(corm)
plots <- feedback %>%
  select(where(is.numeric)) %>%
  map(~ ggplot(data.frame(x = .x), aes(x = x)) +
        geom_histogram(bins = 30, fill = "darkred", color = "white") +
        labs(title = deparse(substitute(.x))) +
        theme_minimal())
#ggpairs(feedback[-1])

write_csv(feedback, "hr_data/feedback.csv")

#add-on application data ----
#reduce size of dataset 
set.seed(187)
applicant_data %<>% 
  filter(start_month >= as_date("2020-01-01"))

#create insufficient interview data
applicant_data %<>% 
  mutate(
    question_1 = case_when(
      str_detect(stage, "Interview") ~ sample(c(NA_integer_,1:5), size = length(str_detect(stage, "Interview")), replace = TRUE, prob = c(1.19, 0.001, 0.0015, 0.003, 0.003, 0.0015))
    ), 
    question_2 = case_when(
      !is.na(question_1) ~ sample(c(1:5), size = length(str_detect(stage, "Interview")), replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15))
    ),
    question_3 = case_when(
      !is.na(question_1) ~ sample(c(1:5), size = length(str_detect(stage, "Interview")), replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15))
    )
  ) 

#write to file
write_csv(applicant_data, "hr_data/applicant_data.csv")


#employee survey data ----
set.seed(187)
response_rates <- hrsystem_data %>%
  mutate(
    start_date = mdy(start_date), 
    leaving_date = mdy(leaving_date)
  ) %>%
  filter(start_date < as.Date("2024-04-01") & (leaving_date > as.Date("2024-04-01") | employee_status == "Current")) %>%
  slice_sample(prop = 0.83)

survey <- response_rates %>%
  select(employee_id, department, job_level, country) %>%
  mutate(
    engagement = rnorm(nrow(response_rates), 6.0 , 2) %>% pmin(7), 
    job = (engagement  + rnorm(nrow(response_rates), 6.0 , 2) %>% pmin(7))/2, 
    manager = (engagement + rnorm(nrow(response_rates), 6.0, 2)  %>% pmin(7))/2, 
    team = (engagement + rnorm(nrow(response_rates), 6.0 , 2)  %>% pmin(7))/2,
    org_lead = (engagement + rnorm(nrow(response_rates), 6.0 , 2)  %>% pmin(7))/2 
  )

#explore dataset
corm <- cor(survey[-1:-4])
#corrplot.mixed(corm)

#create item specific scores
survey %<>%  
  mutate(
      `I am proud to work for this company.` =  ( engagement + engagement  + rnorm(nrow(response_rates), 5.3 , 2)  %>% pmin(7))/3, #small - #| Engagement              | 5.5          |
      `I would recommend this company as a great place to work.` = ( engagement + engagement  + rnorm(nrow(response_rates), 5.4 , 2)  %>% pmin(7))/3, #small - #| Engagement              | 5.6          |
      `I feel motivated to do my best every day.` = ( engagement + engagement  + rnorm(nrow(response_rates), 5.6 , 2)  %>% pmin(7))/3,  #small +              #| Engagement              | 5.4          |
      `I see myself still working here in two years.` = ( engagement + engagement  + rnorm(nrow(response_rates), 4.1 , 2)  %>% pmin(7))/3,   #very large -          #| Engagement              | 5.5          |
      `My work gives me a sense of personal accomplishment.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 6.8 , 2)  %>% pmin(7))/4,  #very large +  #| Engagement              | 5.6          |
      `I trust the decisions made by senior leadership.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.1 , 2)  %>% pmin(7))/4, #very large        #| Leadership & Trust      | 5.1          |
      `Leaders communicate a clear vision for the future.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 5.0 , 2)  %>% pmin(7))/4,       #| Leadership & Trust      | 5.0          |
      `I feel well-informed about what is going on.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 5.0 , 2)  %>% pmin(7))/4,   #small          #| Leadership & Trust      | 5.2          |
      `Senior leaders are visible and approachable.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4 , 2)  %>% pmin(7))/4,   #large -         #| Leadership & Trust      | 4.8          |
      `Leadership lives the values of the company.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.8 , 2)  %>% pmin(7))/4,    #small          #| Leadership & Trust      | 5.0          |
      `My manager treats me with respect.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4,              #very large         #| Manager Effectiveness   | 6.0          |
      `My manager gives me regular and useful feedback.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 5.3 , 2)  %>% pmin(7))/4,         #| Manager Effectiveness   | 5.3          |
      `My manager supports my professional development.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 6.6 , 2)  %>% pmin(7))/4, #very large       #| Manager Effectiveness   | 5.4          |
      `My manager communicates clearly and effectively.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 5.3 , 2)  %>% pmin(7))/4, #small        #| Manager Effectiveness   | 5.5          |
      `My manager motivates me to do my best work.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 6.1 , 2)  %>% pmin(7))/4, #medium             #| Manager Effectiveness   5.6          |
      `My team works well together.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 6.9 , 2)  %>% pmin(7))/4,  #very large                           #| Team Climate            | 5.7          |
      `I feel supported by my colleagues.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4, #very large +                      #| Team Climate            | 5.8          |
      `There is a strong sense of trust within my team.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4, #very large +        #| Team Climate            | 5.5          |
      `People on my team help each other succeed.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4,  #large +             #| Team Climate            | 5.6          |
      `I feel like I belong on my team.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4, #very large +                          #| Team Climate            | 5.7          |
      `I have the tools and resources I need.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.1 , 2)  %>% pmin(7))/4, #medium +                 #| Enablement / Autonomy   | 5.6          |
      `I understand what is expected of me.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.5 , 2)  %>% pmin(7))/4,  #medium                   #| Enablement / Autonomy   | 6.0          |
      `I can make decisions that affect my work.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 4.3 , 2)  %>% pmin(7))/4, #large               #| Enablement / Autonomy   | 5.5          |
      `I have the autonomy I need to be effective.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 4.4 , 2)  %>% pmin(7))/4, #very large             #| Enablement / Autonomy   | 5.6          |
      `I can be myself at work without fear.` = ( engagement + manager + team + 3 * rnorm (nrow(response_rates), 5.4 , 2)  %>% pmin(7))/6,                    #| Enablement / Autonomy   | 5.4          |
      `I have access to learning and development opportunities.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.3 , 2)  %>% pmin(7))/4, #| Career & Development    | 5.3          |
      `I am satisfied with the career opportunities available.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.4 , 2)  %>% pmin(7))/4, # medium  #| Career & Development    | 4.9          |
      `My development is a priority for my manager.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 5.4 , 2)  %>% pmin(7))/4,  #small           #| Career & Development    | 5.2          |
      `I am encouraged to develop new skills.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 5.9 , 2)  %>% pmin(7))/4,  #medium                 #| Career & Development    | 5.4          |
      `I have a clear understanding of how to progress.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.5 , 2)  %>% pmin(7))/4, #medium       #| Career & Development    | 5.0          |
      `I feel valued for the work I do.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.2 , 2)  %>% pmin(7))/4,  #small -                  #| Recognition             | 5.4          |
      `I receive recognition when I do good work.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.8 , 2)  %>% pmin(7))/4, #medium +             #| Recognition             | 5.3          |
      `My contributions are acknowledged by my manager.` = ( engagement + manager  + 2 * rnorm (nrow(response_rates), 6.5 , 2)  %>% pmin(7))/4, #very large +        #| Recognition             | 5.5          |
      `I maintain a healthy work-life balance.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 3.5 , 2)  %>% pmin(7))/4,  #very large -                #| Wellbeing               | 5.3          |
      `My workload is manageable.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 3.2 , 2)  %>% pmin(7))/4,   #very large -                            #| Wellbeing               | 5.1          |
      `The company cares about my wellbeing.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.2 , 2)  %>% pmin(7))/4, #very large -                   #| Wellbeing               | 5.2          |
      `I feel comfortable taking time off.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5 , 2)  %>% pmin(7))/4, #large -                   #| Wellbeing               | 5.8          |
      `I can talk openly about stress or mental health.` = ( engagement + manager + team + 3 * rnorm (nrow(response_rates), 3.5 , 2)  %>% pmin(7))/6, #very large -        #| Wellbeing               | 5.0          |
      `I feel treated fairly regardless of background.` = ( engagement + manager + team + 3 * rnorm (nrow(response_rates), 6.4 , 2)  %>% pmin(7))/6, #medium +       #| D\&I / Belonging        | 5.6          |
      `Diverse perspectives are valued in my team.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 5.3 , 2)  %>% pmin(7))/4,  #small -            #| D\&I / Belonging        | 5.5          |
      `I feel a sense of belonging.` = ( engagement + team  + 2 * rnorm (nrow(response_rates), 6.5 , 2)  %>% pmin(7))/4,  #very large +                           #| D\&I / Belonging        | 5.5          |
      `The company fosters an inclusive environment.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 5.4 , 2)  %>% pmin(7))/4,            #| D\&I / Belonging        | 5.4          |
      `The company adapts quickly to change.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.2 , 2)  %>% pmin(7))/4,  #large -                  #| Change & Agility        | 5.0          |
      `I am comfortable with the pace of change.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.0 , 2)  %>% pmin(7))/4, #very large -                #| Change & Agility        | 5.2          |
      `Innovation is encouraged.` = ( engagement + org_lead + manager + 3 * rnorm (nrow(response_rates), 6.1 , 2)  %>% pmin(7))/6, #large +                               #| Change & Agility        | 5.3          |
      `We can respond to future challenges.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.4 , 2)  %>% pmin(7))/4,  #very large -                   #| Change & Agility        | 5.4          |
      `Communication across departments is effective.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 3.6 , 2)  %>% pmin(7))/4,  #very large-         #| Communication           | 4.8          |
      `I know where to find needed information.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 4.3 , 2)  %>% pmin(7))/4,  #very large -               #| Communication           | 5.3          |
      `Feedback from employees is taken seriously.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 3.5 , 2)  %>% pmin(7))/4, #very large -            #| Communication           | 4.9          |
      `Internal communications are clear and timely.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 4.4 , 2)  %>% pmin(7))/4, #large -           #| Communication           | 5.2          |
      `I am fairly compensated.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.0 , 2)  %>% pmin(7))/4, #small +                                #| Compensation & Benefits | 4.8          |
      `The benefits meet my needs.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 5.2 , 2)  %>% pmin(7))/4,                              #| Compensation & Benefits | 5.2          |
      `My pay reflects my performance.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 4.8 , 2)  %>% pmin(7))/4,  #small +                        #| Compensation & Benefits | 4.6          |
      `People here act with integrity.` = ( engagement + job  + 2 * rnorm (nrow(response_rates), 7 , 2)  %>% pmin(7))/4,  #very large +                        #| Ethics & Values         | 5.6          |
      `The company lives its values.` = ( engagement + org_lead  + 2 * rnorm (nrow(response_rates), 5.3 , 2)  %>% pmin(7))/4                            #| Ethics & Values         | 5.3          |
  )
      
corm <- cor(survey %>% select(where(is.numeric)), use = "pairwise.complete.obs") %>% round(2)

#add team specific score differences
survey %<>% 
  select(-engagement, -manager, -org_lead, -job, -team) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ case_when(
    department == "Sales" ~ .x - 0.2, # -0.3
    department == "HR" ~ .x + 0.3, # + 0.2
    department == "Production" ~ .x - 0.3, #- 0.4
    department == "Customer Service" ~ .x - 0.35, #- 0.45
    department == "Finance" ~ .x + 0.1, #- 0.5
    department == "Marketing" ~ .x + 0.2, #+0.1
    department == "R&D" ~ .x + 0.2, # +0.1
    TRUE ~ .x
  ))) %>%
  ungroup()


#add correlations with performance
survey %<>% left_join(hrsystem_data %>% select(employee_id, performance_24))

survey_wp <- survey %>% 
  filter(!is.na(performance_24))
  
survey_np <- survey %>% 
  filter(is.na(performance_24))

survey_wp %<>%  
  mutate(
    `I am proud to work for this company.`= (8.5 * `I am proud to work for this company.` + 1.5 * (performance_24 + 2)) / 10,
    `I would recommend this company as a great place to work.`= (8.5 * `I would recommend this company as a great place to work.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel motivated to do my best every day.`= (7 *  `I feel motivated to do my best every day.` + 3 * (performance_24 + 2)) / 10, #key predictor 
    `I see myself still working here in two years.`= (8 * `I see myself still working here in two years.` + 2 * (performance_24 + 2)) / 10,
    `My work gives me a sense of personal accomplishment.`= (7.5 * `My work gives me a sense of personal accomplishment.` + 2.5 * (performance_24 + 2)) / 10,
    `I trust the decisions made by senior leadership.`= (8.5 * `I trust the decisions made by senior leadership.` + 1.5 * (performance_24 + 2)) / 10,
    `Leaders communicate a clear vision for the future.`= (8.5 * `Leaders communicate a clear vision for the future.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel well-informed about what is going on.`= (8.5 * `I feel well-informed about what is going on.` + 1.5 * (performance_24 + 2)) / 10,
    `Senior leaders are visible and approachable.`= (8.5 * `Senior leaders are visible and approachable.` + 1.5 * (performance_24 + 2)) / 10,
    `Leadership lives the values of the company.`= (8.5 *  `Leadership lives the values of the company.` + 1.5 * (performance_24 + 2)) / 10,
    `My manager treats me with respect.`= (8.5 * `My manager treats me with respect.` + 1.5 * (performance_24 + 2)) / 10, 
    `My manager gives me regular and useful feedback.`= (7  * `My manager gives me regular and useful feedback.` + 3 * (performance_24 + 2)) / 10, #key predictor
    `My manager supports my professional development.`= (8.5 *`My manager supports my professional development.` + 1.5 * (performance_24 + 2)) / 10,
    `My manager communicates clearly and effectively.`= (8.5 *`My manager communicates clearly and effectively.` + 1.5 * (performance_24 + 2)) / 10,
    `My manager motivates me to do my best work.`= (8.5 *`My manager motivates me to do my best work.` + 1.5 * (performance_24 + 2)) / 10,
    `My team works well together.`= (8.5 *`My team works well together.` + 1.5 *  (performance_24 + 2)) / 10,
    `I feel supported by my colleagues.`= (8.5 *`I feel supported by my colleagues.` + 1.5 * (performance_24 + 2)) / 10,                    
    `There is a strong sense of trust within my team.`= (8.5 *`There is a strong sense of trust within my team.` + 1.5 * (performance_24 + 2)) / 10, 
    `People on my team help each other succeed.`= (8.5 * `People on my team help each other succeed.` + 1.5 * (performance_24 + 2)) / 10, 
    `I feel like I belong on my team.`= (8.5 *`I feel like I belong on my team.` + 1.5 * (performance_24 + 2)) / 10,
    `I have the tools and resources I need.`= (7 * `I have the tools and resources I need.` + 3 * (performance_24 + 2)) / 10, #key predictors
    `I understand what is expected of me.`= (7.5 * `I understand what is expected of me.` + 2.5 * (performance_24 + 2)) / 10, #key predictor
    `I can make decisions that affect my work.`= (7 * `I can make decisions that affect my work.` + 3 * (performance_24 + 2)) / 10, 
    `I have the autonomy I need to be effective.`= (7 * `I have the autonomy I need to be effective.` + 3 * (performance_24 + 2)) / 10, #key predictor
    `I can be myself at work without fear.`= (8.5 *`I can be myself at work without fear.` + 1.5 * (performance_24 + 2)) / 10,
    `I have access to learning and development opportunities.`= (8.5 *`I have access to learning and development opportunities.` + 1.5 * (performance_24 + 2)) / 10,
    `I am satisfied with the career opportunities available.`= (7.5 * `I am satisfied with the career opportunities available.` + 2.5 * (performance_24 + 2)) / 10,
    `My development is a priority for my manager.`= (8.5 *`My development is a priority for my manager.` + 1.5 * (performance_24 + 2)) / 10,
    `I am encouraged to develop new skills.`= (8.5 * `I am encouraged to develop new skills.` + 1.5 * (performance_24 + 2)) / 10,
    `I have a clear understanding of how to progress.`= (8.5 *`I have a clear understanding of how to progress.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel valued for the work I do.`= (7.5 * `I feel valued for the work I do.` + 2.5 * (performance_24 + 2)) / 10,
    `I receive recognition when I do good work.`= (7.5 * `I receive recognition when I do good work.` + 2.5 * (performance_24 + 2)) / 10,
    `My contributions are acknowledged by my manager.`= (7 * `My contributions are acknowledged by my manager.` + 3 * (performance_24 + 2)) / 10, 
    `I maintain a healthy work-life balance.`= (8.5 *`I maintain a healthy work-life balance.` + 1.5 *  (performance_24 + 2)) / 10,
    `My workload is manageable.`= (7  *  `My workload is manageable.` + 3 * (performance_24 + 2)) / 10,
    `The company cares about my wellbeing.`= (8.5 *`The company cares about my wellbeing.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel comfortable taking time off.`= (8.5 *`I feel comfortable taking time off.` + 1.5 * (performance_24 + 2)) / 10,
    `I can talk openly about stress or mental health.`= (8.5 *`I can talk openly about stress or mental health.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel treated fairly regardless of background.`= (8.5 *`I feel treated fairly regardless of background.` + 1.5 * (performance_24 + 2)) / 10,
    `Diverse perspectives are valued in my team.`= (8.5 *`Diverse perspectives are valued in my team.` + 1.5 * (performance_24 + 2)) / 10,
    `I feel a sense of belonging.`= (8.5 *`I feel a sense of belonging.` + 1.5 * (performance_24 + 2)) / 10,
    `The company fosters an inclusive environment.`= (8.5 *`The company fosters an inclusive environment.` + 1.5 * (performance_24 + 2)) / 10,
    `The company adapts quickly to change.`= (8.5 *`The company adapts quickly to change.` + 1.5 * (performance_24 + 2)) / 10,
    `I am comfortable with the pace of change.`= (7 * `I am comfortable with the pace of change.` + 3 * (performance_24 + 2)) / 10,
    `Innovation is encouraged.`= (8.5 *`Innovation is encouraged.` + 1.5 * (performance_24 + 2)) / 10,
    `We can respond to future challenges.`= (8.5 *`We can respond to future challenges.` + 1.5 * (performance_24 + 2)) / 10,
    `Communication across departments is effective.`= (8.5 *`Communication across departments is effective.` + 1.5 * (performance_24 + 2)) / 10,
    `I know where to find needed information.`= (7 * `I know where to find needed information.` + 3 * (performance_24 + 2)) / 10,
    `Feedback from employees is taken seriously.`= (8.5 *`Feedback from employees is taken seriously.` + 1.5 * (performance_24 + 2)) / 10,
    `Internal communications are clear and timely.`= (8.5 *`Internal communications are clear and timely.` + 1.5 * (performance_24 + 2)) / 10,
    `I am fairly compensated.`= (8.5 *`I am fairly compensated.` + 1.5 * (performance_24 + 2)) / 10,
    `The benefits meet my needs.`= (8.5 * `The benefits meet my needs.` + 1.5 * (performance_24 + 2)) / 10,
    `My pay reflects my performance.`= (7.5 * `My pay reflects my performance.` + 2.5 * (performance_24 + 2)) / 10,
    `People here act with integrity.`= (8.5 * `People here act with integrity.` + 1.5 *  (performance_24 + 2)) / 10,
    `The company lives its values.`= (8.5 * `The company lives its values.` + 1.5 * (performance_24 + 2)) / 10
  )

survey <- rbind(survey_np, survey_wp)

hist(survey$`My work gives me a sense of personal accomplishment.`)
cor(survey$`Senior leaders are visible and approachable.`, survey$performance_24, use = "pairwise.complete.obs")
cor(survey$`My manager gives me regular and useful feedback.`, survey$performance_24, use = "pairwise.complete.obs")

#add correlations with tenure
survey %<>% left_join(hrsystem_data %>% select(employee_id, tenure_years, leaving_date, employee_status))

survey_c <- survey %>% filter(employee_status == "Current") 
survey_l <- survey %>% filter(employee_status == "Leaver") 

survey_l %<>%
  mutate(
    `I am proud to work for this company.` = `I am proud to work for this company.` - 0.2,
    `I would recommend this company as a great place to work.`= `I would recommend this company as a great place to work.` - 0.2,
    `I feel motivated to do my best every day.`=  `I feel motivated to do my best every day.`- 0.2,
    `I see myself still working here in two years.`= `I see myself still working here in two years.` - 0.6,
    `My work gives me a sense of personal accomplishment.`= `My work gives me a sense of personal accomplishment.` - 0.2,
    `I trust the decisions made by senior leadership.`= `I trust the decisions made by senior leadership.` - 0.3,
    `Leaders communicate a clear vision for the future.`= `Leaders communicate a clear vision for the future.` - 0.2,
    `I feel well-informed about what is going on.`= `I feel well-informed about what is going on.` - 0.2,
    `Senior leaders are visible and approachable.`= `Senior leaders are visible and approachable.` - 0.2,
    `Leadership lives the values of the company.`=  `Leadership lives the values of the company.` - 0.2,
    `My manager treats me with respect.`= `My manager treats me with respect.` - 0.2, 
    `My manager gives me regular and useful feedback.`= `My manager gives me regular and useful feedback.` - 0.3,
    `My manager supports my professional development.`= `My manager supports my professional development.` - 0.2,
    `My manager communicates clearly and effectively.`= `My manager communicates clearly and effectively.` - 0.2,
    `My manager motivates me to do my best work.`= `My manager motivates me to do my best work.` - 0.2,
    `My team works well together.`= `My team works well together.` - 0.3,
    `I feel supported by my colleagues.`= `I feel supported by my colleagues.` - 0.3,                    
    `There is a strong sense of trust within my team.`= `There is a strong sense of trust within my team.` - 0.3, 
    `People on my team help each other succeed.`=  `People on my team help each other succeed.` - 0.3, 
    `I feel like I belong on my team.`= `I feel like I belong on my team.` - 0.3, 
    `I have the tools and resources I need.`= `I have the tools and resources I need.`- 0.2,
    `I understand what is expected of me.`= `I understand what is expected of me.`- 0.3,
    `I can make decisions that affect my work.`= `I can make decisions that affect my work.` - 0.4, 
    `I have the autonomy I need to be effective.`= `I have the autonomy I need to be effective.`- 0.3,
    `I can be myself at work without fear.`= `I can be myself at work without fear.` - 0.3,
    `I have access to learning and development opportunities.`= `I have access to learning and development opportunities.` - 0.3,
    `I am satisfied with the career opportunities available.`= `I am satisfied with the career opportunities available.` - 0.2,
    `My development is a priority for my manager.`= `My development is a priority for my manager.` - 0.2,
    `I am encouraged to develop new skills.`=  `I am encouraged to develop new skills.` - 0.2,
    `I have a clear understanding of how to progress.`= `I have a clear understanding of how to progress.` - 0.2,
    `I feel valued for the work I do.`= `I feel valued for the work I do.` - 0.4,
    `I receive recognition when I do good work.`= `I receive recognition when I do good work.` - 0,
    `My contributions are acknowledged by my manager.`= `My contributions are acknowledged by my manager.`- 0.2,
    `I maintain a healthy work-life balance.`= `I maintain a healthy work-life balance.` - 0.4,
    `My workload is manageable.`=  `My workload is manageable.` - 0.4,
    `The company cares about my wellbeing.`= `The company cares about my wellbeing.` - 0.4,
    `I feel comfortable taking time off.`= `I feel comfortable taking time off.` - 0.3,
    `I can talk openly about stress or mental health.`= `I can talk openly about stress or mental health.` - 0.3,
    `I feel treated fairly regardless of background.`= `I feel treated fairly regardless of background.` - 0.2,
    `Diverse perspectives are valued in my team.`= `Diverse perspectives are valued in my team.` - 0.3,
    `I feel a sense of belonging.`= `I feel a sense of belonging.` - 0.4,
    `The company fosters an inclusive environment.`= `The company fosters an inclusive environment.` - 0.2,
    `The company adapts quickly to change.`= `The company adapts quickly to change.` - 0.2,
    `I am comfortable with the pace of change.`= `I am comfortable with the pace of change.` - 0.4,
    `Innovation is encouraged.`= `Innovation is encouraged.` - 0.2,
    `We can respond to future challenges.`= `We can respond to future challenges.` - 0.2,
    `Communication across departments is effective.`= `Communication across departments is effective.` - 0.2,
    `I know where to find needed information.`= `I know where to find needed information.` - 0.2,
    `Feedback from employees is taken seriously.`= `Feedback from employees is taken seriously.` - 0.4,
    `Internal communications are clear and timely.`= `Internal communications are clear and timely.` - 0.2,
    `I am fairly compensated.`= `I am fairly compensated.` - 0.2,
    `The benefits meet my needs.`=  `The benefits meet my needs.` - 0.2,
    `My pay reflects my performance.`= `My pay reflects my performance.` - 0.2,
    `People here act with integrity.`=  `People here act with integrity.` - 0.2,
    `The company lives its values.`=  `The company lives its values.` - 0.2
  )

survey_c %<>%
  mutate(
    `I am proud to work for this company.` = `I am proud to work for this company.` + 0,
    `I would recommend this company as a great place to work.`= `I would recommend this company as a great place to work.` + 0,
    `I feel motivated to do my best every day.`=  `I feel motivated to do my best every day.`+ 0,
    `I see myself still working here in two years.`= `I see myself still working here in two years.` + 0.4,
    `My work gives me a sense of personal accomplishment.`= `My work gives me a sense of personal accomplishment.` + 0.3,
    `I trust the decisions made by senior leadership.`= `I trust the decisions made by senior leadership.` + 0.1,
    `Leaders communicate a clear vision for the future.`= `Leaders communicate a clear vision for the future.` + 0,
    `I feel well-informed about what is going on.`= `I feel well-informed about what is going on.` + 0,
    `Senior leaders are visible and approachable.`= `Senior leaders are visible and approachable.` + 0,
    `Leadership lives the values of the company.`=  `Leadership lives the values of the company.` + 0,
    `My manager treats me with respect.`= `My manager treats me with respect.` + 0.6, 
    `My manager gives me regular and useful feedback.`= `My manager gives me regular and useful feedback.` + 0.2,
    `My manager supports my professional development.`= `My manager supports my professional development.` + 0,
    `My manager communicates clearly and effectively.`= `My manager communicates clearly and effectively.` + 0,
    `My manager motivates me to do my best work.`= `My manager motivates me to do my best work.` + 0,
    `My team works well together.`= `My team works well together.` + 0.4,
    `I feel supported by my colleagues.`= `I feel supported by my colleagues.` + 0.1,                    
    `There is a strong sense of trust within my team.`= `There is a strong sense of trust within my team.` + 0.1, 
    `People on my team help each other succeed.`=  `People on my team help each other succeed.` + 0.1, 
    `I feel like I belong on my team.`= `I feel like I belong on my team.` + 0.4,
    `I have the tools and resources I need.`= `I have the tools and resources I need.`+ 0,
    `I understand what is expected of me.`= `I understand what is expected of me.`+ 0.3,
    `I can make decisions that affect my work.`= `I can make decisions that affect my work.` + 0, 
    `I have the autonomy I need to be effective.`= `I have the autonomy I need to be effective.`+ 0.1,
    `I can be myself at work without fear.`= `I can be myself at work without fear.` + 0.1,
    `I have access to learning and development opportunities.`= `I have access to learning and development opportunities.` + 0.1,
    `I am satisfied with the career opportunities available.`= `I am satisfied with the career opportunities available.` + 0 ,
    `My development is a priority for my manager.`= `My development is a priority for my manager.` + 0 ,
    `I am encouraged to develop new skills.`=  `I am encouraged to develop new skills.` + 0 ,
    `I have a clear understanding of how to progress.`= `I have a clear understanding of how to progress.` + 0 ,
    `I feel valued for the work I do.`= `I feel valued for the work I do.` + 0.2,
    `I receive recognition when I do good work.`= `I receive recognition when I do good work.` + 0.2,
    `My contributions are acknowledged by my manager.`= `My contributions are acknowledged by my manager.`+ 0.2,
    `I maintain a healthy work-life balance.`= `I maintain a healthy work-life balance.` + 0.2,
    `My workload is manageable.`=  `My workload is manageable.` + 0.2,
    `The company cares about my wellbeing.`= `The company cares about my wellbeing.` + 0.2,
    `I feel comfortable taking time off.`= `I feel comfortable taking time off.` + 0.1,
    `I can talk openly about stress or mental health.`= `I can talk openly about stress or mental health.` + 0.1,
    `I feel treated fairly regardless of background.`= `I feel treated fairly regardless of background.` + 0 ,
    `Diverse perspectives are valued in my team.`= `Diverse perspectives are valued in my team.` + 0.1,
    `I feel a sense of belonging.`= `I feel a sense of belonging.` + 0.2,
    `The company fosters an inclusive environment.`= `The company fosters an inclusive environment.` + 0 ,
    `The company adapts quickly to change.`= `The company adapts quickly to change.` + 0 ,
    `I am comfortable with the pace of change.`= `I am comfortable with the pace of change.` + 0.2,
    `Innovation is encouraged.`= `Innovation is encouraged.` + 0 ,
    `We can respond to future challenges.`= `We can respond to future challenges.` + 0 ,
    `Communication across departments is effective.`= `Communication across departments is effective.` + 0 ,
    `I know where to find needed information.`= `I know where to find needed information.` + 0 ,
    `Feedback from employees is taken seriously.`= `Feedback from employees is taken seriously.` + 0 ,
    `Internal communications are clear and timely.`= `Internal communications are clear and timely.` + 0 ,
    `I am fairly compensated.`= `I am fairly compensated.` + 0,
    `The benefits meet my needs.`=  `The benefits meet my needs.` + 0,
    `My pay reflects my performance.`= `My pay reflects my performance.` + 0,
    `People here act with integrity.`=  `People here act with integrity.` + 0.3,
    `The company lives its values.`=  `The company lives its values.` + 0
  )

survey <- rbind(survey_c, survey_l) 

#add benchmark data
survey %<>%  
  mutate(
    `Benchmark: I am proud to work for this company.` =  5.8,#          |
    `Benchmark: I would recommend this company as a great place to work.` = 5.6,#          |
    `Benchmark: I feel motivated to do my best every day.` = 5.4,#          |
    `Benchmark: I see myself still working here in two years.` = 5.5,#          |
    `Benchmark: My work gives me a sense of personal accomplishment.` = 5.6,#          |
    `Benchmark: I trust the decisions made by senior leadership.` = 5.1,#          |
    `Benchmark: Leaders communicate a clear vision for the future.` = 5.0,#          |
    `Benchmark: I feel well-informed about what is going on.` = 5.2,#          |
    `Benchmark: Senior leaders are visible and approachable.` = 4.8,#          |
    `Benchmark: Leadership lives the values of the company.` =   5.0,#          |
    `Benchmark: My manager treats me with respect.` =  6.0,#          |
    `Benchmark: My manager gives me regular and useful feedback.` = 5.3,#          |
    `Benchmark: My manager supports my professional development.` = 5.4,#          |
    `Benchmark: My manager communicates clearly and effectively.` = 5.5,#          |
    `Benchmark: My manager motivates me to do my best work.` = 5.6,#          |
    `Benchmark: My team works well together.` =  5.7,#          |
    `Benchmark: I feel supported by my colleagues.` = 5.8,#          |
    `Benchmark: There is a strong sense of trust within my team.` =  5.5,#          |
    `Benchmark: People on my team help each other succeed.` = 5.6,#          |
    `Benchmark: I feel like I belong on my team.` =  5.7,#          |
    `Benchmark: I have the tools and resources I need.` = 5.6,#          |
    `Benchmark: I understand what is expected of me.` = 6.0,#          |
    `Benchmark: I can make decisions that affect my work.` = 5.5,#          |
    `Benchmark: I have the autonomy I need to be effective.` = 5.6,#          |
    `Benchmark: I can be myself at work without fear.` = 5.4,#          |
    `Benchmark: I have access to learning and development opportunities.` = 5.3,#          |
    `Benchmark: I am satisfied with the career opportunities available.` = 4.9,#          |
    `Benchmark: My development is a priority for my manager.` =  5.2,#          |
    `Benchmark: I am encouraged to develop new skills.` = 5.4,#          |
    `Benchmark: I have a clear understanding of how to progress.` = 5.0,#          |
    `Benchmark: I feel valued for the work I do.` =  5.4,#          |
    `Benchmark: I receive recognition when I do good work.` = 5.3,#          |
    `Benchmark: My contributions are acknowledged by my manager.` =  5.5,#          |
    `Benchmark: I maintain a healthy work-life balance.` = 5.3,#          |
    `Benchmark: My workload is manageable.` = 5.1,#          |
    `Benchmark: The company cares about my wellbeing.` =  5.2,#          |
    `Benchmark: I feel comfortable taking time off.` = 5.8,#          |
    `Benchmark: I can talk openly about stress or mental health.` = 5.0,#          |
    `Benchmark: I feel treated fairly regardless of background.` = 5.6,#          |
    `Benchmark: Diverse perspectives are valued in my team.` = 5.5,#          |
    `Benchmark: I feel a sense of belonging.` = 5.5, #          |
    `Benchmark: The company fosters an inclusive environment.` = 5.4,  #        |
    `Benchmark: The company adapts quickly to change.` = 5.0, #          |
    `Benchmark: I am comfortable with the pace of change.` = 5.2,#          |
    `Benchmark: Innovation is encouraged.` = 5.3, #          |
    `Benchmark: We can respond to future challenges.` = 5.4, #          |
    `Benchmark: Communication across departments is effective.` = 4.8, #          |
    `Benchmark: I know where to find needed information.` = 5.3, #          |
    `Benchmark: Feedback from employees is taken seriously.` = 4.9, #          |
    `Benchmark: Internal communications are clear and timely.` = 5.2, #          |
    `Benchmark: I am fairly compensated.` =  4.8, #          |
    `Benchmark: The benefits meet my needs.` = 5.2, #          |
    `Benchmark: My pay reflects my performance.` = 4.6, #         |
    `Benchmark: People here act with integrity.` = 5.6, #          |
    `Benchmark: The company lives its values.` = 5.3 #          |
  )

#explore if dataset works like expected
survey %>% 
  group_by(employee_status) %>%
  summarise(
    av = mean(`I see myself still working here in two years.`)
  )

#round values at the end and make sure no plus 7 or minus & drop variables
survey %<>%
  mutate(
    `I am proud to work for this company.` = `I am proud to work for this company.`  %>% pmin(7) %>% pmax(1) %>% round(),
    `I would recommend this company as a great place to work.`= `I would recommend this company as a great place to work.`  %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel motivated to do my best every day.`=  `I feel motivated to do my best every day.`%>% pmin(7) %>% pmax(1) %>% round(),
    `I see myself still working here in two years.`= `I see myself still working here in two years.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My work gives me a sense of personal accomplishment.`= `My work gives me a sense of personal accomplishment.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I trust the decisions made by senior leadership.`= `I trust the decisions made by senior leadership.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Leaders communicate a clear vision for the future.`= `Leaders communicate a clear vision for the future.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel well-informed about what is going on.`= `I feel well-informed about what is going on.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Senior leaders are visible and approachable.`= `Senior leaders are visible and approachable.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Leadership lives the values of the company.`=  `Leadership lives the values of the company.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My manager treats me with respect.`= `My manager treats me with respect.` %>% pmin(7) %>% pmax(1) %>% round(), 
    `My manager gives me regular and useful feedback.`= `My manager gives me regular and useful feedback.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My manager supports my professional development.`= `My manager supports my professional development.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My manager communicates clearly and effectively.`= `My manager communicates clearly and effectively.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My manager motivates me to do my best work.`= `My manager motivates me to do my best work.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My team works well together.`= `My team works well together.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel supported by my colleagues.`= `I feel supported by my colleagues.` %>% pmin(7) %>% pmax(1) %>% round(),                    
    `There is a strong sense of trust within my team.`= `There is a strong sense of trust within my team.` %>% pmin(7) %>% pmax(1) %>% round(), 
    `People on my team help each other succeed.`=  `People on my team help each other succeed.` %>% pmin(7) %>% pmax(1) %>% round(), 
    `I feel like I belong on my team.`= `I feel like I belong on my team.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I have the tools and resources I need.`= `I have the tools and resources I need.`%>% pmin(7) %>% pmax(1) %>% round(),
    `I understand what is expected of me.`= `I understand what is expected of me.`%>% pmin(7) %>% pmax(1) %>% round(),
    `I can make decisions that affect my work.`= `I can make decisions that affect my work.` %>% pmin(7) %>% pmax(1) %>% round(), 
    `I have the autonomy I need to be effective.`= `I have the autonomy I need to be effective.`%>% pmin(7) %>% pmax(1) %>% round(),
    `I can be myself at work without fear.`= `I can be myself at work without fear.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I have access to learning and development opportunities.`= `I have access to learning and development opportunities.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I am satisfied with the career opportunities available.`= `I am satisfied with the career opportunities available.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My development is a priority for my manager.`= `My development is a priority for my manager.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I am encouraged to develop new skills.`=  `I am encouraged to develop new skills.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I have a clear understanding of how to progress.`= `I have a clear understanding of how to progress.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel valued for the work I do.`= `I feel valued for the work I do.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I receive recognition when I do good work.`= `I receive recognition when I do good work.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My contributions are acknowledged by my manager.`= `My contributions are acknowledged by my manager.`%>% pmin(7) %>% pmax(1) %>% round(),
    `I maintain a healthy work-life balance.`= `I maintain a healthy work-life balance.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My workload is manageable.`=  `My workload is manageable.` %>% pmin(7) %>% pmax(1) %>% round(),
    `The company cares about my wellbeing.`= `The company cares about my wellbeing.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel comfortable taking time off.`= `I feel comfortable taking time off.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I can talk openly about stress or mental health.`= `I can talk openly about stress or mental health.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel treated fairly regardless of background.`= `I feel treated fairly regardless of background.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Diverse perspectives are valued in my team.`= `Diverse perspectives are valued in my team.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I feel a sense of belonging.`= `I feel a sense of belonging.` %>% pmin(7) %>% pmax(1) %>% round(),
    `The company fosters an inclusive environment.`= `The company fosters an inclusive environment.` %>% pmin(7) %>% pmax(1) %>% round(),
    `The company adapts quickly to change.`= `The company adapts quickly to change.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I am comfortable with the pace of change.`= `I am comfortable with the pace of change.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Innovation is encouraged.`= `Innovation is encouraged.` %>% pmin(7) %>% pmax(1) %>% round(),
    `We can respond to future challenges.`= `We can respond to future challenges.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Communication across departments is effective.`= `Communication across departments is effective.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I know where to find needed information.`= `I know where to find needed information.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Feedback from employees is taken seriously.`= `Feedback from employees is taken seriously.` %>% pmin(7) %>% pmax(1) %>% round(),
    `Internal communications are clear and timely.`= `Internal communications are clear and timely.` %>% pmin(7) %>% pmax(1) %>% round(),
    `I am fairly compensated.`= `I am fairly compensated.` %>% pmin(7) %>% pmax(1) %>% round(),
    `The benefits meet my needs.`=  `The benefits meet my needs.` %>% pmin(7) %>% pmax(1) %>% round(),
    `My pay reflects my performance.`= `My pay reflects my performance.` %>% pmin(7) %>% pmax(1) %>% round(),
    `People here act with integrity.`=  `People here act with integrity.` %>% pmin(7) %>% pmax(1) %>% round(),
    `The company lives its values.`=  `The company lives its values.` %>% pmin(7) %>% pmax(1) %>% round()
  )

survey %<>% 
  select(-employee_id, `I am proud to work for this company.`) %>%
  rowwise() %>% 
  mutate(
    survey_id = paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = "")
  ) %>%
  ungroup()

employeesurvey_data <- survey

#write to file
write.csv(employeesurvey_data, "hr_data/employeesurvey_data.csv", row.names = FALSE, fileEncoding = "UTF-8")


#generate open text responses ----
n_responses <- (0.5 * nrow(employeesurvey_data)) %>% round()
api_key <- readLines("api_key.txt")


#defining a function to send a prompt to openAI api
generate_response <- function(prompt, model = "gpt-3.5-turbo", temperature = 0.7) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(
        list(role = "system", content = "You are a helpful assistant generating realistic employee survey responses."),
        list(role = "user", content = prompt)
      ),
      temperature = temperature
    )
  )
  
  content <- content(response, as = "parsed", type = "application/json")
  
  # Extract response text
  content$choices[[1]]$message$content
}

# Example: generate based on a vector of fake Likert scores or themes
prompts <- rep("Please write a short employee comment about workload and time pressure.", 5000)

results <- character(length(prompts))

for (i in seq_along(prompts)) {
  cat("Generating response", i, "of", length(prompts), "\n")
  try({
    results[i] <- generate_response(prompts[i])
    Sys.sleep(1.2)  # Delay to stay within rate limits (60 requests/min)
  }, silent = TRUE)
}

output <- tibble::tibble(prompt = prompts, response = results)
write.csv(output, "synthetic_feedback.csv", row.names = FALSE)

# Make prompts more dynamic
prompts <- paste0("Write a realistic employee comment about ", 
                  sample(c("career development", "teamwork", "manager support", 
                           "wellbeing", "recognition"), 5000, replace = TRUE), 
                  ". The employee feels neutral to slightly positive.")


#do list ----
#innovation outcomes/ wellbeing outcomes (by manager?)
#team scores by manager?
#qualitative comments



#maybe later
#look at hiring data again to make sure the data reflects internal hiring (if not too complicated)
#unterschiedlicher impact von anderen variablen auf performance in sales, da performance anders bewertet wird dort.
#find a way to model contingency of starters following leavers for senior leaders
#qualitative comments?
#skewed normal distributions for e.g. tenure
#5% promotion rate
#2.5% salary increases
#if leaver, last event -> left
#reduce range for both datasets
#(and maybe pulse survey?-probs too much)
#scores lower than 4
#Korrelationen zwischen thematisch ähnlichen Items
#increase differences between ratings sources in feedback data? or not to make it not more complicated
#correct effect sizes for manager ratings, sds are too small

#done
#correlation with tenure by relevant domain.
#should I make the negative differences somewhat less pronounced to help the students find overall differences better?
#decide what should be low, high and missing to match content (autonomy and stress for sure)
#adverse impact against women 
#add department differences
#group differences for country and department (across all datasets)
#add gender data
#scores for assessment stage? / maybe just general fit score on assessment?
#performance data skew (range restriction for validity of assessments?)
#blue collar department (distribution)
#check 360 data again to see what I tried to do there
#leave out interview data and wait for recommendation from students in presentation
#psychometric data - for graduate programmes? (potentially a separate dataset that cant be linked to anything else, including effects for low resilience, high conscienciousness, low openness, high aggreeableness, medium extraversion)
#create employee survey 
#solve problem with 360 data
#25 performance data only for current employees
#benchmark data survey?
#add correlation with performance

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

