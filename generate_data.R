# This R script creates synthetic datasets for teaching in people analytics 
# The case study is a European manufacturing organization with ~10,000 employees

library(tidyverse)
library(magrittr)
library(corrplot)
library(effectsize)

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
    performance_25 = case_when(
      str_detect(employee_id, "emp") ~ ntile(assessmentcenter_score, 5)
    )
  ) 

performance_assessment %<>% 
  filter(str_detect(employee_id, "emp")) %>% 
  select(employee_id, assessmentcenter_score, performance_25)

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
cor(performance_assessment$assessmentcenter_score, performance_assessment$performance_25_new, use = "complete.obs")

hrsystem_data %<>% left_join(performance_assessment %>% select(employee_id, performance_25)) 

#personality data ----
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
    performance_25 = ifelse(is.na(performance_25), sample(c(2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5), length(is.na(performance_25)), replace = TRUE), performance_25)
  ) 

hrsystem_data %<>% 
  mutate(
    performance_25 = case_when(
      job_level != "Entry Level/ Manual" & department == "Sales" ~ sample(c(2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5), length(job_level != "Entry Level/ Manual" & department == "Sales"), replace = TRUE),
      TRUE ~ performance_25
    )
  ) 

hrsystem_data %<>% 
  mutate(
    performance_25 = if_else(employee_status == "Current", performance_25, NA_integer_)
  )

#write to file
write_csv(hrsystem_data, "hr_data/hrsystem_data.csv")



#360 degree feedback data ----
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
applicant_data %<>% 
  filter(start_month >= as_date("2020-01-01"))

#create insufficient interview data
applicant_data %<>% 
  mutate(
    question_1 = case_when(
      str_detect(stage, "Interview") ~ sample(c(NA_integer_,1:5), size = length(str_detect(stage, "Interview")), replace = TRUE, prob = c(0.99, 0.001, 0.0015, 0.003, 0.003, 0.0015))
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

response_rates <- hrsystem_data %>%
  filter(employee_status == "Current") %>%
  slice_sample(prop = 0.83)

survey <- response_rates %>%
  select(employee_id, department, job_level, country) %>%
  mutate(
    engagement = rnorm(nrow(response_rates), 5.6 , 0.9) %>% pmin(7), 
    job = (2 * engagement  + rnorm(nrow(response_rates), 5.6 , 0.9) %>% pmin(7))/3,
    manager = (2 * engagement + rnorm(nrow(response_rates), 5.6, 0.9)  %>% pmin(7))/3,
    team = (2 * engagement + rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/3,
    org_lead = (2 * engagement + rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/3
  )

#explore dataset
corm <- cor(survey[-1:-4])
corrplot.mixed(corm)

#create item specific scores
survey %<>%  
  mutate(
      `I am proud to work for this company.` = (engagement + engagement + 2 * rnorm(nrow(response_rates), 5.8 , 0.9)  %>% pmin(7))/2,                     #| Engagement              | 5.8          |
      `I would recommend this company as a great place to work.` = (engagement + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2, #| Engagement              | 5.6          |
      `I feel motivated to do my best every day.` = (engagement + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,                #| Engagement              | 5.4          |
      `I see myself still working here in two years.` = (engagement + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,            #| Engagement              | 5.5          |
      `My work gives me a sense of personal accomplishment.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,     #| Engagement              | 5.6          |
      `I trust the decisions made by senior leadership.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.1 , 0.9)  %>% pmin(7))/2,         #| Leadership & Trust      | 5.1          |
      `Leaders communicate a clear vision for the future.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.0 , 0.9)  %>% pmin(7))/2,       #| Leadership & Trust      | 5.0          |
      `I feel well-informed about what is going on.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,             #| Leadership & Trust      | 5.2          |
      `Senior leaders are visible and approachable.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 4.8 , 0.9)  %>% pmin(7))/2,             #| Leadership & Trust      | 4.8          |
      `Leadership lives the values of the company.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.0 , 0.9)  %>% pmin(7))/2,              #| Leadership & Trust      | 5.0          |
      `My manager treats me with respect.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 6.0 , 0.9)  %>% pmin(7))/2,                       #| Manager Effectiveness   | 6.0          |
      `My manager gives me regular and useful feedback.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2,         #| Manager Effectiveness   | 5.3          |
      `My manager supports my professional development.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,         #| Manager Effectiveness   | 5.4          |
      `My manager communicates clearly and effectively.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,         #| Manager Effectiveness   | 5.5          |
      `My manager motivates me to do my best work.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,              #| Manager Effectiveness   5.6          |
      `My team works well together.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.7 , 0.9)  %>% pmin(7))/2,                             #| Team Climate            | 5.7          |
      `I feel supported by my colleagues.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.8 , 0.9)  %>% pmin(7))/2,                       #| Team Climate            | 5.8          |
      `There is a strong sense of trust within my team.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,         #| Team Climate            | 5.5          |
      `People on my team help each other succeed.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,               #| Team Climate            | 5.6          |
      `I feel like I belong on my team.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.7 , 0.9)  %>% pmin(7))/2,                         #| Team Climate            | 5.7          |
      `I have the tools and resources I need.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,                   #| Enablement / Autonomy   | 5.6          |
      `I understand what is expected of me.` = (job + engagement + 2 * rnorm(nrow(response_rates), 6.0 , 0.9)  %>% pmin(7))/2,                     #| Enablement / Autonomy   | 6.0          |
      `I can make decisions that affect my work.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,                #| Enablement / Autonomy   | 5.5          |
      `I have the autonomy I need to be effective.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,              #| Enablement / Autonomy   | 5.6          |
      `I can be myself at work without fear.` = (manager + team + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,                    #| Enablement / Autonomy   | 5.4          |
      `I have access to learning and development opportunities.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2, #| Career & Development    | 5.3          |
      `I am satisfied with the career opportunities available.` = (job + engagement + 2 * rnorm(nrow(response_rates), 4.9 , 0.9)  %>% pmin(7))/2,  #| Career & Development    | 4.9          |
      `My development is a priority for my manager.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,             #| Career & Development    | 5.2          |
      `I am encouraged to develop new skills.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,                   #| Career & Development    | 5.4          |
      `I have a clear understanding of how to progress.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.0 , 0.9)  %>% pmin(7))/2,         #| Career & Development    | 5.0          |
      `I feel valued for the work I do.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,                         #| Recognition             | 5.4          |
      `I receive recognition when I do good work.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2,               #| Recognition             | 5.3          |
      `My contributions are acknowledged by my manager.` = (manager + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,         #| Recognition             | 5.5          |
      `I maintain a healthy work-life balance.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2,                  #| Wellbeing               | 5.3          |
      `My workload is manageable.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.1 , 0.9)  %>% pmin(7))/2,                               #| Wellbeing               | 5.1          |
      `The company cares about my wellbeing.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,                    #| Wellbeing               | 5.2          |
      `I feel comfortable taking time off.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.8 , 0.9)  %>% pmin(7))/2,                      #| Wellbeing               | 5.8          |
      `I can talk openly about stress or mental health.` = (manager + team + 2 * rnorm(nrow(response_rates), 5.0 , 0.9)  %>% pmin(7))/2,         #| Wellbeing               | 5.0          |
      `I feel treated fairly regardless of background.` = (manager + team + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,          #| D\&I / Belonging        | 5.6          |
      `Diverse perspectives are valued in my team.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,              #| D\&I / Belonging        | 5.5          |
      `I feel a sense of belonging.` = (team + engagement + 2 * rnorm(nrow(response_rates), 5.5 , 0.9)  %>% pmin(7))/2,                             #| D\&I / Belonging        | 5.5          |
      `The company fosters an inclusive environment.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,            #| D\&I / Belonging        | 5.4          |
      `The company adapts quickly to change.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.0 , 0.9)  %>% pmin(7))/2,                    #| Change & Agility        | 5.0          |
      `I’m comfortable with the pace of change.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,                 #| Change & Agility        | 5.2          |
      `Innovation is encouraged.` = (org_lead + manager + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2,                                #| Change & Agility        | 5.3          |
      `We can respond to future challenges.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.4 , 0.9)  %>% pmin(7))/2,                     #| Change & Agility        | 5.4          |
      `Communication across departments is effective.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 4.8 , 0.9)  %>% pmin(7))/2,           #| Communication           | 4.8          |
      `I know where to find needed information.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2,                 #| Communication           | 5.3          |
      `Feedback from employees is taken seriously.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 4.9 , 0.9)  %>% pmin(7))/2,              #| Communication           | 4.9          |
      `Internal communications are clear and timely.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,            #| Communication           | 5.2          |
      `I am fairly compensated.` = (job + engagement + 2 * rnorm(nrow(response_rates), 4.8 , 0.9)  %>% pmin(7))/2,                                 #| Compensation & Benefits | 4.8          |
      `The benefits meet my needs.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.2 , 0.9)  %>% pmin(7))/2,                              #| Compensation & Benefits | 5.2          |
      `My pay reflects my performance.` = (job + engagement + 2 * rnorm(nrow(response_rates), 4.6 , 0.9)  %>% pmin(7))/2,                          #| Compensation & Benefits | 4.6          |
      `People here act with integrity.` = (job + engagement + 2 * rnorm(nrow(response_rates), 5.6 , 0.9)  %>% pmin(7))/2,                          #| Ethics & Values         | 5.6          |
      `The company lives its values.` = (org_lead + engagement + 2 * rnorm(nrow(response_rates), 5.3 , 0.9)  %>% pmin(7))/2                            #| Ethics & Values         | 5.3          |
  )
      
corm <- cor(survey[-1:-4])
#round values at the end

#to do list ----
#benchmark data survey?
#outcome data (sales, customer service, billed hours?)
#turnover by wellbeing score
#differences depending on performance


#maybe later
#look at hiring data again to make sure the data reflects internal hiring (if not too complicated)

#find a way to model contingency of starters following leavers for senior leaders
#qualitative comments?
#skewed normal distributions for e.g. tenure
#5% promotion rate
#2.5% salary increases
#if leaver, last event -> left
#reduce range for both datasets
#(and maybe pulse survey?-probs too much)

#done
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

