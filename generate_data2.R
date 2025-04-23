# This R script creates synthetic datasets for teaching in people analytics 
# The case study is a European manufacturing organization with ~10,000 employees

library(tidyverse)
library(magrittr)



#Core HR System Dataset
set.seed(187)
#define job - level specific tenurein years
job_level_tenure_targets <- c(
  "Entry" = 1.5,
  "Specialist" = 2,
  "Manager/Senior Specialist" = 3,
  "Director/Head of" = 4,
  "Executive" = 7
) #add department specific and country specific tenure later


#define function to solve the problem with current employee tenure
generate_start_date <- function(level) {
  today <- Sys.Date()
  years_back <- switch(level,
                       "Entry" = 3,
                       "Specialist" = 5,
                       "Manager/Senior Specialist" = 7,
                       "Director/Head of" = 9,
                       "Executive" = 12
  )
  sample(seq(today - years(years_back), today, by = "day"), 1)
}

hrsystem_data <- 
  tibble(
    employee_id = paste0("emp", 100001:120000), #generate random employee IDs
    job_level = sample(names(job_level_tenure_targets), 20000, replace = TRUE, prob = c(0.50, 0.325, 0.175, 0.019, 0.001)), #generate job level data
    country = sample(c("Germany", "France", "Italy", "Spain", "Netherlands"), size = 20000, replace = TRUE, prob = c(0.5, 0.15, 0.1, 0.1, 0.15)), #generate country data
    department = sample(c("Production", "Sales", "Customer Service", "R&D", "Marketing", "Finance", "HR"), size = 20000, replace = TRUE, prob = c(0.40, 0.15, 0.10, 0.10, 0.05, 0.04, 0.02)), #generate department data
    
    start_date = sample(seq(as.Date("2015-01-01"), Sys.Date(), by = "day"), size = 20000, replace = TRUE)
 ) %>% 
  arrange(start_date) %>%
  mutate(
    employee_status = c(
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.05, 0.95)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.15, 0.85)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.25, 0.75)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.35, 0.65)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.45, 0.55)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.55, 0.45)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.65, 0.35)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.75, 0.25)),
           sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.85, 0.15)),
           sample(c("current", "leaver"), size = 1000, replace = TRUE, prob = c(0.95, 0.05)),
           sample(c("current", "leaver"), size = 1000, replace = TRUE, prob = c(0.99, 0.01))
         ), #assign leaver status with higher probabilities for 
    turnover = if_else(employee_status == "leaver", sample(c("involuntary", "voluntary"), size = 1, prob = c(0.2, 0.8)), NA),
    update_status = "active",
  )

hrsystem_data %<>% 
  rowwise() %>%
  mutate(
    start_date = case_when(
      employee_status == "current" & job_level == "Entry" ~ sample(seq(Sys.Date() - years(3), Sys.Date(), by = "day"), 1),
      employee_status == "current" & job_level == "Specialist" ~ sample(seq(Sys.Date() - years(5), Sys.Date(), by = "day"), 1),
      employee_status == "current" & job_level == "Manager/Senior Specialist" ~ sample(seq(Sys.Date() - years(6), Sys.Date(), by = "day"), 1),
      employee_status == "current" & job_level == "Director/Head of" ~ sample(seq(Sys.Date() - years(8), Sys.Date(), by = "day"), 1),
      employee_status == "current" & job_level == "Executive" ~ sample(seq(Sys.Date() - years(10), Sys.Date(), by = "day"), 1),
      TRUE ~ start_date  # keep existing value for Leavers
    ),
    start_year = floor_date(start_date, "year"),
    start_month = floor_date(start_date, "month")
  ) %>%
  mutate(
    target_tenure_days = round(job_level_tenure_targets[[job_level]] * 365),
    sampled_tenure = if_else(
      employee_status == "leaver",
      round(rnorm(1, mean = target_tenure_days, sd = 180)),
      NA_integer_
    ),
    leaving_date = if_else(
      employee_status == "leaver",
      start_date + days(sampled_tenure),
      NA_Date_
    ),
    leaving_year = floor_date(leaving_date, "year"),
    leaving_month = floor_date(leaving_date, "month")
  ) %>%
  ungroup() %>%
  mutate(
    current_tenure_date = if_else(employee_status == "Leaver", leaving_date, Sys.Date()),
    tenure = interval(start_date, current_tenure_date),
    tenure_years = round(time_length(tenure, "years"), 2)
  ) %>%
  select(-target_tenure_days, -sampled_tenure)

  
  hrsystem_data %>% count(employee_status, job_level)
    
  hrsystem_data %>% group_by(job_level) %>% 
    summarise(av_ten = mean(tenure_years)) #%>%
  ggplot(aes(tenure, fill = employee_status)) +
    geom_histogram(bins = 500)  
    
  #arrange(start_date) %>% 
  # mutate(
  #   status = c(
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.05, 0.95)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.15, 0.85)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.25, 0.75)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.35, 0.65)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.45, 0.55)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.55, 0.45)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.65, 0.35)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.75, 0.25)),
  #     sample(c("current", "leaver"), size = 2000, replace = TRUE, prob = c(0.85, 0.15)),
  #     sample(c("current", "leaver"), size = 1000, replace = TRUE, prob = c(0.95, 0.05)),
  #     sample(c("current", "leaver"), size = 1000, replace = TRUE, prob = c(0.99, 0.01))
  #   )
  #   ) %>% 
  # rowwise() %>%
  # mutate(
  #   turnover = if_else(
  #     status == "leaver",
  #     sample(c("involuntary", "voluntary"), size = 1, prob = c(0.2, 0.8)),
  #     NA
  #   ),
  #   leaving_date = if_else(
  #     status == "leaver",
  #     sample(seq(start_date, Sys.Date(), by = "day"), 1),
  #     NA
  #   )
  # ) %>% 
  # ungroup() %>% 
  # mutate(
  #   tenure = if_else(status == "leaver", as.period(interval(start_date, as.Date(leaving_date))), as.period(interval(start_date, Sys.Date()))),
  #   tenure_years = time_length(tenure, "years"),
  #   tenure_months = time_length(tenure, "months"),
  #   helper_update = "current status"
  # ) %>%
  # arrange(tenure)
  # 
  # 





#%>% view()
    #count(start_month, status) %>% 
    #ggplot(aes(start_month, n, fill = status)) +
    #geom_col()
        

# ----------- Helper Functions -----------
generate_ids <- function(n, prefix="emp") {
  sprintf("%s%05d", prefix, seq(1, n))
}

random_dates <- function(n, start="2015-01-01", end="2024-01-01") {
  as.Date(runif(n, as.numeric(as.Date(start)), as.numeric(as.Date(end))), origin = "1970-01-01")
}

random_sample <- function(vec, size, replace = TRUE) {
  sample(vec, size = size, replace = replace)
}

# ----------- Dataset 0: Employee Master Data -----------
num_employees <- 10000

countries <- c("Germany", "France", "Italy", "Spain", "Netherlands")
weights <- c(0.5, 0.15, 0.1, 0.1, 0.15)
departments <- c("Finance", "Marketing", "R&D", "HR", "Sales", "Customer Service", "Production")
levels <- c("Entry", "Specialist", "Manager", "Senior Manager", "Executive")
level_probs <- c(0.4, 0.3, 0.2, 0.08, 0.02)

employees <- tibble(
  EmployeeID = generate_ids(num_employees),
  Country = sample(countries, num_employees, replace = TRUE, prob = weights),
  Department = sample(departments, num_employees, replace = TRUE),
  JobLevel = sample(levels, num_employees, replace = TRUE, prob = level_probs),
  Gender = sample(c("Male", "Female", "Other"), num_employees, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
  Ethnicity = sample(c("White", "Black", "Asian", "Mixed", "Other"), num_employees, replace = TRUE, prob = c(0.6, 0.1, 0.15, 0.1, 0.05)),
  StartDate = random_dates(num_employees, start="2000-01-01", end="2024-01-01")
)

# Former employees
former_employees <- employees %>%
  slice_sample(n = 10000) %>%
  mutate(
    EmployeeID = generate_ids(10000, prefix = "ex"),
    LeavingDate = random_dates(10000, start="2018-01-01", end="2023-12-31")
  )

# ----------- Dataset 1: Assessment Data (Recruitment) -----------

roles <- c("Entry", "Specialist", "Manager", "Senior Manager", "Executive")
role_applicants <- c(50000, 10000, 3000, 800, 200)

assessment_data <- map2_dfr(roles, role_applicants, function(role, n) {
  tibble(
    ApplicantID = generate_ids(n, prefix = paste0("app_", substr(role, 1, 2))),
    RoleApplied = role,
    Stage_CV = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    Stage_PhoneInterview = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
    Stage_HiringManager = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
    Stage_Additional = sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2)),
    InterviewType = sample(c("Structured", "Unstructured"), n, replace = TRUE, prob = c(0.7, 0.3)),
    CognitiveAbility = rnorm(n, mean = 100, sd = 15),
    PersonalityFit = runif(n, min = 1, max = 5),
    AssessmentCentreScore = runif(n, min = 1, max = 5),
    UsedAI = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.1, 0.9)),
    CheatingDetected = rbinom(n, 1, prob = 0.02),
    Gender = sample(c("Male", "Female"), n, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Asian", "Other"), n, replace = TRUE, prob = c(0.6, 0.1, 0.2, 0.1))
  )
})

# ----------- Dataset 2: 360 Feedback Data -----------
leadership_roles <- employees %>%
  filter(JobLevel %in% c("Manager", "Senior Manager", "Executive")) %>%
  slice_sample(prop = 0.8)

competencies <- c("Vision", "Inspiration", "Execution", "Integrity", "AnalyticalSkill", "Communication")

generate_feedback_scores <- function(n) {
  rnorm(n, mean = 4, sd = 0.5) %>% pmin(5) %>% pmax(1)
}

feedback_data <- leadership_roles %>%
  rowwise() %>%
  mutate(
    Self = list(setNames(generate_feedback_scores(length(competencies)), competencies)),
    Manager = list(setNames(generate_feedback_scores(length(competencies)), competencies)),
    Peers = list(setNames(colMeans(replicate(sample(2:4, 1), generate_feedback_scores(length(competencies))), na.rm = TRUE), competencies)),
    Reports = list(setNames(colMeans(replicate(sample(2:6, 1), generate_feedback_scores(length(competencies))), na.rm = TRUE), competencies))
  ) %>%
  unnest_wider(c(Self, Manager, Peers, Reports), names_sep = "_")

feedback_data <- feedback_data %>%
  mutate(
    AnalyticalSkill_Peers = ifelse(Department %in% c("R&D", "Production"), AnalyticalSkill_Peers + 0.3, AnalyticalSkill_Peers),
    Communication_Reports = ifelse(Department == "Marketing", Communication_Reports + 0.3, Communication_Reports)
  ) %>%
  mutate(across(starts_with("AnalyticalSkill"), ~pmin(5, .)),
         across(starts_with("Communication"), ~pmin(5, .)))

# ----------- Dataset 3: Employment Record -----------
employment_record <- employees %>%
  mutate(
    Promotions = sample(0:4, n(), replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02)),
    Salary = round(runif(n(), 30000, 120000), -2),
    SalaryIncreasePct = round(runif(n(), 0, 0.10), 3),
    PerformanceRating = sample(1:5, n(), replace = TRUE, prob = c(0.05, 0.1, 0.3, 0.4, 0.15)),
    SickDays = rpois(n(), lambda = 8)
  )

promotion_events <- employment_record %>%
  rowwise() %>%
  do({
    id <- .$EmployeeID
    n_promotions <- .$Promotions
    if (n_promotions == 0) return(NULL)
    data.frame(
      EmployeeID = id,
      PromotionDate = sort(random_dates(n_promotions, .$StartDate, "2024-01-01")),
      NewLevel = sample(levels, n_promotions, replace = TRUE)
    )
  }) %>% bind_rows()

leaving_data <- employees %>%
  slice_sample(prop = 0.2) %>%
  mutate(
    LeavingDate = random_dates(n(), start = "2020-01-01", end = "2024-01-01")
  )

# ----------- Dataset 4: Sales Data -----------
sales_team <- employees %>%
  filter(Department == "Sales")

sales_data <- sales_team %>%
  rowwise() %>%
  mutate(
    Year = list(2019:2024),
    Revenue = list(round(runif(6, 50000, 500000), -2)),
    DealsClosed = list(sample(20:200, 6, replace = TRUE))
  ) %>%
  unnest(cols = c(Year, Revenue, DealsClosed))

# ----------- Dataset 5: Customer Service Ratings -----------
cs_team <- employees %>%
  filter(Department == "Customer Service")

customer_ratings <- cs_team %>%
  rowwise() %>%
  mutate(
    Month = list(seq(ymd("2023-01-01"), ymd("2024-01-01"), by = "month")),
    AvgResponseTime = list(round(runif(13, 30, 300), 1)),
    SatisfactionScore = list(round(runif(13, 3.5, 5), 2)),
    ResolutionRate = list(round(runif(13, 0.7, 1), 2))
  ) %>%
  unnest(cols = c(Month, AvgResponseTime, SatisfactionScore, ResolutionRate))

# ----------- Dataset 6: Production Metrics -----------
prod_team <- employees %>%
  filter(Department == "Production")

production_data <- prod_team %>%
  rowwise() %>%
  mutate(
    Month = list(seq(ymd("2023-01-01"), ymd("2024-01-01"), by = "month")),
    UnitsProduced = list(sample(800:1500, 13, replace = TRUE)),
    DefectRate = list(round(runif(13, 0.01, 0.05), 3)),
    DowntimeHours = list(round(runif(13, 0, 20), 1))
  ) %>%
  unnest(cols = c(Month, UnitsProduced, DefectRate, DowntimeHours))

# ----------- Dataset 7: Learning & Development Activities -----------
learning_topics <- c("Leadership", "Compliance", "Technical", "Soft Skills", "Project Management")
learning_modes <- c("eLearning", "Workshop", "Coaching", "Blended")

learning_data <- employees %>%
  slice_sample(prop = 0.6) %>%
  rowwise() %>%
  mutate(
    ActivityID = generate_ids(1, prefix = "learn"),
    Topic = sample(learning_topics, 1),
    Mode = sample(learning_modes, 1),
    CompletionDate = random_dates(1, start = "2021-01-01", end = "2024-01-01"),
    Hours = sample(2:16, 1),
    FeedbackScore = round(runif(1, 3.5, 5), 2)
  )

# ----------- Dataset 8: Survey Data -----------
survey_items <- c("Respect", "Inclusion", "Workload", "Recognition", "Wellbeing", "Innovation", "Trust")

survey_data <- employees %>%
  slice_sample(prop = 0.7) %>%  # 70% response rate
  rowwise() %>%
  mutate(
    SurveyDate = random_dates(1, "2023-01-01", "2023-12-31"),
    Respect = round(runif(1, 3.5, 5), 2),
    Inclusion = round(runif(1, 3.0, 5), 2),
    Workload = round(runif(1, 2.5, 5), 2),
    Recognition = round(runif(1, 3.0, 5), 2),
    Wellbeing = round(runif(1, 3.0, 5), 2),
    Innovation = round(runif(1, 2.5, 5), 2),
    Trust = round(runif(1, 3.0, 5), 2)
  )

# Preview
head(survey_data)
