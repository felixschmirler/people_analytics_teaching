#Script to practice predictive validity and adverse impact 

#Setup ----

#Load Packages
#install.packages("tidyverse")
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load Datasets
applicant_data <- read_csv("hr_data/applicant_data.csv")
assessmentcenter_data <- read_csv("hr_data/assessmentcenter_data.csv")
personality_data <- read_csv("hr_data/personality_data.csv")
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 


#Predictive Validity ----

#explore hiring data
view(applicant_data) #no score, just hired or not
view(assessmentcenter_data) #assessment center score but no performance score
view(personality_data) #a lot of scores but we don't know if there was an overall score or how the personality traits were weighted
view(hrsystem_data) #some people have performance data that we could match with our assessment center data

#join the assessment center data with the performance ratings from the hr system
performance_data <- hrsystem_data %>%
  select(employee_id, performance_24)

assessmentcenter_performance <- left_join(assessmentcenter_data, performance_data)

assessmentcenter_performance %>% 
  count(performance_24)

#Solution: calculate predictive validity ----
?cor #if not familiar with functions, take a look at the documentation
?cor.test

cor(assessmentcenter_performance$assessmentcenter_score, assessmentcenter_performance$performance_24, use = "complete.obs") 
cor.test(assessmentcenter_performance$assessmentcenter_score, assessmentcenter_performance$performance_24, use = "complete.obs") 

#well, not great but at least a bit useful. Worth reviewing if there are certain aspects that can be improved

#Adverse impact ---- 

#explore applicant data 
applicant_data %>%
  count(gender) #attention! long format data! many people were in multiple stages

#explore applicant data 
applicant_data %>%
  count(gender, status) #attention! long format data! many people were in multiple stages

applicant_data %>%
  count(stage) #multiple assessment stages, probably different for every department

#look at one applicant
applicant_data %>% 
  filter(applicant_id == "00meqf7Xk0") %>%
  view()

#look at selection by stage and gender
applicant_data %>%
  count(stage, gender, status) %>%
  view() #this is what we need but could get a bit complicated 

#create new simpler dataset to look at selection rates at the CV screening stage only
applicant_data %>%
  count(stage, gender, status) %>%
  filter(stage == "Stage 1: CV Screening")

#solution: calculate 4/5ths rule ----
#female selection rate
37608 / 151828 #0.2477013
0.2477013 / 0.2521669 #98.2% compared to men

#male selection rate
45182 / 179175 #0.2521669

#other selection rate
370 / 1637 #0.2260232
0.2260232/0.2521669 #89.6%

#no clear evidence for adverse impact from a first glance but definitely somehting that should be explored with statistical tests and different application rates + culture could still lead to problems
