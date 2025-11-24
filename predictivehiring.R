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

#create dataset with only employee id and performance data
performance_data <- hrsystem_data %>%
  select(employee_id, performance_24)

#join the assessment center data with the performance ratings from the hr system
assessmentcenter_performance <- left_join(assessmentcenter_data, performance_data)

#explore the data visually 

#performance distribution
assessmentcenter_performance %>% 
  count(performance_24) %>%
  ggplot(aes(performance_24, n)) +
  geom_col() + 
  ylim(0, 500) #not a terrible performance distribution
  
#assessment center score distribution
assessmentcenter_performance %>%
  ggplot(aes(assessmentcenter_score, fill = status)) + 
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.1)
  
#visually explor the correlation between assessment center score and performance data  
assessmentcenter_performance %>% 
    ggplot(aes(performance_24, assessmentcenter_score)) +
    geom_jitter()
  
#Solution: calculate predictive validity ----
?cor #if not familiar with functions, take a look at the documentation
?cor.test

cor(assessmentcenter_performance$assessmentcenter_score, assessmentcenter_performance$performance_24, use = "complete.obs") 
cor.test(assessmentcenter_performance$assessmentcenter_score, assessmentcenter_performance$performance_24, use = "complete.obs") 

#well, not great but at least a bit more useful than useless. Worth reviewing if there are certain aspects that can be improved

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

#create simplified stage variable
applicant_data <- applicant_data %>%
  mutate(
    stage_simplified = str_sub(stage, 1, 7)
  )

#look at selection by stage and gender
applicant_data %>%
  count(stage_simplified, gender, status) %>%
  view() #this is what we need but a bit hard to navigate 

#quick visualisation
applicant_data %>%
  count(stage_simplified, gender, status) %>% 
  ggplot(aes(stage_simplified, n, fill = gender)) +
  geom_col()

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

#statistical tests for each category against majority group
male_female_table <- applicant_data %>%
  filter(stage == "Stage 1: CV Screening", gender != "Other") %>%
  select(gender, status) %>%
  table()

chisq.test(male_female_table) #significant, very large sample size though
library(vcd) #for large samples more important than significans: effect size
assocstats(male_female_table)

male_other_table <- applicant_data %>%
  filter(stage == "Stage 1: CV Screening", gender != "Female") %>%
  select(gender, status) %>%
  table()

chisq.test(male_other_table) 
assocstats(male_other_table)
