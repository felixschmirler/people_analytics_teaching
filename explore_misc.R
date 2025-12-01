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
feedback_data <- read_csv("hr_data/feedback.csv")
employeesurvey_data <- read_csv("hr_data/employeesurvey_data.csv")


#360 Feedback Data ----

##1. rating dimensions ----

#create combined ratings across sources for each dimension (equally weighted)
av_rating <- feedback_data %>%
  mutate(
    i_inspire = (`Self: Inspire with Purpose` +   `Manager: Inspire with Purpose` + `Peers: Inspire with Purpose` +  `Reports: Inspire with Purpose`)/4,
    a_drive =  (`Self: Drive for Results`   + `Manager: Drive for Results` + `Peers: Drive for Results` + `Reports: Drive for Results`)/4,
    b_own =  (`Self: Own the Outcome` + `Manager: Own the Outcome` + `Peers: Own the Outcome` + `Reports: Own the Outcome`)/4,
    k_speak =  (`Self: Speak Up and Listen` + `Manager: Speak Up and Listen` + `Peers: Speak Up and Listen` + `Reports: Speak Up and Listen`)/4,
    f_leadempathy =  (`Self: Lead with Empathy`  + `Manager: Lead with Empathy`+ `Peers: Lead with Empathy` + `Reports: Lead with Empathy`)/4,
    j_grow =  (`Self: Grow Others` + `Manager: Grow Others` + `Peers: Grow Others` + `Reports: Grow Others`)/4,
    l_bridge = (`Self: Bridge Across Boundaries` + `Manager: Bridge Across Boundaries` + `Peers: Bridge Across Boundaries` + `Reports: Bridge Across Boundaries`)/4 ,
    d_act =  (`Self: Act with Integrity` + `Manager: Act with Integrity` + `Peers: Act with Integrity` + `Reports: Act with Integrity`)/4,
    h_make =  (`Self: Make it Simple` + `Manager: Make it Simple`+ `Peers: Make it Simple` + `Reports: Make it Simple`)/4,
    e_leadchange =  (`Self: Lead Change Fearlessly` + `Manager: Lead Change Fearlessly` + `Peers: Lead Change Fearlessly` + `Reports: Lead Change Fearlessly`)/4,
    c_learn =  (`Self: Learn and Adapt Fast` + `Manager: Learn and Adapt Fast` + `Peers: Learn and Adapt Fast` + `Reports: Learn and Adapt Fast`)/4,
    g_champion =  (`Self: Champion Diversity` + `Manager: Champion Diversity` + `Peers: Champion Diversity` + `Reports: Champion Diversity`)/4
  ) %>%
  select(i_inspire, a_drive, b_own, k_speak, f_leadempathy, j_grow, l_bridge, d_act, h_make, e_leadchange, c_learn, g_champion)

#create average scores for each dimension
av_rating %>% 
  summarise(
    inspire = mean(i_inspire),
    drive =  mean(a_drive),
    own = mean(b_own),
    speak = mean(k_speak),
    leadempathy =  mean(f_leadempathy),
    grow = mean(j_grow),
    bridge = mean(l_bridge),
    act =  mean(d_act),
    make =  mean(h_make),
    leadchange =  mean(e_leadchange),
    learn = mean(c_learn),
    champion =  mean(g_champion)
  ) %>% #view() 
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_col() + 
  coord_flip()

##2. rating source ----

#create combined ratings across sources for each dimension (equally weighted)
av_rating_source <- feedback_data %>%
  mutate(
    av_self = (`Self: Inspire with Purpose` +         `Self: Drive for Results` +            `Self: Own the Outcome` +              `Self: Speak Up and Listen` +          `Self: Lead with Empathy` +    `Self: Grow Others` +                  `Self: Bridge Across Boundaries` +     `Self: Act with Integrity` +           `Self: Make it Simple` +               `Self: Lead Change Fearlessly` +       `Self: Learn and Adapt Fast` +    `Self: Champion Diversity`) / 12,
    av_manager =   (`Manager: Inspire with Purpose` +         `Manager: Drive for Results` +            `Manager: Own the Outcome` +              `Manager: Speak Up and Listen` +          `Manager: Lead with Empathy` +    `Manager: Grow Others` +                  `Manager: Bridge Across Boundaries` +     `Manager: Act with Integrity` +           `Manager: Make it Simple` +               `Manager: Lead Change Fearlessly` +       `Manager: Learn and Adapt Fast` +    `Manager: Champion Diversity`) / 12,
    av_peers =  (`Peers: Inspire with Purpose` +         `Peers: Drive for Results` +            `Peers: Own the Outcome` +              `Peers: Speak Up and Listen` +          `Peers: Lead with Empathy` +    `Peers: Grow Others` +                  `Peers: Bridge Across Boundaries` +     `Peers: Act with Integrity` +           `Peers: Make it Simple` +               `Peers: Lead Change Fearlessly` +       `Peers: Learn and Adapt Fast` +    `Peers: Champion Diversity`) / 12,
    av_reports =  (`Reports: Inspire with Purpose` +         `Reports: Drive for Results` +            `Reports: Own the Outcome` +              `Reports: Speak Up and Listen` +          `Reports: Lead with Empathy` +    `Reports: Grow Others` +                  `Reports: Bridge Across Boundaries` +     `Reports: Act with Integrity` +           `Reports: Make it Simple` +               `Reports: Lead Change Fearlessly` +       `Reports: Learn and Adapt Fast` +    `Reports: Champion Diversity`) / 12
  ) %>%
  select(av_self, av_manager, av_peers, av_reports)

#create average scores for each rating source
av_rating_source %>% 
  summarise(
    self = mean(av_self),
    manager =  mean(av_manager),
    peers = mean(av_peers),
    reports = mean(av_reports)
  ) %>%
  pivot_longer(cols = everything()) %>% #view() 
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_col() + 
  coord_flip()

#Performance Data ---- 
hrsystem_data %>% 
  group_by(department) %>% 
  summarise(
    av_rating = mean(performance_24, na.rm = TRUE)
  ) %>% #view() 
  ggplot(aes(department, av_rating)) +
  geom_col() 

#there are nicer ways to explore the distribution but tough for ordinal data

#hard to compare due to different number of employees
hrsystem_data %>%  
  ggplot(aes(performance_24)) +
  geom_bar() +
  facet_wrap(~department)

#density plots don't work here either
hrsystem_data %>% 
  ggplot(aes(performance_24)) +
  geom_density() +
  facet_wrap(~department)

#staked bar chart is maybe the best option 
hrsystem_data %>% 
  count(department, performance_24) %>% #view()
  filter(!is.na(performance_24)) %>%
  ggplot(aes(department, n, fill = performance_24)) +
  geom_bar(position="fill", stat="identity") 

#Employee survey data ----

##1.Completion rates ----

#count per department doesn't tell us the percentages 
employeesurvey_data %>% 
  count(department)

#asssuming the data was pulled from the system at the same time let's look at current employees
#there are a lot of assumptions about this data e.g. maternity leaves etc. removed

hrsystem_data %>%
  filter(employee_status == "Current") %>%
  count(department)

#you can do it manually from here or automate this completely

complete <- employeesurvey_data %>% 
  count(department) %>% 
  rename(completed = n)

total = hrsystem_data %>%
  filter(employee_status == "Current") %>%
  count(department) %>%
  rename(total = n)

completion_rates <- left_join(complete, total) %>%
  mutate(
    completion_rate = completed / total,
    completion_rate = completion_rate %>% 
      round(3) %>%
      scales::percent() 
      
    )
  
write_csv(completion_rates, paste0(Sys.Date(),"completionrates.csv"))
file.remove(paste0(Sys.Date(),"completionrates.csv"))

            