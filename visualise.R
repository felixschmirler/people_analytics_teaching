#Script to Explore the different HR Datasets

#Setup ----

#Load Packages
library(tidyverse) #packages for data wrangling, analysis and visualisation

#Load Datasets
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 
applicant_data <- read_csv("hr_data/applicant_data.csv")
feedback_data <- read_csv("hr_data/feedback.csv")
assessmentcenter_data <- read_csv("hr_data/assessmentcenter_data.csv")
personality_data <- read_csv("hr_data/personality_data.csv")
employeesurvey_data <- read_csv("hr_data/employeesurvey_data.csv")

#visualisations ----

#let's look at tenure distribution
hist(hrsystem_data$tenure_years)

#distribution
hrsystem_data %>%
  ggplot(aes(x = tenure_years)) +
  geom_histogram()  #alternative
#geom_density() #alternative
#geom_boxplot() #alternative

#distribution by employee status
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  geom_histogram(position = "identity", alpha = 0.5)  

#better to look at this with a density plot to understand proportions 
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = employee_status)) +
  geom_density(position = "identity", alpha = 0.5) 

#1. tenure by job level 
#let's look at a factor with more levels - a bit crowded
hrsystem_data %>%
  ggplot(aes(x = tenure_years, fill = job_level)) +
  geom_density(position = "identity", alpha = 0.5) 

#facet wrap can help
hrsystem_data %>%
  #filter(department != "Production") %>% #data looks bimodal because of Production, let's look at data without Production
  ggplot(aes(x = tenure_years)) +
  geom_density(position = "identity", alpha = 0.5) +
  facet_wrap(~job_level)

#not normally distributed, maybe boxplot can help
hrsystem_data %>%
  filter(department != "Production") %>%
  ggplot(aes(x = tenure_years)) +
  geom_boxplot(position = "identity", alpha = 0.5) +
  facet_wrap(~job_level)

#can we flip this?
hrsystem_data %>%
  filter(department != "Production") %>%
  ggplot(aes(x = tenure_years)) +
  geom_boxplot(position = "identity", alpha = 0.5) +
  facet_wrap(~job_level) + 
  coord_flip()

#let's look at a scatter plot
feedback_data %>%
ggplot(aes(x=`Peers: Drive for Results`, y=`Reports: Drive for Results`)) +
  geom_point() #+
  #geom_smooth(method=lm , color="red", se=FALSE) 


#let's plot the number of employees by country
hrsystem_data %>% 
  ggplot(aes(x = country)) +
  geom_bar()

#lets split by employee status and add a title
hrsystem_data %>% 
  ggplot(aes(x = country, fill = employee_status)) +
  geom_bar() + 
  ggtitle("Great graph: Employees in different Countries")

#we can also plot numeric data as barcharts (but this is for reporting only, not for exploration)
# Calculates mean (sd etc. can also be computed for standard errors)
hrsystem_data %>%
  group_by(job_level) %>%
  summarise( 
    mean=mean(tenure_years)
  ) %>%
  ggplot(aes(x=job_level, y=mean)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=0.5) +
  coord_flip()

#Aufgaben ----

#1. Wie unterscheidet sich die Verteilung der Mitarbeiter auf die verschiedenen Job Level in den verschiedenen Abteilungen? ----
hrsystem_data %>% 
  filter(employee_status == "Current") %>% 
  ggplot(aes(department, fill = job_level)) +
  geom_bar() #Sehr viele Entry Levle Positionen in Sales und Customer Service, mehr senior employees in R&D
  
#2.   Wie sind die verschiedenen Departments auf die verschiedenen Länder verteilt? ----
hrsystem_data %>%
  filter(employee_status == "Current") %>% 
  ggplot(aes(country, fill = department)) + 
  geom_bar() #Hauptstandort Deutschland, Tschechine zusätzlicher Produktionsstandort mit R&D und HR Shared service Center, Andere Europäsche Länder Hauptsächlich Vertrieb und Kundenservice

#3. Wie unterscheidet sich die typische Tenure zwischen Ländern, Departments und Job Leveln?
hrsystem_data %>% 
  ggplot(aes(tenure_years, fill = country)) +
  geom_density(position = "identity", alpha = 0.5) #+
  #facet_wrap(~department) #Länder scheinen auf den ersten Blick unterschiedliche Tenure zu haben, aber wenn man sich die Tenure in jedem Land für die verschiedenen Departments ansieht, sieht man, dass nur die Departments Unterschiede haben, nicht aber die Länder

hrsystem_data %>% 
  ggplot(aes(tenure_years, fill = department)) +
  geom_density(position = "identity", alpha = 0.5) #Sales hat die kürzeste Tenure im Schnitt, also hoher Turnover; Die Leute bleiben sehr lange in Production und R&D scheint auch Leute für länger zu halten. Zum Teil könnte dies auch durch die Zusammensetzung erklärt werden z.B. mehr junior employees = kürzere typische Tenure

hrsystem_data %>% 
  ggplot(aes(tenure_years, fill = job_level)) +
  geom_density(position = "identity", alpha = 0.5) #Unüberraschenderweise bleiben Mitarbeiter in gehobenen Positionen länger in einem Unternehmen (sind vllt niedrig eingestiegung und befördert worden bzw. generell längere Zeit in einem Job erwartbar; Ausnahme sind Director/Head of Rollen, also mittleres Management, etwas häufiger extern eingestellt für strategische Ziele und auch viel Druck auf mittleres Management)

  
#4.  Wie ist die Geschlechterverteilung in den verschiedenen Departments und job levels? ----
hrsystem_data %>% 
  filter(employee_status == "Current") %>% 
  ggplot(aes(job_level, fill = gender)) +
  geom_bar() #Generell höherer Anteil an Männern in allen Job Levels

hrsystem_data %>% 
  filter(employee_status == "Current") %>% 
  ggplot(aes(department, fill = gender)) +
  geom_bar() #Höherer Anteil an Frauen in Customer Service, Marketing und HR, deutlich höherer Männeranteil in der Produktion und R&D

#   Gibt es beim einstellen neuer Mitarbeiter einen Bias?
applicant_data %>% 
  filter(status == "hired") %>%
  ggplot(aes(gender)) +
  geom_bar() #Es werden mehr Männer eingestellt, aber davon wissen wir noch nicht, ob sich nicht einfach mehr Männer bewerben und die Einstellungsverfahren trotzdem fair sind.

applicant_data %>% 
  ggplot(aes(status, fill = gender)) +
  geom_bar() +
  facet_wrap(~stage) #Ehrlich gesagt, hart zu erkennen, aber scheint ungefähr ausgeglichen, da müssen wir wohl eher Zahlen anschauen und vllt einen statistischen Test machen

#   Welchen Score braucht man in dem Sales Assessment Center ungefähr um eingestellt zu werden? 
assessmentcenter_data %>% 
  ggplot(aes(assessmentcenter_score, fill = status)) + 
  geom_histogram(position = "identity", alpha = 0.5) #ungefähr 3.75 

#   Ist das Assessment Center ein valides Instrument, um Performance vorherzusagen? 
assessmentcenter_data %>% 
  left_join(hrsystem_data %>% select(employee_id, performance_24)) %>%
  ggplot(aes(assessmentcenter_score, performance_25)) +
  geom_point() +
  geom_smooth(method = lm) #sieht schon mal gut aus, aber hier brauchen wir einen statistsichen Test

#   Hat ein Geschlecht bessere Performance Bewertungen?
hrsystem_data %>%
  ggplot(aes(performance_24, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.5) #weder Histogramm noch Density machen hier mega viel Sinn, auch ein barchart hat Nachteile, das würde man am besten mit einer Tabelle lösen. aber auch hier würde man einen statistischen Test machen wollen



