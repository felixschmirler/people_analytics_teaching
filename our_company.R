# Explore the company with simple numeric summaries and visualisations ----
# All questions refer to current employees only.

# 0. Setup ----
library(tidyverse)

hrsystem_data <- read_csv("hr_data/hrsystem_data.csv")

current <- hrsystem_data %>%
  filter(employee_status == "Current")


# 1. Wie groß ist das Unternehmen? ----

# Numeric
current %>%
  summarise(employees = n())

# Kommentar: Deutlich über Mittelstand von der Größe

# 2. Wie groß ist das Unternehmen in verschiedenen Ländern? ----

# Numeric
current %>%
  count(country)

# Visual
current %>%
  ggplot(aes(x = country)) +
  geom_bar()

# Kommentar: ein Unternehmen mit Mittelpunkt in Deutschland und Europa


# 3. Welche Departments gibt es und wie groß sind sie in verschiedenen Ländern? ----

# Numeric
current %>%
  count(department)

# Visual: number of employees in each department by country
current %>%
  ggplot(aes(x = department)) +
  geom_bar()

# Kommentar: sehr großer Produktionsanteil (+großer Vertieb und Kundenservice). 


# Numeric
current %>%
  count(country, department)

# Visual: number of employees in each department by country
current %>%
  ggplot(aes(x = country, fill = department)) +
  geom_bar()

# Kommentar: Produktion und Entwicklung nur in D und CZ  
# Departments in anderen Ländern deuten auf reine Vertriebsstandorte 


# 4. Wie sind die Job Level in den Departments verteilt? ----

# Numeric: number and percentage within each department
current %>%
  count(department, job_level) %>%
  group_by(department) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Visual: proportions within each department
current %>%
  ggplot(aes(x = department, fill = job_level)) +
  geom_bar(position = "fill")

# Kommentar: Produktion hat großen Anteil an Manual (blue collor) Jobs
# auch viele entry level in sales und customer service


# 5. Wie ist die Geschlechterverteilung? ----

## 5a. Insgesamt
# Numeric
current %>%
  count(gender) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Visual
current %>%
  ggplot(aes(x = gender)) +
  geom_bar()

# Kommentar: Ein Männerdominiertes Unternehmen

## 5b. Nach Land
# Numeric
current %>%
  count(country, gender) %>%
  group_by(country) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Visual
current %>%
  ggplot(aes(x = country, fill = gender)) +
  geom_bar(position = "fill")

# Kommentar: Länderunterschiede kommen oft von Unterschieden in Departments

## 5c. Nach Department
# Numeric
current %>%
  count(department, gender) %>%
  group_by(department) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Visual
current %>%
  ggplot(aes(x = department, fill = gender)) +
  geom_bar(position = "fill")

# Kommentar: Production unüberraschenderweise Grund des Geschlechterverhältnisses
# In Tschechien alleine würde man daher vllt eine kleinere oder sogar negative 
# Gender Pay Gap finden

## 5d. Nach Job Level
# Numeric
current %>%
  count(job_level, gender) %>%
  group_by(job_level) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Visual
current %>%
  ggplot(aes(x = job_level, fill = gender)) +
  geom_bar(position = "fill")

# Kommentar: Leider ein typisches Muster, besonders in hohen Führungspositionen 
# ist der Anteil von Frauen niedriger als im Rest des Unternehmens


# 6. Wie lang ist die durchschnittliche Tenure? ----

## 6a. Insgesamt
# Numeric
current %>%
  summarise(mean_tenure = round(mean(tenure_years, na.rm = TRUE), 1))

## 6b. Nach Land
# Numeric
current %>%
  group_by(country) %>%
  summarise(mean_tenure = round(mean(tenure_years, na.rm = TRUE), 1))

# Visual
current %>%
  group_by(country) %>%
  summarise(mean_tenure = mean(tenure_years, na.rm = TRUE)) %>%
  ggplot(aes(x = country, y = mean_tenure)) +
  geom_col()

# Kommentar: Länderunterschiede kommen oft von Unterschieden in Departments

## 6c. Nach Department
# Numeric
current %>%
  group_by(department) %>%
  summarise(mean_tenure = round(mean(tenure_years, na.rm = TRUE), 1))

# Visual
current %>%
  group_by(department) %>%
  summarise(mean_tenure = mean(tenure_years, na.rm = TRUE)) %>%
  ggplot(aes(x = department, y = mean_tenure)) +
  geom_col()

# Kommentar: Wie häufig im produzierenden Gewerbe besonders lange Bindung ans 
# Unternehmen in der Produktion, weniger in den Departments mit hohem Druck und
# und objektiven Performance Indikatoren

## 6d. Nach Job Level
# Numeric
current %>%
  group_by(job_level) %>%
  summarise(mean_tenure = round(mean(tenure_years, na.rm = TRUE), 1))

# Visual
current %>%
  group_by(job_level) %>%
  summarise(mean_tenure = mean(tenure_years, na.rm = TRUE)) %>%
  ggplot(aes(x = job_level, y = mean_tenure)) +
  geom_col()

# Kommentar: Die Unterschiede hier sind untypisch, da wir jedoch wissen, dass 
# ein großer Teil der Entry Level/ Manual Jobs in der Produktion ist, sollten 
# wir uns das nach Department ansehen.

## 6e. Nach Job Level Und Department ()
# Numeric
current %>%
  group_by(job_level, department) %>%
  summarise(mean_tenure = round(mean(tenure_years, na.rm = TRUE), 1))

# Visual
current %>%
  group_by(job_level, department) %>%
  summarise(mean_tenure = mean(tenure_years, na.rm = TRUE)) %>%
  ggplot(aes(x = department, y = mean_tenure, fill = job_level)) + #job level in fill, nicht x achse
  geom_col()

# Kommentar: Wir sehen über alle Departments außer Produktion: je mehr senior, 
# desto länger bleiben die Mitarbeiter im Unternehmen. Ausnahme ist das Middle 
# Management, die häufiger von extern kommen um spezifische Ziele zu erreichen.
