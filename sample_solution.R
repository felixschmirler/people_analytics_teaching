#Sample solution to people analytics analysis task

#Load Packages
library(tidyverse) 
library(magrittr)
library(effectsize)
library(survival)
library(survminer)


#Load Datasets
hrsystem_data <- read_csv("hr_data/hrsystem_data.csv") 
applicant_data <- read_csv("hr_data/applicant_data.csv")
feedback_data <- read_csv("hr_data/feedback.csv")
assessmentcenter_data <- read_csv("hr_data/assessmentcenter_data.csv")
personality_data <- read_csv("hr_data/personality_data.csv")
employeesurvey_data <- read_csv("hr_data/employeesurvey_data.csv")


#personality profile of new hires/ Wird systematisch ein gewisser Typ Mensch eingestellt? Wenn ja welche Auswirkungen hat dies auf die strategischen Ziele? -----	
personality_data %>% 
  group_by(Status) %>%
  summarise(
    Adjustment_av = mean(Adjustment),
    Ambition_av = mean(Ambition),
    Sociability_av = mean(Sociability),
    Interpersonal_Sensitivity_av = mean(Interpersonal_Sensitivity),
    Prudence_av = mean(Prudence),
    Inquisitive_av = mean(Inquisitive),
    Learning_Approach_av = mean(Learning_Approach)
  )

#plot
personality_data %>% 
  group_by(Status) %>%
  summarise(
    Adjustment_av = mean(Adjustment),
    Ambition_av = mean(Ambition),
    Sociability_av = mean(Sociability),
    Interpersonal_Sensitivity_av = mean(Interpersonal_Sensitivity),
    Prudence_av = mean(Prudence),
    Inquisitive_av = mean(Inquisitive),
    Learning_Approach_av = mean(Learning_Approach)
  ) %>% 
  pivot_longer(cols = 2:8) %>% 
  ggplot(aes(name, value, fill = Status)) +
  geom_bar(position="dodge", stat="identity")

#statistical tests
t.test(personality_data$Adjustment ~ personality_data$Status)
cohens_d(personality_data$Adjustment ~ personality_data$Status)

t.test(personality_data$Ambition ~ personality_data$Status)
cohens_d(personality_data$Ambition ~ personality_data$Status)

t.test(personality_data$Sociability ~ personality_data$Status)
cohens_d(personality_data$Sociability ~ personality_data$Status)

t.test(personality_data$Interpersonal_Sensitivity ~ personality_data$Status)
cohens_d(personality_data$Interpersonal_Sensitivity ~ personality_data$Status)

t.test(personality_data$Prudence ~ personality_data$Status)
cohens_d(personality_data$Prudence ~ personality_data$Status)

t.test(personality_data$Inquisitive ~ personality_data$Status)
cohens_d(personality_data$Inquisitive ~ personality_data$Status)

t.test(personality_data$Learning_Approach ~ personality_data$Status)
cohens_d(personality_data$Learning_Approach ~ personality_data$Status)

#	Zusammenfassung: Es werden besonders ambitioniert, gewissenhafte und kontaktfreudige Menschen eingestellt.
  #	Was ist gut? Hohe Ambitionen können helfen Innovationsziele zu verfolgen, Gewissenhaftigkeit kann helfen vor Überarbeitung zu schützen. Sociability positiver Faktor für zusammenarbeit als auch um Verbindungen zu knüpfen die unterstützend wirken können
  #	Was ist schlecht? Emotionale Stabilität und Offenheit für neue Erfahrungen scheinen keine Rolle zu spielen für die Einstellung. Auch zu hohe Gewissenhaftigkeit könnte sich nachteilig auf Wellbeing und Innovation auswirken und hohe Ambitionen können auch zu Überlastung führen, wenn die Umgebung zusätzlich viel fordert. 
  #	Was fehlt? Nichts von einer Persönlichkeitsperspektive, man könnte noch Richtung Mindsets Dinge hinzufügen aber schwammiger Übergang zu Persönlichkeit
#Empfehlung: Nicht die dringlichste Baustelle aber man sollte das Einstellungsverfahren auf jeden Fall noch einmal genauer unter die Lupe nehmen, besonders in Bereichen wie Sales wo Stressresistenz besonders wichtig ist oder in Abteilungen, die sich besonders schwer tuen mit Innovation  

#behavioural profile of leaders/ Was sind die typischen Verhaltensweisen der Führungskräfte? Wie helfen oder schaden diese unseren strategischen Zielen? -----	
#rating source 
av_rating_source <- feedback_data %>%
  mutate(
    av_self = (`Self: Inspire with Purpose` +         `Self: Drive for Results` +            `Self: Own the Outcome` +              `Self: Speak Up and Listen` +          `Self: Lead with Empathy` +    `Self: Grow Others` +                  `Self: Bridge Across Boundaries` +     `Self: Act with Integrity` +           `Self: Make it Simple` +               `Self: Lead Change Fearlessly` +       `Self: Learn and Adapt Fast` +    `Self: Champion Diversity`) / 12,
    av_manager =   (`Manager: Inspire with Purpose` +         `Manager: Drive for Results` +            `Manager: Own the Outcome` +              `Manager: Speak Up and Listen` +          `Manager: Lead with Empathy` +    `Manager: Grow Others` +                  `Manager: Bridge Across Boundaries` +     `Manager: Act with Integrity` +           `Manager: Make it Simple` +               `Manager: Lead Change Fearlessly` +       `Manager: Learn and Adapt Fast` +    `Manager: Champion Diversity`) / 12,
    av_peers =  (`Peers: Inspire with Purpose` +         `Peers: Drive for Results` +            `Peers: Own the Outcome` +              `Peers: Speak Up and Listen` +          `Peers: Lead with Empathy` +    `Peers: Grow Others` +                  `Peers: Bridge Across Boundaries` +     `Peers: Act with Integrity` +           `Peers: Make it Simple` +               `Peers: Lead Change Fearlessly` +       `Peers: Learn and Adapt Fast` +    `Peers: Champion Diversity`) / 12,
    av_reports =  (`Reports: Inspire with Purpose` +         `Reports: Drive for Results` +            `Reports: Own the Outcome` +              `Reports: Speak Up and Listen` +          `Reports: Lead with Empathy` +    `Reports: Grow Others` +                  `Reports: Bridge Across Boundaries` +     `Reports: Act with Integrity` +           `Reports: Make it Simple` +               `Reports: Lead Change Fearlessly` +       `Reports: Learn and Adapt Fast` +    `Reports: Champion Diversity`) / 12
  ) %>%
  select(av_self, av_manager, av_peers, av_reports)

#pivot_longer
av_rating_source <- av_rating_source %>%
  pivot_longer(cols = everything())

#compare rating sources
source_fit <- lm(av_rating_source$value ~ av_rating_source$name)
summary(source_fit)
#plot(source_fit)

#rating source 
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

#plot
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
  ) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_bar(stat="identity") + 
  coord_flip()
  
#compare ratings (even though this should be caveated as inconclusive, we might be comparing apples with pears without a benchmark)
av_rating_long <- av_rating %>% 
  pivot_longer(cols = everything()) 
rating_fit <- lm(av_rating_long$value ~ av_rating_long$name)
summary(rating_fit)
#plot(rating_fit)

#understand effect sizes
t.test(av_rating$a_drive, av_rating$f_leadempathy)
cohens_d(av_rating$a_drive, av_rating$f_leadempathy)

t.test(av_rating$a_drive, av_rating$h_make)
cohens_d(av_rating$a_drive, av_rating$h_make)

t.test(av_rating$a_drive, av_rating$l_bridge)
cohens_d(av_rating$a_drive, av_rating$l_bridge)

#Zusammenfassung
  #	Was ist gut? Führungskräfte zeigen starke Erwartungen was Leistungsbereitschaft und Qualität angeht und unterstützen die ambitionierten Wachstums/Veränderungsziele. Auch Empathy und Integrity wird gut gewertet
  #	Was ist schlecht? Harter push for performance ohne genug grow ohne development (grow) ist ein risiko für Wellbeing und Innovation (einfach mehr vom gleichen fordern), auch bridge boundaries und speak up verbesserungswürdig 
  #	Was fehlt? Es wird nicht gemessen inwiefern Manager auf das Wellbeing der Mitarbeiter achten und inwiefern sie selbst role models sind für die richtigen Verhaltensweisen im Bereich Wellbeing und Innovation. Auch Autonomy Support wird nicht gemessen, obwohl es sehr niedrig scored im Unternehmen und für beide Outcomes wichtig ist.
#Empfehlung: Relevanteste Dimensionen zum 360 hinzufügen und Managern direkte skills an die Hand geben mit Überlastung im Team umzugehen und mehr Autonomie zu gewährleisten. Mittelfristig mehr Feedback und Coaching skills.  

#employee survey data/ -	Was haben wir für ein Betriebsklima aktuell? Ist unsere Unternehmenskultur da wo sie sein sollte, um unsere strategischen Ziele zu erreichen? -----	
#create difference from benchmark for visualisation
employeesurvey_data %<>%
  mutate(
    `Diff: I am proud to work for this company.`= `I am proud to work for this company.` - `Benchmark: I am proud to work for this company.`,
    `Diff: I would recommend this company as a great place to work.`= `I would recommend this company as a great place to work.` - `Benchmark: I would recommend this company as a great place to work.`,
    `Diff: I feel motivated to do my best every day.`=  `I feel motivated to do my best every day.`- `Benchmark: I feel motivated to do my best every day.`,
    `Diff: I see myself still working here in two years.`= `I see myself still working here in two years.` - `Benchmark: I see myself still working here in two years.`,
    `Diff: My work gives me a sense of personal accomplishment.`= `My work gives me a sense of personal accomplishment.` - `Benchmark: My work gives me a sense of personal accomplishment.`,
    `Diff: I trust the decisions made by senior leadership.`= `I trust the decisions made by senior leadership.` - `Benchmark: I trust the decisions made by senior leadership.`,
    `Diff: Leaders communicate a clear vision for the future.`= `Leaders communicate a clear vision for the future.` - `Benchmark: Leaders communicate a clear vision for the future.`,
    `Diff: I feel well-informed about what is going on.`= `I feel well-informed about what is going on.` - `Benchmark: I feel well-informed about what is going on.`,
    `Diff: Senior leaders are visible and approachable.`= `Senior leaders are visible and approachable.` - `Benchmark: Senior leaders are visible and approachable.`,
    `Diff: Leadership lives the values of the company.`=  `Leadership lives the values of the company.` - `Benchmark: Leadership lives the values of the company.`,
    `Diff: My manager treats me with respect.`= `My manager treats me with respect.` - `Benchmark: My manager treats me with respect.`, 
    `Diff: My manager gives me regular and useful feedback.`= `My manager gives me regular and useful feedback.` - `Benchmark: My manager gives me regular and useful feedback.`,
    `Diff: My manager supports my professional development.`= `My manager supports my professional development.` - `Benchmark: My manager supports my professional development.`,
    `Diff: My manager communicates clearly and effectively.`= `My manager communicates clearly and effectively.` - `Benchmark: My manager communicates clearly and effectively.`,
    `Diff: My manager motivates me to do my best work.`= `My manager motivates me to do my best work.` - `Benchmark: My manager motivates me to do my best work.`,
    `Diff: My team works well together.`= `My team works well together.` - `Benchmark: My team works well together.`,
    `Diff: I feel supported by my colleagues.`= `I feel supported by my colleagues.` - `Benchmark: I feel supported by my colleagues.`,                    
    `Diff: There is a strong sense of trust within my team.`= `There is a strong sense of trust within my team.` - `Benchmark: There is a strong sense of trust within my team.`, 
    `Diff: People on my team help each other succeed.`=  `People on my team help each other succeed.` - `Benchmark: People on my team help each other succeed.`, 
    `Diff: I feel like I belong on my team.`= `I feel like I belong on my team.` - `Benchmark: I feel like I belong on my team.`,
    `Diff: I have the tools and resources I need.`= `I have the tools and resources I need.`- `Benchmark: I have the tools and resources I need.`,
    `Diff: I understand what is expected of me.`= `I understand what is expected of me.`- `Benchmark: I understand what is expected of me.`,
    `Diff: I can make decisions that affect my work.`= `I can make decisions that affect my work.` - `Benchmark: I can make decisions that affect my work.`, 
    `Diff: I have the autonomy I need to be effective.`= `I have the autonomy I need to be effective.`- `Benchmark: I have the autonomy I need to be effective.`,
    `Diff: I can be myself at work without fear.`= `I can be myself at work without fear.` - `Benchmark: I can be myself at work without fear.`,
    `Diff: I have access to learning and development opportunities.`= `I have access to learning and development opportunities.` - `Benchmark: I have access to learning and development opportunities.`,
    `Diff: I am satisfied with the career opportunities available.`= `I am satisfied with the career opportunities available.` - `Benchmark: I am satisfied with the career opportunities available.`,
    `Diff: My development is a priority for my manager.`= `My development is a priority for my manager.` - `Benchmark: My development is a priority for my manager.`,
    `Diff: I am encouraged to develop new skills.`=  `I am encouraged to develop new skills.` - `Benchmark: I am encouraged to develop new skills.`,
    `Diff: I have a clear understanding of how to progress.`= `I have a clear understanding of how to progress.` - `Benchmark: I have a clear understanding of how to progress.`,
    `Diff: I feel valued for the work I do.`= `I feel valued for the work I do.` - `Benchmark: I feel valued for the work I do.`,
    `Diff: I receive recognition when I do good work.`= `I receive recognition when I do good work.` - `Benchmark: I receive recognition when I do good work.`,
    `Diff: My contributions are acknowledged by my manager.`= `My contributions are acknowledged by my manager.`- `Benchmark: My contributions are acknowledged by my manager.`,
    `Diff: I maintain a healthy work-life balance.`= `I maintain a healthy work-life balance.` - `Benchmark: I maintain a healthy work-life balance.`,
    `Diff: My workload is manageable.`=  `My workload is manageable.` - `Benchmark: My workload is manageable.`,
    `Diff: The company cares about my wellbeing.`= `The company cares about my wellbeing.` - `Benchmark: The company cares about my wellbeing.`,
    `Diff: I feel comfortable taking time off.`= `I feel comfortable taking time off.` - `Benchmark: I feel comfortable taking time off.`,
    `Diff: I can talk openly about stress or mental health.`= `I can talk openly about stress or mental health.` - `Benchmark: I can talk openly about stress or mental health.`,
    `Diff: I feel treated fairly regardless of background.`= `I feel treated fairly regardless of background.` - `Benchmark: I feel treated fairly regardless of background.`,
    `Diff: Diverse perspectives are valued in my team.`= `Diverse perspectives are valued in my team.` - `Benchmark: Diverse perspectives are valued in my team.`,
    `Diff: I feel a sense of belonging.`= `I feel a sense of belonging.` - `Benchmark: I feel a sense of belonging.`,
    `Diff: The company fosters an inclusive environment.`= `The company fosters an inclusive environment.` - `Benchmark: The company fosters an inclusive environment.`,
    `Diff: The company adapts quickly to change.`= `The company adapts quickly to change.` - `Benchmark: The company adapts quickly to change.`,
    `Diff: I am comfortable with the pace of change.`= `I am comfortable with the pace of change.` - `Benchmark: I am comfortable with the pace of change.`,
    `Diff: Innovation is encouraged.`= `Innovation is encouraged.` - `Benchmark: Innovation is encouraged.`,
    `Diff: We can respond to future challenges.`= `We can respond to future challenges.` - `Benchmark: We can respond to future challenges.`,
    `Diff: Communication across departments is effective.`= `Communication across departments is effective.` - `Benchmark: Communication across departments is effective.`,
    `Diff: I know where to find needed information.`= `I know where to find needed information.` - `Benchmark: I know where to find needed information.`,
    `Diff: Feedback from employees is taken seriously.`= `Feedback from employees is taken seriously.` - `Benchmark: Feedback from employees is taken seriously.`,
    `Diff: Internal communications are clear and timely.`= `Internal communications are clear and timely.` - `Benchmark: Internal communications are clear and timely.`,
    `Diff: I am fairly compensated.`= `I am fairly compensated.` - `Benchmark: I am fairly compensated.`,
    `Diff: The benefits meet my needs.`=  `The benefits meet my needs.` - `Benchmark: The benefits meet my needs.`,
    `Diff: My pay reflects my performance.`= `My pay reflects my performance.` - `Benchmark: My pay reflects my performance.`,
    `Diff: People here act with integrity.`=  `People here act with integrity.` - `Benchmark: People here act with integrity.`,
    `Diff: The company lives its values.`=  `The company lives its values.` - `Benchmark: The company lives its values.`
  )

survey_diff <- employeesurvey_data %>% 
  select(performance_24, tenure_years, employee_status, matches("Diff"))
  #employeesurvey_data %>% select(performance_24, tenure_years, employee_status, matches("Diff"))

#descriptives
new_diff <- survey_diff %>% #filter(employee_status != "Current") %>%
  select(-employee_status, -performance_24, -tenure_years) %>%
  summarise(across(everything(), mean)) %>% 
  pivot_longer(everything()) #%>% view()

new_diff %>% 
  ggplot(aes(reorder(name, value), value)) + 
  geom_col() +
  coord_flip()

employeesurvey_data$`Benchmark: I am proud to work for this company.`
t.test(employeesurvey_data$`I am proud to work for this company.`, mu = 5.8)
cohens_d(employeesurvey_data$`I am proud to work for this company.`, mu = 5.8)

#Zusammenfassung: 
#	Was ist gut? Starke Werte/Kultur in Bezug auf Performance/Career, Team Cohesion und Manager Relationship. Good Factors for both Wellbeing and Innovation
  #	Was ist schlecht? Niedriges Vertrauen in Senior Leaders/Communication which introduces uncertainty, very concerning values for everything mental health and autonomy (very red flags), low confidence in adaptability 
  #	Was fehlt? Man könnte Wellbeing noch präziser messen anstatt nur über Work-load, stress, exhaustion, vitality, vigour etc. und auch wenn mindset etwas mehr individueller Faktor ist und vllt besser in einem Training pre-post test ist, würde ich etwas richtung Growth Mindset/Learning Goal Orientation messen.
  

#	Welche Faktoren in unserem Unternehmen, haben den größten Einfluss auf Mitarbeiter-Performance und -Bindung?

#impact on performance 
new_cor_per <- survey_diff %>% 
  select(-employee_status) %>%
  cor(use = "pairwise.complete.obs") #%>% view()


#impact on turnover
survey_diff[6] %>% names()
t.test(survey_diff[[6]] ~ survey_diff$employee_status)
cohens_d(survey_diff[[6]] ~ survey_diff$employee_status)


#create empty tible
turnover_effects <- tibble(Cohens_d = double(), CI = double(), CI_low = double(), CI_high = double(), item = character())

for (i in 4:57) {
  #survey_diff[i] %>% names() %>% print()
  #t.test(survey_diff[[i]] ~ survey_diff$employee_status) %>% print()
  #cohens_d(survey_diff[[i]] ~ survey_diff$employee_status) %>% print()
  temp <- as_tibble(cohens_d(survey_diff[[i]] ~ survey_diff$employee_status))
  temp %<>% 
    mutate(
      item = survey_diff[i] %>% names()
    )
  
  turnover_effects %<>% 
    rbind(temp)
  
}

#survival analysis
#dichotomise one of the survey items (could also be done for an aggregated score)
hist(employeesurvey_data$`I see myself still working here in two years.`)
mean(employeesurvey_data$`I see myself still working here in two years.`)
median(employeesurvey_data$`I see myself still working here in two years.`)
table(employeesurvey_data$`I see myself still working here in two years.`)

employeesurvey_data %<>%
  mutate(
    turnover_intention = ifelse(`I see myself still working here in two years.` > 5, "low", "high"),
    status = ifelse(employee_status == "Current", 0, 1),
    survey_date = as.Date("2024-04-01"),
    tenure_after_survey = ifelse(employee_status == "Current", 
                                 as.Date("2024-12-31") - survey_date,
                                 as.Date(leaving_date, format = "%m/%d/%Y") - survey_date)
  )

table(employeesurvey_data$turnover_intention)
employeesurvey_data %>% select(tenure_after_survey) %>% view()

# Create Surv object
surv_object <- Surv(time = employeesurvey_data$tenure_after_survey,
                    event = employeesurvey_data$status)

# Fit Kaplan-Meier model stratified by turnover intention
km_fit <- survfit(surv_object ~ turnover_intention, data = employeesurvey_data)

# Plot survival curves
ggsurvplot(
  km_fit,
  data = employeesurvey_data,
  conf.int = TRUE,                # Show confidence intervals
  pval = TRUE,                    # Show log-rank test p-value
  risk.table = TRUE,             # Add risk table below the plot
  xlab = "Time since survey days",
  ylab = "Survival probability"
)

#Zusammenfassung: Von den wichtigsten Prädiktoren für Performance und Mitarbeiterbindung sind einige sehr gut wie Recognition und gute Beziehungen, andere hingegen wie der hohge workload und Veränderungsprozesse sowie mangelnde Autonomie treiben Mitarbeiter aus dem Unternehmen und könnten auch einen Einfluss auf unseren Unternehmenserfolg haben. 
