#### annual report ####


#### Module_4_Section_B ####
names(Module_4_Section_B)

View(Module_4_Section_B)

### sub service

Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  group_by(Year, `Sub-Program`)%>%
  mutate(`Sub-Program Count` = sum(Count))%>%
  ggplot(aes(x = `Sub-Program Count`, y = `Sub-Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = `Sub-Program`, label = `Sub-Program Count`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year by Program Group")

m4sb_sp_23 <-Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  group_by(Year, `Sub-Program`)%>%
  mutate(`Sub-Program Count` = sum(Count))%>%
  filter(Year == 2023)%>%
  ungroup()%>%
  select(!c(Count, Year))%>%
rename("2023 Sub-Program Count" = `Sub-Program Count`)

m4sb_sp_22 <- Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  group_by(Year, `Sub-Program`)%>%
  mutate(`Sub-Program Count` = sum(Count))%>%
  filter(Year == 2022)%>%
  ungroup()%>%
  select(!c(Count, Year))%>%
  rename("2022 Sub-Program Count" = `Sub-Program Count`)

names(m4sb_sp_22)

joined_m4sb_sp <- left_join(m4sb_sp_23, m4sb_sp_22, by = c("Program", "Sub-Program", "Service"))

percent_change_m4sb_sp <- joined_m4sb_sp %>%
  mutate(Change = `2023 Sub-Program Count` - `2022 Sub-Program Count`)%>%
  mutate(`Percent Change` = round(100*Change / `2022 Sub-Program Count`,2))

percent_change_m4sb_sp%>%
  ggplot(aes(x = `Percent Change`, y = `Sub-Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = `Sub-Program`, label = `Percent Change`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Percent Change in Program Year by Program Group")


#### service 

Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Count, y = Service, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Service, label = Count), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year by Service")

m4sb_service_23 <-Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Year == 2023)%>%
  ungroup()%>%
  select(!Year)%>%
  rename("2023 Service Count" = `Count`)

m4sb_service_23

m4sb_service_22 <- Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Year == 2022)%>%
  ungroup()%>%
  select(!c(Year))%>%
  rename("2022 Service Count" = `Count`)

names(m4sb_service_22)

joined_m4sb_service <- left_join(m4sb_service_23, m4sb_service_22, by = c("Program", "Sub-Program", "Service"))

percent_change_m4sb_service <- joined_m4sb_service %>%
  mutate(Change = `2023 Service Count` - `2022 Service Count`)%>%
  mutate(`Percent Change` = round(100*Change / `2022 Service Count`,2))

percent_change_m4sb_service %>%
  ggplot(aes(x = `Percent Change`, y = `Service`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = `Service`, label = `Percent Change`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Percent Change in Program Year by Service")


#### program

Module_4_Section_B %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  group_by(Year, `Program`)%>%
  mutate(`Program Count` = sum(Count))%>%
  ggplot(aes(x = `Program Count`, y = `Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = `Program`, label = `Program Count`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+p
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year by Program")

m4sb_program_23 <-Module_4_Section_B %>%
    mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
    group_by(Year, `Program`)%>%
    mutate(`Program Count` = sum(Count))%>%
    filter(Year == 2023)%>%
    ungroup()%>%
    select(!c(Count, Year))%>%
    rename("2023 Program Count" = `Program Count`)
  
m4sb_program_22 <- Module_4_Section_B %>%
    mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
    group_by(Year, `Program`)%>%
    mutate(`Program Count` = sum(Count))%>%
    filter(Year == 2022)%>%
    ungroup()%>%
    select(!c(Count, Year))%>%
    rename("2022 Program Count" = `Program Count`)
  

  
joined_m4sb_program <- left_join(m4sb_program_23, m4sb_program_22, by = c("Program", "Sub-Program", "Service"))
  
percent_change_m4sb_program <- joined_m4sb_program %>%
    mutate(Change = `2023 Program Count` - `2022 Program Count`)%>%
    mutate(`Percent Change` = round(100*Change / `2022 Program Count`,2))
  
percent_change_m4sb_program %>%
  ggplot(aes(x = `Percent Change`, y = `Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = `Program`, label = `Percent Change`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Percent Change in Program Year by Program")


  
  
#### Module_4_Section_C
names(Module_4_Section_C)

View(Module_4_Section_C)



Module_4_Section_C %>%
  filter(Group == "Total")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Count, y = Measure, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Count), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Total")


names(Module_4_Section_C)

gender_age_measures <- c("Male", "Female", "Other", "Unknown", "0 to 5",
                         "6 to 13", "14 to 17", "18 to 24",
                         "25 to 44", "45 to 54", "55 to 59", "60 to 64",
                         "65 to 74", "75+")

gender_age_total <- Module_4_Section_C %>%
  filter(Subgroup == "Age")%>%
  group_by(Year)%>%
  summarise(Total = sum(Count))

total_22_pre <- gender_age_total %>%
  filter(Year == 2022)%>%
  select(Total)

total_23_pre <- gender_age_total %>%
  filter(Year == 2023)

total_22 <- total_22_pre$Total

total_23 <- total_23_pre$Total

total_23

total_22

Module_4_Section_C %>%
  filter(Subgroup == "Gender"|
           Subgroup == "Age")%>%
  mutate(Measure = factor(Measure, levels = gender_age_measures))%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Count, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Count), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Age and Gender")

m4sc_age_gender_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Gender"|
           Subgroup == "Age")%>%
  filter(Measure %in% gender_age_measures)%>%
  filter(Year == 2023)%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_age_gender_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Gender"|
           Subgroup == "Age")%>%
  filter(Measure %in% gender_age_measures)%>%
  filter(Year == 2022)%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

names(m4sc_age_gender_23)

m4sc_age_gender_joined <- left_join(m4sc_age_gender_23, m4sc_age_gender_22, by = c("Group", "Subgroup", "Measure"))

percent_change_m4sc_age_gender_joined <- m4sc_age_gender_joined %>%
  mutate(Change = `2023 Count` - `2022 Count`)%>%
  mutate(`Percent Change` = round(100*Change / `2022 Count`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  filter(`Percent Change` != -100)

percent_change_m4sc_age_gender_joined %>%
  ggplot(aes(x = `Percent Change`, y = `Measure`, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = `Measure`, label = `Percent Change`), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Percent Change in in Age and Gender")

m4sc_age_gender_23_percent <- m4sc_age_gender_23 %>%
  mutate(`2023 Percent` = round(100*`2023 Count` / total_23,2))

m4sc_age_gender_22_percent <- m4sc_age_gender_22 %>%
  mutate(`2022 Percent` = round(100*`2022 Count` / total_22,2))
  
joined_m4sc_age_gender_percent <- left_join(m4sc_age_gender_23_percent, m4sc_age_gender_22_percent, by = c("Group", "Subgroup", "Measure"))

long_joined_m4sc_age_gender <- joined_m4sc_age_gender_percent %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_joined_m4sc_age_gender %>%
  mutate(Measure = factor(Measure, levels = gender_age_measures))%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Type == "Percent")%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Age and Gender Percentages")


### education

View(Module_4_Section_C)

education_measures <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  select(Measure)

education_measures_string <- education_measures$Measure

Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  mutate(Measure = factor(Measure, levels = education_measures_string))%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Count, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Count), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Education")

m4sc_edu_1424_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  filter(str_detect(Measure, "14-24"))%>%
  summarise(Total = sum(Count))

m4sc_edu_1424_total_23 <- m4sc_edu_1424_23$Total


m4sc_edu_25_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  filter(str_detect(Measure, "14-24", negate = TRUE))%>%
  summarise(Total = sum(Count))

m4sc_edu_25_total_23 <- m4sc_edu_25_23$Total


m4sc_edu_1424_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2022)%>%
  filter(str_detect(Measure, "14-24"))%>%
  summarise(Total = sum(Count))

m4sc_edu_1424_total_22 <- m4sc_edu_1424_22$Total


m4sc_edu_25_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2022)%>%
  filter(str_detect(Measure, "14-24", negate = TRUE))%>%
  summarise(Total = sum(Count))

m4sc_edu_25_total_22 <- m4sc_edu_25_22$Total

m4sc_edu_25_total_22


m4sc_education_23_u25 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  filter(str_detect(Measure, "14-24"))%>%
  mutate(`2023 Percent Under 25` = round(100*Count / m4sc_edu_1424_total_23,2))%>%
  rename("2023 Count Under 25" = "Count")%>%
  select(!Year)

m4sc_education_23_25over <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  filter(str_detect(Measure, "14-24", negate = TRUE))%>%
  mutate(`2023 Percent 25+` = round(100*Count / m4sc_edu_25_total_23,2))%>%
  rename("2023 Count 25+" = "Count")%>%
  select(!Year)


m4sc_education_22_u25 <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2022)%>%
  filter(str_detect(Measure, "14-24"))%>%
  mutate(`2022 Percent Under 25` = round(100*Count / m4sc_edu_1424_total_22,2))%>%
  rename("2022 Count Under 25" = "Count")%>%
  select(!Year)

m4sc_education_22_25over <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2022)%>%
  filter(str_detect(Measure, "14-24", negate = TRUE))%>%
  mutate(`2022 Percent 25+` = round(100*Count / m4sc_edu_25_total_22,2))%>%
  rename("2022 Count 25+" = "Count")%>%
  select(!Year)

m4sc_education_25over_joined <- left_join(m4sc_education_23_25over, m4sc_education_22_25over, by = c("Group", "Subgroup", "Measure"))

m4sc_education_u25_joined <- left_join(m4sc_education_23_u25, m4sc_education_22_u25, by = c("Group", "Subgroup", "Measure"))

m4sc_education_25over_percent_change <- m4sc_education_25over_joined %>%
  mutate(`Percent Change 25+` = round(100*(`2023 Count 25+` - `2022 Count 25+`)/ `2022 Count 25+`,2))

m4sc_education_u25_percent_change <- m4sc_education_u25_joined %>%
  mutate(`Percent Change Under 25` = round(100*(`2023 Count Under 25` - `2022 Count Under 25`)/ `2022 Count Under 25`,2))



long_m4sc_education_u25 <- m4sc_education_u25_joined %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_education_25over <- m4sc_education_25over_joined %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_education_25over


long_m4sc_education_25over %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = education_measures_string))%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Education Percentages")

long_m4sc_education_u25 %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = education_measures_string))%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = Measure, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Under 25 Education Percentages")


#### outcomes 

names(outcomes)

mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 4 2022")%>%
  filter(Achieved != 0)%>%
  ggplot(aes(x = Achieved, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = Achieved), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.y = element_text(size = rel(1.3)))+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(legend.text = element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(y = " ", x = " ")+
  ggtitle("CSBG Outcomes Quarter 1 2023 and Quarter 4 2022")