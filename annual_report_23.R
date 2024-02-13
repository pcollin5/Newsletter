#### annual report ####


#### Module_4_Section_B ####
names(Module_4_Section_B)

View(Module_4_Section_B)

### program 

total_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

all_total_23 <- total_23$Total

all_total_23

total_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

all_total_22 <- total_22$Total

all_total_22

program_percent_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  group_by(Program)%>%
  mutate(`Program Total` = sum(Count))%>%
  reframe(`2023` = round(100*`Program Total` / all_total_23,2))%>%
  unique()

program_percent_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  group_by(Program)%>%
  mutate(`Program Total` = sum(Count))%>%
  reframe(`2022` = round(100*`Program Total` / all_total_23,2))%>%
  unique()

joined_program_percent <- full_join(program_percent_23, program_percent_22)

joined_program_percent_with_difference <- joined_program_percent %>%
  mutate(Difference = `2023` - `2022`)

m4sb_programs_percent_of_total <- joined_program_percent_with_difference %>%
  pivot_longer(-Program,names_to = "Year", values_to = "Percent")%>%
  filter(Year != "Difference")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = `Percent`, y = `Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = `Program`, label = `Percent`), color = "white")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent of Community Service")

m4sb_programs_percent_of_total

m4sb_percent_change_program_of_total <- joined_program_percent_with_difference %>%
  pivot_longer(-Program,names_to = "Year", values_to = "Percent")%>%
  filter(Year == "Difference")%>%
  ggplot(aes(x = `Percent`, y = `Program`, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = `Program`, label = `Percent`), color = "white")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent of Community Service Difference")

m4sb_percent_change_program_of_total


m4sb_program_totals <- Module_4_Section_B %>%
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year by Program")

m4sb_program_totals

m4sb_percent_change_program_of_total

m4sb_programs_percent_of_total

### sub service

m4sb_sub_program_totals <- Module_4_Section_B %>%
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


m4sb_sub_program_totals

# need totals for percents 
#2023

emp_23 <- Module_4_Section_B %>%
  filter(Program == "Employment Service")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

emp_total_23 <- emp_23$Total  

emp_subprogram_23 <- Module_4_Section_B%>%
  filter(Program == "Employment Service")%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / emp_total_23,2))%>%
  ungroup()%>%
  mutate(`2023 Service Percent` = round(100*(Count) / emp_total_23,2))




edu_23 <- Module_4_Section_B %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

edu_total_23 <- edu_23$Total

edu_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / edu_total_23,2))%>%
  ungroup()%>%
  mutate(`2023 Service Percent` = round(100*(Count) / edu_total_23,2))

edu_subprogram_23

income_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  filter(str_detect(Program, "Income"))%>%
  summarise(Total = sum(Count))

income_total_23 <- income_23$Total

income_total_23

income_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Income"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / income_total_23,2))%>%
  mutate(`2023 Service Percent` = round(100*(Count) / income_total_23,2))

income_subprogram_23


housing_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  filter(str_detect(Program, "Housing"))%>%
  summarise(Total = sum(Count))

housing_total_23 <- housing_23$Total

housing_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / housing_total_23,2))%>%
  mutate(`2023 Service Percent` = round(100*(Count) / housing_total_23,2))

housing_subprogram_23


health_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  filter(str_detect(Program, "Health"))%>%
  summarise(Total = sum(Count))

health_total_23 <- health_23$Total

health_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / health_total_23,2))%>%
  mutate(`2023 Service Percent` = round(100*(Count) / health_total_23,2))


health_subprogram_23

multi_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  filter(str_detect(Program, "Multiple"))%>%
  summarise(Total = sum(Count))

multi_total_23 <- multi_23$Total

multi_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Multiple"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / multi_total_23,2))%>%
  mutate(`2023 Service Percent` = round(100*(Count) / multi_total_23,2))


civic_23 <- Module_4_Section_B %>%
  filter(Year == 2023)%>%
  filter(str_detect(Program, "Civic"))%>%
  summarise(Total = sum(Count))

civic_total_23 <- civic_23$Total

civic_subprogram_23 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Civic"))%>%
  filter(Year == 2023)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2023 Subprogram Count` = sum(Count))%>%
  mutate(`2023 Subprogram Percent` = round(100*`2023 Subprogram Count` / civic_total_23,2))%>%
  mutate(`2023 Service Percent` = round(100*(Count) / civic_total_23,2))

civic_subprogram_23


#2022

emp_22 <- Module_4_Section_B %>%
  filter(Program == "Employment Service")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

emp_total_22 <- emp_22$Total  

emp_subprogram_22 <- Module_4_Section_B%>%
  filter(Program == "Employment Service")%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / emp_total_22,2))%>%
  ungroup()%>%
  mutate(`2022 Service Percent` = round(100*(Count) / emp_total_22,2))




edu_22 <- Module_4_Section_B %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

edu_total_22 <- edu_22$Total

edu_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / edu_total_22,2))%>%
  ungroup()%>%
  mutate(`2022 Service Percent` = round(100*(Count) / edu_total_22,2))

edu_subprogram_22

income_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  filter(str_detect(Program, "Income"))%>%
  summarise(Total = sum(Count))

income_total_22 <- income_22$Total

income_total_22

income_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Income"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / income_total_22,2))%>%
  mutate(`2022 Service Percent` = round(100*(Count) / income_total_22,2))

income_subprogram_22


housing_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  filter(str_detect(Program, "Housing"))%>%
  summarise(Total = sum(Count))

housing_total_22 <- housing_22$Total

housing_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / housing_total_22,2))%>%
  mutate(`2022 Service Percent` = round(100*(Count) / housing_total_22,2))

housing_subprogram_22


health_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  filter(str_detect(Program, "Health"))%>%
  summarise(Total = sum(Count))

health_total_22 <- health_22$Total

health_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / health_total_22,2))%>%
  mutate(`2022 Service Percent` = round(100*(Count) / health_total_22,2))


health_subprogram_22

multi_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  filter(str_detect(Program, "Multiple"))%>%
  summarise(Total = sum(Count))

multi_total_22 <- multi_22$Total

multi_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Multiple"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / multi_total_22,2))%>%
  mutate(`2022 Service Percent` = round(100*(Count) / multi_total_22,2))


civic_22 <- Module_4_Section_B %>%
  filter(Year == 2022)%>%
  filter(str_detect(Program, "Civic"))%>%
  summarise(Total = sum(Count))

civic_total_22 <- civic_22$Total

civic_subprogram_22 <- Module_4_Section_B%>%
  filter(str_detect(Program, "Civic"))%>%
  filter(Year == 2022)%>%
  group_by(`Sub-Program`)%>%
  mutate(`2022 Subprogram Count` = sum(Count))%>%
  mutate(`2022 Subprogram Percent` = round(100*`2022 Subprogram Count` / civic_total_22,2))%>%
  mutate(`2022 Service Percent` = round(100*(Count) / civic_total_22,2))

civic_subprogram_22

subprograms_23 <- rbind(emp_subprogram_23, edu_subprogram_23, income_subprogram_23, housing_subprogram_23, health_subprogram_23, multi_subprogram_23,
      civic_subprogram_23)%>%
  select(!Year)%>%
  rename("2023 Service Count" = "Count")

subprograms_22 <- rbind(emp_subprogram_22, edu_subprogram_22, income_subprogram_22, housing_subprogram_22, health_subprogram_22, multi_subprogram_22,
      civic_subprogram_22)%>%
  select(!Year)%>%
  rename("2022 Service Count" = "Count")

joined_percents <- full_join(subprograms_23, subprograms_22, by = c("Program", "Sub-Program", "Service"))

names(joined_percents)

mutated_joined_percents <- joined_percents %>%
  mutate(`Service Percent Change` = round(100*(`2023 Service Count` - `2022 Service Count`) / `2022 Service Count`,2))%>%
  mutate(`Service Percent Difference` = `2023 Service Percent` - `2022 Service Percent`)%>%
  mutate(`Subprogram Percent Change` = round(100*(`2023 Subprogram Count` - `2022 Subprogram Count`)/ `2022 Subprogram Count`,2))%>%
  mutate(`Subprogram Percent Difference` = `2023 Subprogram Percent` - `2022 Subprogram Percent`)


long_joined_percents <- mutated_joined_percents %>%
  pivot_longer(-c("Program", "Sub-Program", "Service"), names_to = c("Year", "Category", "Type"), values_to = "Value", names_sep = " ")

#### now make the charts for domains

long_joined_percents

# employment and income

m4sb_emp_income_service_totals <- long_joined_percents %>%
  filter(Program == "Employment Service"|
           Program == "Income and Asset Building Services")%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Count")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
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
  guides(
    fill = guide_legend(
      title = "Program",
      override.aes = aes(label = "")
    )
  )+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Totals by Service")

m4sb_emp_income_service_totals

m4sb_emp_income_serivce_percent_of_program <- long_joined_percents %>%
  filter(Program == "Employment Service"|
           Program == "Income and Asset Building Services")%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Percent")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
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
  guides(
    fill = guide_legend(
      title = "Program",
      override.aes = aes(label = "")
    )
  )+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Service of Program")

m4sb_emp_income_serivce_percent_of_program


m4sb_emp_income_service_percent_change <- long_joined_percents %>%
  filter(Program == "Employment Service"|
           Program == "Income and Asset Building Services")%>%
  filter(Year == "Service")%>%
  filter(Type == "Change")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  guides(
    fill = guide_legend(
      title = "Program",
      override.aes = aes(label = "")
    )
  )+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent Change in Service Totals")

m4sb_emp_income_service_percent_change


m4sb_emp_income_service_percent_of_program_change <- long_joined_percents %>%
  filter(Program == "Employment Service"|
           Program == "Income and Asset Building Services")%>%
  filter(Year == "Service")%>%
  filter(Type == "Difference")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Program))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  guides(
    fill = guide_legend(
      title = "Program",
      override.aes = aes(label = "")
    )
  )+
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Difference in Percent of Program")

m4sb_emp_income_service_percent_of_program_change


#education

m4sb_education_service_totals <- long_joined_percents %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Count")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Totals by Service")

m4sb_education_service_totals

m4sb_education_service_percent_of_program <- long_joined_percents %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Percent")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Service of Program")

m4sb_education_service_percent_of_program

m4sb_education_service_percent_change <- long_joined_percents %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Change")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent Change in Service Totals")

m4sb_education_service_percent_change

m4sb_education_service_percent_difference <- long_joined_percents %>%
  filter(str_detect(Program, "Education"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Difference")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Difference in Percent of Program")


m4sb_education_service_percent_difference






#housing
m4sb_housing_service_totals <- long_joined_percents %>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Count")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Totals by Service")

m4sb_housing_service_totals


m4sb_housing_service_percent_of_program <- long_joined_percents %>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Percent")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Service of Program")

m4sb_housing_service_percent_of_program

m4sb_housing_service_percent_change <- long_joined_percents %>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Change")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent Change in Service Totals")

m4sb_housing_service_percent_change

m4sb_housing_service_percent_of_program_difference <- long_joined_percents %>%
  filter(str_detect(Program, "Housing"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Difference")%>%
  filter(Category != "Subprogram")%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Difference in Percent of Program")


m4sb_housing_service_percent_of_program_difference

# health

m4sb_health_service_totals <- long_joined_percents %>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Count")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Totals by Service")

m4sb_health_service_totals

m4sb_health_service_percent_of_program <- long_joined_percents %>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Percent")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Service Percent of Program")

m4sb_health_service_percent_of_program

m4sb_health_service_percent_change <- long_joined_percents %>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Change")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent Change in Service Totals")


m4sb_health_service_percent_change

m4sb_health_service_percent_of_program_change <- long_joined_percents %>%
  filter(str_detect(Program, "Health"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Difference")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Difference in Percent of Program")

m4sb_health_service_percent_of_program_change

# multi

m4sb_multiple_service_totals <- long_joined_percents %>%
  filter(str_detect(Program, "Multi"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Count")%>%
  filter(Category != "Subprogram")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Totals by Service")

m4sb_multiple_service_totals

m4sb_multiple_service_percent_of_program <- long_joined_percents %>%
  filter(str_detect(Program, "Multi"))%>%
  filter(Year == "2023"|
           Year == "2022")%>%
  filter(Type == "Percent")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  ggplot(aes(x = Value, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year+`Sub-Program`, scales = "free_y")+
  geom_label(aes(group = Service, label = Value), color = "white")+
  scale_fill_uethda()+
  theme(legend.position = "none")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Service Percent of Program")

m4sb_multiple_service_percent_of_program


m4sb_multiple_service_percent_change <- long_joined_percents %>%
  filter(str_detect(Program, "Multi"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Change")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Percent Change in Service Totals")

m4sb_multiple_service_percent_change

m4sb_multiple_service_percent_of_program_difference <- long_joined_percents %>%
  filter(str_detect(Program, "Multi"))%>%
  filter(Year == "Service")%>%
  filter(Type == "Difference")%>%
  filter(Category != "Subprogram")%>%
  filter(Value != 0)%>%
  ggplot(aes(x = `Value`, y = Service, fill = Service))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Program, label = Value), color = "white")+
  facet_wrap(~`Sub-Program`, scales = "free_y")+
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
  ggtitle("Module 4 Section B 2021-2022 and 2022-2023 Program Year Difference in Percent of Program")


m4sb_multiple_service_percent_of_program_difference




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
  
m4sb_programs_percent_change <- percent_change_m4sb_program %>%
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

m4sb_programs_percent_change
  
  
#### Module_4_Section_C
names(Module_4_Section_C)

View(Module_4_Section_C)



m4sc_total <- Module_4_Section_C %>%
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

m4sc_total

m4sc_total_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Total")%>%
  filter(Year == 2023)%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_total_23

m4sc_total_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Total")%>%
  filter(Year == 2022)%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)



m4sc_total_joined <- left_join(m4sc_total_23, m4sc_total_22, by = c("Group", "Subgroup", "Measure"))

m4sc_total_joined

percent_change_m4sc_total_joined <- m4sc_total_joined %>%
  mutate(Change = `2023 Count` - `2022 Count`)%>%
  mutate(`Percent Change` = round(100*Change / `2022 Count`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  filter(`Percent Change` != -100)

m4sc_total_percent_change <- percent_change_m4sc_total_joined %>%
  ggplot(aes(x = `Percent Change`, y = Measure, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_label(aes(group = Measure, label = `Percent Change`), color = "white")+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Total Percent Change")

m4sc_total_percent_change

names(Module_4_Section_C)


#### gender and age 

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

m4sc_age_gender_totals <- Module_4_Section_C %>%
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

m4sc_age_gender_totals

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

m4sc_age_gender_percent_change <- percent_change_m4sc_age_gender_joined %>%
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

m4sc_age_gender_percent_change

m4sc_age_gender_23_percent <- m4sc_age_gender_23 %>%
  mutate(`2023 Percent` = round(100*`2023 Count` / total_23,2))

m4sc_age_gender_22_percent <- m4sc_age_gender_22 %>%
  mutate(`2022 Percent` = round(100*`2022 Count` / total_22,2))
  
joined_m4sc_age_gender_percent <- left_join(m4sc_age_gender_23_percent, m4sc_age_gender_22_percent, by = c("Group", "Subgroup", "Measure"))

long_joined_m4sc_age_gender <- joined_m4sc_age_gender_percent %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

m4sc_age_gender_percent_of_total <- long_joined_m4sc_age_gender %>%
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

m4sc_age_gender_percent_of_total

### education

View(Module_4_Section_C)

education_measures <- Module_4_Section_C %>%
  filter(Subgroup == "Education")%>%
  filter(Year == 2023)%>%
  select(Measure)

education_measures_string <- education_measures$Measure

m4sc_education_totals <- Module_4_Section_C %>%
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

m4sc_education_totals


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

View(long_m4sc_education_25over)


m4sc_education_25over_percent <- long_m4sc_education_25over %>%
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

m4sc_education_25over_percent

m4sc_education_u25_percent <- long_m4sc_education_u25 %>%
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

m4sc_education_u25_percent

long_m4sc_education_u25_percent_change <- m4sc_education_u25_percent_change%>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")


long_m4sc_education_25over_percent_change <- m4sc_education_25over_percent_change%>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

m4sc_u25_education_percent_change_graph <- long_m4sc_education_u25_percent_change  %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = education_measures_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Under 25 Years Education Percent Change")

m4sc_u25_education_percent_change_graph

m4sc_o25_education_percent_change_graph <- long_m4sc_education_25over_percent_change  %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = education_measures_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Over 25 Years Education Percent Change")


m4sc_o25_education_percent_change_graph

### race/ethnicity

ethnicity_race <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Year == 2023)%>%
  select(Measure)%>%
  filter(Measure != "Unknown")

ethnicity_race_string <- ethnicity_race$Measure

ethnicity_race_string

m4sc_race_ethnicity_totals <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = ethnicity_race_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Ethnicity/Race")


m4sc_race_ethnicity_totals


m4sc_race_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_race_total_23 <- m4sc_race_23$Total

m4sc_race_total_23

m4sc_race_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_race_total_22 <- m4sc_race_22$Total

m4sc_race_total_22

m4sc_race_ethnicity_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_race_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_race_ethnicity_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Ethnicity/Race")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_race_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_race_ethnicity_joined <- left_join(m4sc_race_ethnicity_23, m4sc_race_ethnicity_22, by = c("Group", "Subgroup", "Measure"))

m4sc_race_ethnicity_joined

m4sc_race_ethnicity_percent_change <- m4sc_race_ethnicity_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_race_ethnicity_percent_change <- m4sc_race_ethnicity_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_race_ethnicity_percent_change

long_m4sc_race_ethnicity_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = ethnicity_race_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Race/Ethnicity Percentages")

m4sc_race_ethnicity_percent_change <- long_m4sc_race_ethnicity_percent_change %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = ethnicity_race_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Race/Ethnicity Percent Change")

m4sc_race_ethnicity_percent_change


#### work status 

work_status <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  filter(Year == 2023)%>%
  select(Measure)

work_status_string <- work_status$Measure

m4sc_work_status <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  mutate(Measure = factor(Measure, levels = work_status_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Work Status")

m4sc_work_status

m4sc_work_status_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_work_status_total_23 <- m4sc_work_status_23$Total

m4sc_work_status_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_work_status_total_22 <- m4sc_work_status_22$Total

m4sc_work_status_percent_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_work_status_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_work_status_percent_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Work Status (18+)")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_work_status_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_work_status_joined <- left_join(m4sc_work_status_percent_23, m4sc_work_status_percent_22, by = c("Group", "Subgroup", "Measure"))

m4sc_work_status_joined 

m4sc_work_status_percent_change <- m4sc_work_status_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_work_status_percent_change <- m4sc_work_status_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_work_status_percent_change

m4sc_work_status_percent <- long_m4sc_work_status_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = work_status_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Work Status Percentages")

m4sc_work_status_percent 


m4sc_work_status_percent_change <- long_m4sc_work_status_percent_change %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = work_status_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Work Status Percent Change")

m4sc_work_status_percent_change


#### military status and disconnected youth

mil_discon <- Module_4_Section_C %>%
  filter(Subgroup == "Miltary Status"|
           Subgroup == "Disconnected Youth")%>%
  filter(Year == 2023)%>%
  select(Measure)

mil_discon_string <- mil_discon$Measure

mil_discon_string

m4sc_mil_disconnected_youth_totals <- Module_4_Section_C %>%
  filter(Subgroup == "Miltary Status"|
           Subgroup == "Disconnected Youth")%>%
  mutate(Measure = factor(Measure, levels = mil_discon_string))%>%
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
  ggtitle("Module 4 Section C Characteristics: Military Status and Disconnected Youth")

m4sc_mil_disconnected_youth_totals



m4sc_mil_23 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Military"))%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_mil_total_23 <- m4sc_mil_23$Total

m4sc_mil_total_23

Module_4_Section_C %>%
  filter(str_detect(Subgroup, "Discon"))


m4sc_discon_23 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Disconnected"))%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_discon_total_23 <- m4sc_discon_23$Total

m4sc_discon_total_23



m4sc_mil_dis_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Miltary Status"|
           Subgroup == "Disconnected Youth")%>%
  filter(Year == 2023)%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)



m4sc_mil_dis_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Miltary Status"|
           Subgroup == "Disconnected Youth")%>%
  filter(Year == 2022)%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)


m4sc_mil_dis_joined <- left_join(m4sc_mil_dis_23, m4sc_mil_dis_22, by = c("Group", "Subgroup", "Measure"))

m4sc_mil_dis_percent_change <- m4sc_mil_dis_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))


long_m4sc_mil_dis_percent_change<- m4sc_mil_dis_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_mil_dis_percent_change

long_m4sc_mil_dis_percent_change %>%
  filter(Type == "Count")%>%
  mutate(Measure = factor(Measure, levels = mil_discon_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Military Service and Disconnected Youth Percentages")

m4sc_mil_dis_percent_change_graph <- long_m4sc_mil_dis_percent_change %>%
  filter(Type == "Change")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = mil_discon_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C Characteristics: Military and Disconnected Youth Percent Change")


m4sc_mil_dis_percent_change_graph

#### health and insurance

disability <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  filter(Year == 2023)%>%
  select(Measure)

disability_string <- disability$Measure

m4sc_disability_totals_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  mutate(Measure = factor(Measure, levels = disability_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Disability")

m4sc_disability_totals_graph



m4sc_disability_23 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Disabling"))%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_disability_total_23 <- m4sc_disability_23$Total

m4sc_disability_22 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Disabling"))%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_disability_total_22 <- m4sc_disability_22$Total

m4sc_disability_percents_23 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Disabling"))%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_disability_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_disability_percents_22 <- Module_4_Section_C %>%
  filter(str_detect(Measure, "Disabling"))%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_disability_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_disability_joined <- left_join(m4sc_disability_percents_23, m4sc_disability_percents_22, by = c("Group", "Subgroup", "Measure"))

m4sc_disability_joined

m4sc_disability_percent_change <- m4sc_disability_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_disability_percent_change <- m4sc_disability_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

# health ins 

m4sc_health_ins_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  filter(str_detect(Measure, "Disabling", negate = TRUE))%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_health_ins_total_23 <- m4sc_health_ins_23$Total

m4sc_health_ins_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  filter(str_detect(Measure, "Disabling", negate = TRUE))%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_health_ins_total_22 <- m4sc_health_ins_22$Total

m4sc_health_ins_percents_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  filter(str_detect(Measure, "Disabling", negate = TRUE))%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_health_ins_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_health_ins_percents_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Health")%>%
  filter(str_detect(Measure, "Disabling", negate = TRUE))%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_health_ins_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_health_ins_joined <- left_join(m4sc_health_ins_percents_23, m4sc_health_ins_percents_22, by = c("Group", "Subgroup", "Measure"))

m4sc_health_ins_joined

m4sc_health_ins_percent_change <- m4sc_health_ins_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_health_ins_percent_change <- m4sc_health_ins_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")


long_m4sc_disability_and_health_ins <- rbind(long_m4sc_disability_percent_change, long_m4sc_health_ins_percent_change)



m4sc_disability_and_health_insurance_graph <- long_m4sc_disability_and_health_ins %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = disability_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Disability and Health Insurance")

m4sc_disability_and_health_insurance_graph

m4sc_disability_health_ins_percent_change <- long_m4sc_disability_and_health_ins %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = disability_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Disability and Health Insurance Percent Change")

m4sc_disability_health_ins_percent_change

### health ins types 

health_ins_type <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  filter(Year == 2023)%>%
  select(Measure)%>%
  filter(Measure != "Unknown")

health_ins_string <- health_ins_type$Measure

m4sc_health_ins_totals_graphs <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  mutate(Measure = factor(Measure, levels = health_ins_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Health Insurance Sources")

m4sc_health_ins_totals_graphs

m4sc_health_ins_type_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_health_ins_type_total_23 <- m4sc_health_ins_type_23$Total

m4sc_health_ins_type_total_23

m4sc_health_ins_type_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_health_ins_type_total_22 <- m4sc_health_ins_type_22$Total

m4sc_health_ins_type_total_22

m4sc_health_ins_type_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_health_ins_type_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_health_ins_type_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Health Insurance Sources")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_health_ins_type_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  filter(Measure != "Unknown")%>%
  select(!Year)

m4sc_health_ins_type_joined <- left_join(m4sc_health_ins_type_23, m4sc_health_ins_type_22, by = c("Group", "Subgroup", "Measure"))

m4sc_health_ins_type_joined

m4sc_health_ins_type_percent_change <- m4sc_health_ins_type_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_health_ins_type_percent_change <- m4sc_health_ins_type_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_health_ins_type_percent_change

m4sc_health_ins_percent_graph <- long_m4sc_health_ins_type_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = health_ins_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Health Insurance Sources Percentages")

m4sc_health_ins_percent_graph

m4sc_health_ins_percent_change_graph <-long_m4sc_health_ins_type_percent_change %>%
  filter(Measure != "State CHIP")%>%
  filter(Measure != "State Health Insurance for Adults")%>%
  filter(Measure != "Type of Policy Unknown")%>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = health_ins_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Health Insurance Sources Percent Change")

m4sc_health_ins_percent_change_graph

### household type

hh_type <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  filter(Year == 2023)%>%
  select(Measure)

hh_type_string <- hh_type$Measure

hh_type_string

m4sc_hh_type_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  mutate(Measure = factor(Measure, levels = hh_type_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Type")

m4sc_hh_type_count_graph

m4sc_hh_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_hh_total_23 <- m4sc_hh_23$Total

m4sc_hh_total_23

m4sc_hh_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_hh_total_22 <- m4sc_hh_22$Total

m4sc_hh_total_22

m4sc_hh_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_hh_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_hh_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Type")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_hh_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_hh_joined <- left_join(m4sc_hh_23, m4sc_hh_22, by = c("Group", "Subgroup", "Measure"))

m4sc_hh_joined

m4sc_hh_percent_change <- m4sc_hh_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_hh_percent_change <- m4sc_hh_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_hh_percent_change


m4sc_hh_percent_graph <- long_m4sc_hh_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = hh_type_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Type Percentages")

m4sc_hh_percent_graph

m4sc_hh_percent_change_graph <- long_m4sc_hh_percent_change  %>%
  filter(Type == "Change")%>%
  mutate(Measure = factor(Measure, levels = hh_type_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Type Percent Change")


m4sc_hh_percent_change_graph

### hh size 

hh_size <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  filter(Year == 2023)%>%
  select(Measure)

hh_size_string <- hh_size$Measure

hh_size_string

m4sc_hh_size_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  mutate(Measure = factor(Measure, levels = hh_size_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Size")

m4sc_hh_size_count_graph

m4sc_hh_size_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_hh_size_total_23 <- m4sc_hh_size_23$Total

m4sc_hh_size_total_23

m4sc_hh_size_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_hh_size_total_22 <- m4sc_hh_size_22$Total

m4sc_hh_size_total_22

m4sc_hh_size_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_hh_size_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_hh_size_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Size")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_hh_size_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_hh_size_joined <- left_join(m4sc_hh_size_23, m4sc_hh_size_22, by = c("Group", "Subgroup", "Measure"))

m4sc_hh_size_joined

m4sc_hh_size_percent_change <- m4sc_hh_size_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_hh_size_percent_change <- m4sc_hh_size_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_hh_size_percent_change

m4sc_hh_size_percent_graph <- long_m4sc_hh_size_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = hh_size_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Size Percentages")

m4sc_hh_size_percent_graph

m4sc_hh_size_percent_change_graph <- long_m4sc_hh_size_percent_change  %>%
  filter(Type == "Change")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = hh_size_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Size Percent Change")

m4sc_hh_size_percent_change_graph

### housing

housing <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  filter(Year == 2023)%>%
  select(Measure)

housing_string <- housing$Measure

m4sc_housing_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  mutate(Measure = factor(Measure, levels = housing_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Housing Type")

m4sc_housing_count_graph


m4sc_housing_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_housing_total_23 <- m4sc_housing_23$Total

m4sc_housing_total_23

m4sc_housing_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_housing_total_22 <- m4sc_housing_22$Total

m4sc_housing_total_22

m4sc_housing_type_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_housing_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_housing_type_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Housing")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_housing_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_housing_joined <- left_join(m4sc_housing_type_23, m4sc_housing_type_22, by = c("Group", "Subgroup", "Measure"))

m4sc_housing_joined

m4sc_housing_percent_change <- m4sc_housing_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_housing_percent_change <- m4sc_housing_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_housing_percent_change

m4sc_housing_percent_graph <- long_m4sc_housing_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = housing_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Housing Type Percentages")

m4sc_housing_percent_graph 

m4sc_housing_percent_change_graph <- long_m4sc_housing_percent_change  %>%
  filter(Type == "Change")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = housing_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Housing Type Percent Change")

m4sc_housing_percent_change_graph

### Household Incomes (% of HHS Guideline)

hh_income <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  filter(Year == 2023)%>%
  select(Measure)

hh_income_string <- hh_income$Measure

m4sc_hh_income_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  mutate(Measure = factor(Measure, levels = hh_income_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Income as Percent of HHS Guideline")

m4sc_hh_income_count_graph

m4sc_hh_income_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_hh_income_total_23 <- m4sc_hh_income_23$Total

m4sc_hh_income_total_23

m4sc_hh_income_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_hh_income_total_22 <- m4sc_hh_income_22$Total

m4sc_hh_income_total_22

m4sc_hh_income_perc_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_hh_income_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_hh_income_perc_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Household Incomes (% of HHS Guideline)")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_hh_income_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_hh_income_joined <- left_join(m4sc_hh_income_perc_23, m4sc_hh_income_perc_22, by = c("Group", "Subgroup", "Measure"))

m4sc_hh_income_joined

m4sc_hh_income_percent_change <- m4sc_hh_income_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_hh_income_percent_change <- m4sc_hh_income_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_hh_income_percent_change

m4sc_hh_income_percent_graph <- long_m4sc_hh_income_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = hh_income_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Income Percentages")

m4sc_hh_income_percent_graph

m4sc_hh_income_percent_change <- long_m4sc_hh_income_percent_change %>%
  filter(Type == "Change")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = hh_income_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Income Percent Change")


m4sc_hh_income_percent_change

# Sources of Household Income

income_source <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  filter(Year == 2023)%>%
  select(Measure)

income_source_string <- income_source$Measure

m4sc_hh_income_sources_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  mutate(Measure = factor(Measure, levels = income_source_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Sources of Household Income")

m4sc_hh_income_sources_count_graph


m4sc_is_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_is_total_23 <- m4sc_is_23$Total

m4sc_hh_total_23

m4sc_is_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_is_total_22 <- m4sc_is_22$Total

m4sc_is_total_22

m4sc_income_source_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_is_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_income_source_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Sources of Household Income")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_is_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_income_source_joined <- left_join(m4sc_income_source_23, m4sc_income_source_22, by = c("Group", "Subgroup", "Measure"))

m4sc_income_source_joined

m4sc_income_source_percent_change <- m4sc_income_source_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_income_source_percent_change <- m4sc_income_source_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_income_source_percent_change

m4sc_hh_income_sources_percent_graph <- long_m4sc_income_source_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = income_source_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Income Source")

m4sc_hh_income_sources_percent_graph


m4sc_hh_income_sources_percent_change_graph <- long_m4sc_income_source_percent_change  %>%
  filter(Type == "Change")%>%
  filter(Measure != "Unknown")%>%
  mutate(Measure = factor(Measure, levels = income_source_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Household Income Source Percent Change")

m4sc_hh_income_sources_percent_change_graph


# Other Income Source

other_source <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  filter(Year == 2023)%>%
  select(Measure)

other_source_string <- other_source$Measure

m4sc_other_income_source_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  mutate(Measure = factor(Measure, levels = other_source_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Other Household Income Source")

m4sc_other_income_source_count_graph


m4sc_oi_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_oi_total_23 <- m4sc_oi_23$Total

m4sc_oi_total_23

m4sc_oi_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_oi_total_22 <- m4sc_oi_22$Total

m4sc_oi_total_22

m4sc_oi_source_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_hh_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_oi_source_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Other Income Source")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_hh_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_oi_joined <- left_join(m4sc_oi_source_23, m4sc_oi_source_22, by = c("Group", "Subgroup", "Measure"))

m4sc_oi_joined

m4sc_oi_percent_change <- m4sc_oi_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_oi_percent_change <- m4sc_oi_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_oi_percent_change

m4sc_other_income_percent_graph <- long_m4sc_oi_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = other_source_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Other Household Income Percentages")

m4sc_other_income_percent_graph

m4sc_other_income_percent_change_graph <- long_m4sc_oi_percent_change  %>%
  filter(Type == "Change")%>%
  filter(Value != 0)%>%
  mutate(Measure = factor(Measure, levels = other_source_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Other Household Income Percent Change")

m4sc_other_income_percent_change_graph

# Non-Cash Benefits

nc_benefits <- Module_4_Section_C %>%
  filter(Subgroup == "Non-Cash Benefits")%>%
  filter(Year == 2023)%>%
  select(Measure)

nc_benefits_string <- nc_benefits$Measure

m4sc_non_cash_count_graph <- Module_4_Section_C %>%
  filter(Subgroup == "Non-Cash Benefits")%>%
  mutate(Measure = factor(Measure, levels = nc_benefits_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Non-Cash Benefits Sources")

m4sc_non_cash_count_graph


m4sc_nc_23 <- Module_4_Section_C %>%
  filter(Subgroup =="Non-Cash Benefits")%>%
  filter(Year == 2023)%>%
  summarise(Total = sum(Count))

m4sc_nc_total_23 <- m4sc_nc_23$Total

m4sc_nc_total_23

m4sc_nc_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Non-Cash Benefits")%>%
  filter(Year == 2022)%>%
  summarise(Total = sum(Count))

m4sc_nc_total_22 <- m4sc_nc_22$Total

m4sc_nc_total_22

m4sc_ncb_23 <- Module_4_Section_C %>%
  filter(Subgroup == "Non-Cash Benefits")%>%
  filter(Year == 2023)%>%
  mutate(`2023 Percent` = round(100*Count / m4sc_nc_total_23, 2))%>%
  rename("2023 Count" = "Count")%>%
  select(!Year)

m4sc_ncb_22 <- Module_4_Section_C %>%
  filter(Subgroup == "Non-Cash Benefits")%>%
  filter(Year == 2022)%>%
  mutate(`2022 Percent` = round(100*Count / m4sc_nc_total_22, 2))%>%
  rename("2022 Count" = "Count")%>%
  select(!Year)

m4sc_nc_joined <- left_join(m4sc_ncb_23, m4sc_ncb_22, by = c("Group", "Subgroup", "Measure"))

m4sc_nc_joined

m4sc_nc_percent_change <- m4sc_nc_joined %>%
  mutate(`Percent Change` = round(100*(`2023 Count` - `2022 Count`)/ `2022 Count`,2))

long_m4sc_nc_percent_change <- m4sc_nc_percent_change %>%
  pivot_longer(-c("Group", "Subgroup", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")

long_m4sc_nc_percent_change


m4sc_non_cash_percent_graph <- long_m4sc_nc_percent_change %>%
  filter(Type == "Percent")%>%
  mutate(Measure = factor(Measure, levels = nc_benefits_string))%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Non-Cash Benefits Percentages")

m4sc_non_cash_percent_graph


m4sc_non_cash_percent_change_graph <- long_m4sc_nc_percent_change  %>%
  filter(Type == "Change")%>%
  filter(Value != 0)%>%
  mutate(Measure = factor(Measure, levels = nc_benefits_string))%>%
  ggplot(aes(x = Value, y = fct_rev(Measure), fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Non-Cash Benefits Percent Change")


m4sc_non_cash_percent_change_graph


#### outcomes ######## oCountutcomes ####

names(outcomes)

View(outcomes)

long_outcomes <- outcomes %>%
  pivot_longer(-c("Year", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_23 <- long_outcomes %>%
  filter(Year == 2023)%>%
  rename("2023 Result" = "Result")%>%
  select(!Year)

outcomes_22 <- long_outcomes %>%
  filter(Year == 2022)%>%
  rename("2022 Result" = "Result")%>%
  select(!Year)

View(outcomes_23)

View(outcomes_22)

joined_outcomes <- full_join(outcomes_23, outcomes_22, by = c("Domain", "Outcome", "Measure"))

joined_outcomes[is.na(joined_outcomes)] <- 0

View(joined_outcomes)  


#### seperate out by domains ##


### education

outcomes_education_count_graph <- joined_outcomes %>%
  filter(str_detect(Domain, "Education"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  scale_fill_uethda()+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Education Outcomes 2023-2022")
  
outcomes_education_count_graph


joined_outcomes %>%
  filter(str_detect(Domain, "Education"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

edu_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Education"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)
  
joined_edu_with_totals <- full_join(joined_outcomes%>%
                                      filter(str_detect(Domain, "Education")), edu_outcomes_totals)

joined_edu_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
    filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  scale_fill_brewer(palette = "PuOr")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Education Outcomes 2023-2022")
  
education_outcomes_stacked <- joined_edu_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
  scale_fill_brewer(palette = "PuOr")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Education Outcomes 2023-2022 as Percentage of Total")
  
education_outcomes_stacked


education_outcomes_percent_change <- joined_edu_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
  scale_fill_brewer(palette = "PuOr")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Education Outcomes 2023-2022 Percent Change")

education_outcomes_percent_change



#### employment

outcomes_emp_count <- joined_outcomes %>%
  filter(str_detect(Domain, "Employment"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Employment Outcomes 2023-2022")

outcomes_emp_count


joined_outcomes %>%
  filter(str_detect(Domain, "Employment"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

emp_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Employment"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)

joined_emp_with_totals <- full_join(joined_outcomes%>%
                                      filter(str_detect(Domain, "Employment")), emp_outcomes_totals)
joined_emp_with_totals


employment_outcomes_count_graph <- joined_emp_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Paired")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Employment Outcomes 2023-2022")

employment_outcomes_count_graph

employment_outcomes_stacked_graph <- joined_emp_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
  scale_fill_brewer(palette = "Paired")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Employment Outcomes 2023-2022 as Percentage of Total")

employment_outcomes_stacked_graph


employment_outcomes_percent_change_graph <- joined_emp_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
  scale_fill_brewer(palette = "Paired")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Employment Outcomes 2023-2022 Percent Change")

employment_outcomes_percent_change_graph




#### health

joined_outcomes %>%
  filter(str_detect(Domain, "Health and Social/Behavioral Development"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Set1")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Health and Social/Behavioral Development Outcomes 2023-2022")


joined_outcomes %>%
  filter(str_detect(Domain, "Health and Social/Behavioral Development"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

health_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Health and Social/Behavioral Development"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)

joined_health_with_totals <- full_join(joined_outcomes%>%
                                      filter(str_detect(Domain, "Health and Social/Behavioral Development")), health_outcomes_totals)
joined_health_with_totals


health_outcomes_count_graph <- joined_health_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Set1")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Health and Social/Behavioral Development Outcomes 2023-2022")

health_outcomes_count_graph


health_outcomes_stacked_graph <- joined_health_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
  scale_fill_brewer(palette = "Set1")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Health and Social/Behavioral Development Outcomes 2023-2022 as Percentage of Total")

health_outcomes_stacked_graph


health_outcomes_percent_change_graph <- joined_health_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
  scale_fill_brewer(palette = "Set1")+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Health Outcomes 2023-2022 Percent Change")

health_outcomes_percent_change_graph


#### housing

joined_outcomes %>%
  filter(str_detect(Domain, "Housing"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Set3")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Housing Outcomes 2023-2022")


joined_outcomes %>%
  filter(str_detect(Domain, "Housing"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

housing_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Housing"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)

joined_housing_with_totals <- full_join(joined_outcomes%>%
                                         filter(str_detect(Domain, "Housing")), housing_outcomes_totals)
joined_housing_with_totals


housing_outcomes_count_graph <- joined_housing_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "black", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Housing Outcomes 2023-2022")

housing_outcomes_count_graph



housing_outcomes_stacked_graph <- joined_housing_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_stack(0.9))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Housing Outcomes 2023-2022 as Percentage of Total")


housing_outcomes_stacked_graph

housing_outcomes_percent_change <- joined_housing_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_dodge(1))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Housing Outcomes 2023-2022 Percent Change")

housing_outcomes_percent_change


####Income and Asset Building

joined_outcomes %>%
  filter(str_detect(Domain, "Income and Asset Building"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Set3")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Income and Asset Building Outcomes 2023-2022")


joined_outcomes %>%
  filter(str_detect(Domain, "Income and Asset Building"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

income_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Income and Asset Building"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)

joined_income_with_totals <- full_join(joined_outcomes%>%
                                          filter(str_detect(Domain, "Income and Asset Building")), income_outcomes_totals)
joined_income_with_totals


income_outcomes_count_graph <- joined_income_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "black", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Income and Asset Building Outcomes 2023-2022")

income_outcomes_count_graph

income_outcomes_stacked_graph <- joined_income_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_stack(0.9))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Income and Asset Building Outcomes 2023-2022 as Percentage of Total")

income_outcomes_stacked_graph

income_outcomes_percent_change <- joined_income_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_dodge(1))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Income and Asset Building Outcomes 2023-2022 Percent Change")

income_outcomes_percent_change



#### Civic Engagement and Community Involvement

joined_outcomes %>%
  filter(str_detect(Domain, "Civic Engagement and Community Involvement"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  scale_fill_brewer(palette = "Set3")+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "white", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
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
  ggtitle("Civic Engagement and Community Involvement Outcomes 2023-2022")


joined_outcomes %>%
  filter(str_detect(Domain, "Civic Engagement and Community Involvement"))%>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))

civic_outcomes_totals <- joined_outcomes %>%
  filter(str_detect(Domain, "Civic Engagement and Community Involvement"))%>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("2023 Total" = "2023 Result", "2022 Total" = "2022 Result")%>%
  select(!Measure)

joined_civic_with_totals <- full_join(joined_outcomes%>%
                                         filter(str_detect(Domain, "Civic Engagement and Community Involvement")), civic_outcomes_totals)
joined_civic_with_totals


civic_outcomes_count_graph <- joined_civic_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value), position = position_dodge(0.9),color = "black", show.legend = FALSE)+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Civic Engagement and Community Involvement Outcomes 2023-2022")

civic_outcomes_count_graph


civic_outcomes_stacked_graph <- joined_civic_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Year == "2023" |
           Year == "2022")%>%
  filter(Value > 0)%>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Year)+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_stack(0.9))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Civic Engagement and Community Involvement Outcomes 2023-2022 as Percentage of Total")

civic_outcomes_stacked_graph


joined_civic_with_totals %>%
  group_by(Outcome)%>%
  mutate(`2023-2022 Difference` = `2023 Result` - `2022 Result`)%>%
  mutate(`Percent Change` = round(100*(`2023 Result` - `2022 Result`) / `2022 Result`,2))%>%
  mutate(`2023 Percent` = round(100*(`2023 Result` / `2023 Total`),2))%>%
  mutate(`2022 Percent` = round(100*(`2022 Result` / `2022 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Year", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Year == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "black", show.legend = FALSE, position = position_dodge(1))+
  theme(text = element_text("Calibri"))+
  scale_fill_brewer(palette = "Set3")+
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
  ggtitle("Civic Engagement and Community Involvement Outcomes 2023-2022 Percent Change")
