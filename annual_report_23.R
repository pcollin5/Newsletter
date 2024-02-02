#### annual report ####


#### Module_4_Section_B
names(Module_4_Section_B)

View(Module_4_Section_B)

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




#### Module_4_Section_C
names(Module_4_Section_C)

View(Module_4_Section_C)


Module_4_Section_C %>%
  mutate(Year = factor(Year, levels = c("2022", "2023")))%>%
  group_by(Year, `Subgroup`)%>%
  mutate(`Subgroup Count` = sum(Count))%>%
  ggplot(aes(x = `Subgroup Count`, y = `Subgroup`, fill = Group))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~Year)+
  geom_label(aes(group = `Subgroup`, label = `Subgroup Count`), color = "white")+
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year by Program Group")

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




Module_4_Section_C %>%
  filter(Subgroup == "Gender"|
           Subgroup == "Age")%>%
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
  ggtitle("Module 4 Section C 2021-2022 and 2022-2023 Program Year Characteristics: Age and Gender")






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