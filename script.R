#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2023 and Quarter 4 2022")  
    
liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
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
  
outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2023 and Quarter 4 2022")+
theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")
  

outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2023 and Quarter 4 2022")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 4 2022")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 1 2023 and Quarter 4 2022")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 1 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2023 and Quarter 1 2022")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 1 2022")%>%
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
  ggtitle("CSBG Outcomes Quarter 1 2023 and Quarter 1 2022")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 1 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2023 and Quarter 1 2022")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 1 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2023 and Quarter 1 2022")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 1 2022")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 1 2023 and Quarter 1 2022")

commodities_plot_previous_year

#### quarter 2 2023 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 2 2023 and Quarter 1 2023")  

liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 2 2023")%>%
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
  ggtitle("CSBG Outcomes Quarter 2 2023 and Quarter 1 2023")

outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 2 2023 and Quarter 1 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 2 2023 and Quarter 1 2023")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2023"|
           Date == "Quarter 2 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 2 2023 and Quarter 1 2023")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 2 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 2 2023 and Quarter 2 2022")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 2 2022")%>%
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
  ggtitle("CSBG Outcomes Quarter 2 2023 and Quarter 2 2022")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 2 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 2 2023 and Quarter 2 2022")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 2 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 2 2023 and Quarter 2 2022")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022", "Quarter 1 2023", "Quarter 2 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 2 2022")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 2 2023 and Quarter 2 2022")

commodities_plot_previous_year

#### Quarter 3 2023 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 3 2023 and Quarter 2 2023")  

liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 3 2023")%>%
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
  ggtitle("CSBG Outcomes Quarter 3 2023 and Quarter 2 2023")

outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 3 2023 and Quarter 2 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 3 2023 and Quarter 2 2023")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2023"|
           Date == "Quarter 3 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 3 2023 and Quarter 2 2023")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 3 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 3 2023 and Quarter 3 2022")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 3 2022")%>%
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
  ggtitle("CSBG Outcomes Quarter 3 2023 and Quarter 3 2022")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 3 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 3 2023 and Quarter 3 2022")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 3 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 3 2023 and Quarter 3 2022")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 3 2022")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 3 2023 and Quarter 3 2022")

commodities_plot_previous_year

#### Quarter 4 2023 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 4 2023 and Quarter 3 2023")  

liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 4 2023")%>%
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
  ggtitle("CSBG Outcomes Quarter 4 2023 and Quarter 3 2023")

outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 4 2023 and Quarter 3 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 4 2023 and Quarter 3 2023")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2023"|
           Date == "Quarter 4 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 4 2023 and Quarter 3 2023")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 4 2023 and Quarter 4 2022")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  filter(Date == "Quarter 4 2023"|
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
  ggtitle("CSBG Outcomes Quarter 4 2023 and Quarter 4 2022")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 4 2023 and Quarter 4 2022")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 4 2022")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 4 2023 and Quarter 4 2022")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 4 2022")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 4 2023 and Quarter 4 2022")

commodities_plot_previous_year

#### Quarter 1 2023 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2024 and Quarter 4 2023")  

liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 1 2024")%>%
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
  ggtitle("CSBG Outcomes Quarter 1 2024 and Quarter 4 2023")

outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2024 and Quarter 4 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2024 and Quarter 4 2023")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2023"|
           Date == "Quarter 1 2024")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 1 2024 and Quarter 4 2023")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 1 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2024 and Quarter 1 2023")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 1 2023")%>%
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
  ggtitle("CSBG Outcomes Quarter 1 2024 and Quarter 1 2023")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 1 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2024 and Quarter 1 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 1 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2024 and Quarter 1 2023")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 4 2023",
                                        "Quarter 1 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 1 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 1 2024 and Quarter 1 2023")

commodities_plot_previous_year



#### Quarter 2 2024 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 2 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 2 2024 and Quarter 1 2024")  

liheap_plot

#### OUTCOMES ####

names(OUTCOMES)

outcomes <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 2 2024")%>%
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
  ggtitle("CSBG Outcomes Quarter 2 2024 and Quarter 1 2024")

outcomes  

#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 2 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 2 2024 and Quarter 1 2024")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 2 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 2 2024 and Quarter 1 2024")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2024"|
           Date == "Quarter 2 2024")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 2 2024 and Quarter 1 2024")

commodities_plot

#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", angle = 90, hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 2 2024 and Quarter 2 2023")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 2 2023")%>%
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
  ggtitle("CSBG Outcomes Quarter 2 2024 and Quarter 2 2023")

outcomes_previous_year  

#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 2 2024 and Quarter 2 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 2 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 2 2024 and Quarter 2 2023")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 2 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  ggtitle("Commodities Quarter 2 2024 and Quarter 2 2023")

commodities_plot_previous_year



#### Quarter 3 2024 ####
#### heres all this shit again ####

options(scipen = 999)

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 3 2024 and Quarter 2 2024")  

liheap_plot

#### OUTCOMES ####

names(NEW_OUTCOMES)

outcomes

NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("Outcomes Quarter 3 2024 and Quarter 2 2024")

outcomes  

#### new outcomes format ####

#### outcomes ######## oCountutcomes ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter <- long_outcomes %>%
  filter(Date == "Quarter 3 2024")%>%
  rename("Quarter 3 2024 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter <- long_outcomes %>%
  filter(Date == "Quarter 2 2024")%>%
  rename("Quarter 2 2024 Result" = "Result")%>%
  select(!Date)

joined_outcomes <- full_join(outcomes_new_quarter, outcomes_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_outcomes[is.na(joined_outcomes)] <- 0

joined_outcomes


outcomes_totals <- joined_outcomes %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 3 2024 Total" = "Quarter 3 2024 Result", "Quarter 2 2024 Total" = "Quarter 2 2024 Result")%>%
  select(!Measure)

joined_with_totals <- full_join(joined_outcomes, outcomes_totals)

outcomes_count_graph <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_3_2024" |
           Date == "Quarter_2_2024")%>%
  mutate(Date = factor(Date, levels = c("Quarter_2_2024", "Quarter_3_2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 3 2024-Quarter 2 2024")

outcomes_count_graph

outcomes_percent_change <- joined_with_totals %>%
group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 3 2024-Quarter 2 2024")

outcomes_percent_change

outcomes_stacked <- joined_with_totals %>%
group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Previous", "Current")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 3 and Quarter 2 2024 as Percentage of Total")

outcomes_stacked

#### outcomes previous year ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 3 2024")%>%
  rename("Quarter 3 2024 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 3 2023")%>%
  rename("Quarter 3 2023 Result" = "Result")%>%
  select(!Date)

joined_outcomes_year <- full_join(outcomes_new_quarter_year, outcomes_previous_quarter_year, by = c("Domain", "Outcome", "Measure"))

joined_outcomes_year[is.na(joined_outcomes_year)] <- 0

joined_outcomes_year


outcomes_totals_year <- joined_outcomes_year %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 3 2024 Total" = "Quarter 3 2024 Result", "Quarter 3 2023 Total" = "Quarter 3 2023 Result")%>%
  select(!Measure)

joined_with_totals_year <- full_join(joined_outcomes_year, outcomes_totals_year)

outcomes_count_graph_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 3 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 3 2023 Result`) / `Quarter 3 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2023 Result` / `Quarter 3 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_3_2023 Result" = "Quarter 3 2023 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_3_2023 Total" = "Quarter 3 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_3_2024" |
           Date == "Quarter_3_2023")%>%
  mutate(Date = factor(Date, levels = c("Quarter_3_2023", "Quarter_3_2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 3 2024-Quarter 3 2023")

outcomes_count_graph_year


outcomes_percent_change_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 3 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 3 2023 Result`) / `Quarter 3 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2023 Result` / `Quarter 3 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_3_2023 Result" = "Quarter 3 2023 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_3_2023 Total" = "Quarter 3 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 3 2024-Quarter 3 2023")

outcomes_percent_change_year

outcomes_stacked_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 3 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 3 2023 Result`) / `Quarter 3 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2023 Result` / `Quarter 3 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_3_2023 Result" = "Quarter 3 2023 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_3_2023 Total" = "Quarter 3 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Previous", "Current")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 3 2024 and Quarter 3 2023 as Percentage of Total")

outcomes_stacked_year


#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 3 2024 and Quarter 2 2024")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)

wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 3 2024 and Quarter 2 2024")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 3 2024 and Quarter 2 2024")

commodities_plot

#### First Steps ####

first_steps_outcomes <- FIRST_STEPS %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("First Steps Outcomes Quarter 3 2024 and Quarter 2 2024")+
  labs(subtitle =  "There were 3 classes in Quarter 2 2024", fontface = "italic")
  

first_steps_outcomes 

long_first_steps <- FIRST_STEPS %>%
  select(!Program)%>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

first_steps_new_quarter <- long_first_steps %>%
  filter(Date == "Quarter 3 2024")%>%
  rename("Quarter 3 2024 Result" = "Result")%>%
  select(!Date)

first_steps_previous_quarter <- long_first_steps %>%
  filter(Date == "Quarter 2 2024")%>%
  rename("Quarter 2 2024 Result" = "Result")%>%
  select(!Date)

joined_first_steps <- full_join(first_steps_new_quarter, first_steps_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_first_steps[is.na(joined_first_steps)] <- 0

joined_first_steps

first_steps_outcomes_totals <- joined_first_steps %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 3 2024 Total" = "Quarter 3 2024 Result", "Quarter 2 2024 Total" = "Quarter 2 2024 Result")%>%
  select(!Measure)

first_steps_outcomes_totals

joined_first_steps_with_totals <- full_join(joined_first_steps, first_steps_outcomes_totals)

joined_first_steps_with_totals

quarters_labs <- c("Quarter 2 2024", "Quarter 3 2024")
names(quarters_labs) <- c("Quarter 2 2024", "Quarter 3 2024")

quarters_labs

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_3_2024" |
           Date == "Quarter_2_2024")%>%
  mutate(Date = factor(Date, levels = c("Quarter_2_2024", "Quarter_3_2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("First Steps Outcomes Quarter 3 2024-Quarter 2 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("First Steps Outcomes Percent Change Quarter 3 2024-Quarter 2 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 3 2024 Result` - `Quarter 2 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 3 2024 Result` - `Quarter 2 2024 Result`) / `Quarter 2 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 2 2024 Result` / `Quarter 2 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_2_2024 Result" = "Quarter 2 2024 Result",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total",
         "Quarter_2_2024 Total" = "Quarter 2 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Previous", "Current")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("First Steps Outcomes Quarter 3 and Quarter 2 2024 as Percentage of Total")


#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 3 2024 and Quarter 3 2023")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 3 2023")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("CSBG Outcomes Quarter 3 2024 and Quarter 3 2023")

outcomes_previous_year  

##### new outcomes format #####


#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 3 2024 and Quarter 3 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 3 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 3 2024 and Quarter 3 2023")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 3 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 3 2024 and Quarter 3 2023")

commodities_plot_previous_year



#### Quarter 4 2024 ####
#### heres all this shit again ####

options(scipen = 999)

quarters_labs <- c("Quarter 3 2024", "Quarter 4 2024")
names(quarters_labs) <- c("Quarter 3 2024", "Quarter 4 2024")

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 4 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 4 2024 and Quarter 3 2024")  

liheap_plot

#### OUTCOMES ####

names(NEW_OUTCOMES)

outcomes

NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 4 2024")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("Outcomes Quarter 4 2024 and Quarter 3 2024")

outcomes  

#### new outcomes format ####

#### outcomes ######## oCountutcomes ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter <- long_outcomes %>%
  filter(Date == "Quarter 4 2024")%>%
  rename("Quarter 4 2024 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter <- long_outcomes %>%
  filter(Date == "Quarter 3 2024")%>%
  rename("Quarter 3 2024 Result" = "Result")%>%
  select(!Date)

joined_outcomes <- full_join(outcomes_new_quarter, outcomes_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_outcomes[is.na(joined_outcomes)] <- 0

joined_outcomes


outcomes_totals <- joined_outcomes %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 4 2024 Total" = "Quarter 4 2024 Result", "Quarter 3 2024 Total" = "Quarter 3 2024 Result")%>%
  select(!Measure)

joined_with_totals <- full_join(joined_outcomes, outcomes_totals)

outcomes_count_graph <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_4_2024" |
           Date == "Quarter_3_2024")%>%
  filter(Measure != "Total")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 4 2024", "Quarter 3 2024")))%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 4 2024-Quarter 3 2024")



outcomes_count_graph

outcomes_percent_change <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 4 2024-Quarter 3 2024")

outcomes_percent_change

outcomes_stacked <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 4 and Quarter 3 2024 as Percentage of Total")

outcomes_stacked

#### outcomes previous year ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 4 2024")%>%
  rename("Quarter 4 2024 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 4 2023")%>%
  rename("Quarter 4 2023 Result" = "Result")%>%
  select(!Date)

joined_outcomes_year <- full_join(outcomes_new_quarter_year, outcomes_previous_quarter_year, by = c("Domain", "Outcome", "Measure"))

joined_outcomes_year[is.na(joined_outcomes_year)] <- 0

joined_outcomes_year


outcomes_totals_year <- joined_outcomes_year %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 4 2024 Total" = "Quarter 4 2024 Result", "Quarter 4 2023 Total" = "Quarter 4 2023 Result")%>%
  select(!Measure)

joined_with_totals_year <- full_join(joined_outcomes_year, outcomes_totals_year)

outcomes_count_graph_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 4 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 4 2023 Result`) / `Quarter 4 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2023 Result` / `Quarter 4 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_4_2023 Result" = "Quarter 4 2023 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_4_2023 Total" = "Quarter 4 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_4_2024" |
           Date == "Quarter_4_2023")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 4 2024", "Quarter 4 2023")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 4 2024-Quarter 4 2023")

outcomes_count_graph_year


outcomes_percent_change_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 4 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 4 2023 Result`) / `Quarter 4 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2023 Result` / `Quarter 4 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_4_2023 Result" = "Quarter 4 2023 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_4_2023 Total" = "Quarter 4 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 4 2024-2023")

outcomes_percent_change_year

outcomes_stacked_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 4 2023 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 4 2023 Result`) / `Quarter 4 2023 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2023 Result` / `Quarter 4 2023 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_4_2023 Result" = "Quarter 4 2023 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_4_2023 Total" = "Quarter 4 2023 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 4 2024-2023 as Percentage of Total")

outcomes_stacked_year


#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 4 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 4 2024 and Quarter 3 2024")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)


wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 4 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 4 2024 and Quarter 3 2024")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 3 2024")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 3 2024 and Quarter 2 2024")

commodities_plot

#### First Steps ####

first_steps_outcomes <- FIRST_STEPS %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  filter(Date == "Quarter 3 2024"|
           Date == "Quarter 4 2024")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("First Steps Outcomes Quarter 4 2024 and Quarter 3 2024")+
  labs(subtitle =  "Summer Break from classes in Quarter 4 2024", fontface = "italic")


first_steps_outcomes 

long_first_steps <- FIRST_STEPS %>%
  select(!Program)%>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

first_steps_new_quarter <- long_first_steps %>%
  filter(Date == "Quarter 4 2024")%>%
  rename("Quarter 4 2024 Result" = "Result")%>%
  select(!Date)

first_steps_previous_quarter <- long_first_steps %>%
  filter(Date == "Quarter 3 2024")%>%
  rename("Quarter 3 2024 Result" = "Result")%>%
  select(!Date)

joined_first_steps <- full_join(first_steps_new_quarter, first_steps_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_first_steps[is.na(joined_first_steps)] <- 0

joined_first_steps

first_steps_outcomes_totals <- joined_first_steps %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 4 2024 Total" = "Quarter 4 2024 Result", "Quarter 3 2024 Total" = "Quarter 3 2024 Result")%>%
  select(!Measure)

first_steps_outcomes_totals

joined_first_steps_with_totals <- full_join(joined_first_steps, first_steps_outcomes_totals)

joined_first_steps_with_totals

quarters_labs <- c("Quarter 3 2024", "Quarter 4 2024")
names(quarters_labs) <- c("Quarter 3 2024", "Quarter 4 2024")

quarters_labs

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_4_2024" |
           Date == "Quarter_3_2024")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 4 2024", "Quarter 3 2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("First Steps Outcomes Quarter 4 2024-Quarter 3 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("First Steps Percent Change Quarter 4-3 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 4 2024 Result` - `Quarter 3 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 4 2024 Result` - `Quarter 3 2024 Result`) / `Quarter 3 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 3 2024 Result` / `Quarter 3 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_3_2024 Result" = "Quarter 3 2024 Result",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total",
         "Quarter_3_2024 Total" = "Quarter 3 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("First Steps Quarter 4-3 2024 as Percentage of Total")


#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 4 2024 and Quarter 4 2023")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("CSBG Outcomes Quarter 4 2024 and Quarter 4 2023")

outcomes_previous_year  

##### new outcomes format #####


#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 4 2024 and Quarter 4 2023")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023","Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 4 2024 and Quarter 4 2023")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 4 2024 and Quarter 4 2023")

commodities_plot_previous_year


#### Quarter 4 2024 ####
#### heres all this shit again ####

options(scipen = 999)

quarters_labs <- c("Quarter 4 2024", "Quarter 1 2025")
names(quarters_labs) <- c("Quarter 4 2024", "Quarter 1 2025")

#### LIHEAP ####

names(LIHEAP)


liheap_plot <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 1 2025")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white", hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2025 and Quarter 4 2024")  

liheap_plot

#### OUTCOMES ####

names(NEW_OUTCOMES)

outcomes

NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 1 2025")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("Outcomes Quarter 1 2025 and Quarter 4 2024")

outcomes  

#### new outcomes format ####

#### outcomes ######## oCountutcomes ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter <- long_outcomes %>%
  filter(Date == "Quarter 1 2025")%>%
  rename("Quarter 1 2025 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter <- long_outcomes %>%
  filter(Date == "Quarter 4 2024")%>%
  rename("Quarter 4 2024 Result" = "Result")%>%
  select(!Date)

joined_outcomes <- full_join(outcomes_new_quarter, outcomes_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_outcomes[is.na(joined_outcomes)] <- 0

joined_outcomes


outcomes_totals <- joined_outcomes %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 1 2025 Total" = "Quarter 1 2025 Result", "Quarter 4 2024 Total" = "Quarter 4 2024 Result")%>%
  select(!Measure)

joined_with_totals <- full_join(joined_outcomes, outcomes_totals)

outcomes_count_graph <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_1_2025" |
           Date == "Quarter_4_2024")%>%
  filter(Measure != "Total")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2025", "Quarter 4 2024")))%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 1 2025-Quarter 4 2024")



outcomes_count_graph

outcomes_percent_change <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 1 2025-Quarter 4 2024")

outcomes_percent_change

outcomes_stacked <- joined_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 1 2025-Quarter 4 2024 as Percentage of Total")

outcomes_stacked

#### outcomes previous year ####

outcomes <- NEW_OUTCOMES

long_outcomes <- outcomes %>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

outcomes_new_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 1 2025")%>%
  rename("Quarter 1 2025 Result" = "Result")%>%
  select(!Date)

outcomes_previous_quarter_year <- long_outcomes %>%
  filter(Date == "Quarter 1 2024")%>%
  rename("Quarter 1 2024 Result" = "Result")%>%
  select(!Date)

joined_outcomes_year <- full_join(outcomes_new_quarter_year, outcomes_previous_quarter_year, by = c("Domain", "Outcome", "Measure"))

joined_outcomes_year[is.na(joined_outcomes_year)] <- 0

joined_outcomes_year


outcomes_totals_year <- joined_outcomes_year %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 1 2025 Total" = "Quarter 1 2025 Result", "Quarter 1 2024 Total" = "Quarter 1 2024 Result")%>%
  select(!Measure)

joined_with_totals_year <- full_join(joined_outcomes_year, outcomes_totals_year)

outcomes_count_graph_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 1 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 1 2024 Result`) / `Quarter 1 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 1 2024 Result` / `Quarter 1 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_1_2024 Result" = "Quarter 1 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_1_2024 Total" = "Quarter 1 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_1_2025" |
           Date == "Quarter_1_2024")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2025", "Quarter 1 2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("Outcomes Quarter 1 2025-Quarter 1 2024")

outcomes_count_graph_year


outcomes_percent_change_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 1 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 1 2024 Result`) / `Quarter 1 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 1 2024 Result` / `Quarter 1 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_1_2024 Result" = "Quarter 1 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_1_2024 Total" = "Quarter 1 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("Outcomes Percent Change Quarter 1 2025-2024")

outcomes_percent_change_year

outcomes_stacked_year <- joined_with_totals_year %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 1 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 1 2024 Result`) / `Quarter 1 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 1 2024 Result` / `Quarter 1 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_1_2024 Result" = "Quarter 1 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_1_2024 Total" = "Quarter 1 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("Outcomes Quarter 1 2025-2024 as Percentage of Total")

outcomes_stacked_year


#### OUTREACH ####

outreach <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 1 2025")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2025 and Quarter 4 2024")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =" "  , fontface = "italic")


outreach

#### WAP ####

names(WAP)


wap <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 1 2025")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2025 and Quarter 4 2024")

wap

#### COMMODITIES ####

names(Commodities)

commodities_plot <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 1 2025")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 2 2024"|
           Date == "Quarter 1 2024")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 3 2024 and Quarter 2 2024")

commodities_plot

#### First Steps ####

first_steps_outcomes <- FIRST_STEPS %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 1 2025")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("First Steps Outcomes Quarter 1 2025 and Quarter 4 2024")+
  labs(subtitle =  "Summer Break from classes in Quarter 4 2024", fontface = "italic")


first_steps_outcomes 

long_first_steps <- FIRST_STEPS %>%
  select(!Program)%>%
  pivot_longer(-c("Date", "Domain", "Outcome"), values_to = "Result", names_to = "Measure")

first_steps_new_quarter <- long_first_steps %>%
  filter(Date == "Quarter 1 2025")%>%
  rename("Quarter 1 2025 Result" = "Result")%>%
  select(!Date)

first_steps_previous_quarter <- long_first_steps %>%
  filter(Date == "Quarter 4 2024")%>%
  rename("Quarter 4 2024 Result" = "Result")%>%
  select(!Date)

joined_first_steps <- full_join(first_steps_new_quarter, first_steps_previous_quarter, by = c("Domain", "Outcome", "Measure"))

joined_first_steps[is.na(joined_first_steps)] <- 0

joined_first_steps

first_steps_outcomes_totals <- joined_first_steps %>%
  group_by(Outcome)%>%
  filter(Measure == "Total")%>%
  rename("Quarter 1 2025 Total" = "Quarter 1 2025 Result", "Quarter 4 2024 Total" = "Quarter 4 2024 Result")%>%
  select(!Measure)

first_steps_outcomes_totals

joined_first_steps_with_totals <- full_join(joined_first_steps, first_steps_outcomes_totals)

joined_first_steps_with_totals

quarters_labs <- c("Quarter 4 2024", "Quarter 1 2025")
names(quarters_labs) <- c("Quarter 2 2024", "Quarter 1 2025")

quarters_labs

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type != "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Quarter_1_2025" |
           Date == "Quarter_4_2024")%>%
  mutate(Date = str_replace_all(Date, "_", " "))%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2025", "Quarter 4 2024")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(~Date, labeller = labeller(Date = quarters_labs))+
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
  ggtitle("First Steps Outcomes Quarter 1 2025-Quarter 4 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Date == "Percent")%>%
  filter(Value != Inf)%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_dodge(1))+
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
  ggtitle("First Steps Percent Change Quarter 1 2025-Quarter 4 2024")

joined_first_steps_with_totals %>%
  group_by(Outcome)%>%
  mutate(`Previous Difference` = `Quarter 1 2025 Result` - `Quarter 4 2024 Result`)%>%
  mutate(`Percent Change` = round(100*(`Quarter 1 2025 Result` - `Quarter 4 2024 Result`) / `Quarter 4 2024 Result`,2))%>%
  mutate(`Current Percent` = round(100*(`Quarter 1 2025 Result` / `Quarter 1 2025 Total`),2))%>%
  mutate(`Previous Percent` = round(100*(`Quarter 4 2024 Result` / `Quarter 4 2024 Total`),2))%>%
  filter(!is.nan(`Percent Change`))%>%
  rename("Quarter_1_2025 Result" = "Quarter 1 2025 Result",
         "Quarter_4_2024 Result" = "Quarter 4 2024 Result",
         "Quarter_1_2025 Total" = "Quarter 1 2025 Total",
         "Quarter_4_2024 Total" = "Quarter 4 2024 Total")%>%
  pivot_longer(-c("Domain", "Outcome", "Measure"), names_to = c("Date", "Type"), values_to = "Value", names_sep = " ")%>%
  filter(Type == "Percent")%>%
  filter(Type != "Total")%>%
  filter(Date == "Current" |
           Date == "Previous")%>%
  filter(Value > 0)%>%
  mutate(Date = factor(Date, levels = c("Current", "Previous")))%>%
  filter(Measure != "Total")%>%
  ggplot(aes(x = Value, y = Outcome, fill = Measure))+
  geom_bar(stat = "identity")+
  facet_wrap(~Date)+
  geom_label(aes(label = Value),color = "white", show.legend = FALSE, position = position_stack(0.9))+
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
  ggtitle("First Steps Quarter 1 2025-Quarter 4 2024 as Percentage of Total")


#### LIHEAP PREVIOUS YEAR####

names(LIHEAP)


liheap_plot_previous_year <- LIHEAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2025"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Amount, fill = Program))+
  geom_col(position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Program,label = Households), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = Program, label = paste("$",Amount, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",hjust = 0.75)+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size=rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "Navy box is total amount,White box is number of households served", fontface = "italic")+
  ggtitle("LIHEAP Quarter 1 2025 and Quarter 1 2024")  

liheap_plot_previous_year

#### outcomes previous year ####

outcomes_previous_year <- NEW_OUTCOMES %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  filter(Date == "Quarter 1 2025"|
           Date == "Quarter 1 2024")%>%
  mutate(`Yes or Achieved` = Yes + Achieved)%>%
  ggplot(aes(x = `Yes or Achieved`, y = Outcome, fill = Outcome))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~fct_rev(Date))+
  geom_label(aes(group = Outcome, label = `Yes or Achieved`), color = "white")+
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
  ggtitle("CSBG Outcomes Quarter 1 2025 and Quarter 1 2024")

outcomes_previous_year  

##### new outcomes format #####


#### OUTREACH PREVIOUS YEAR####

outreach_previous_year <- OUTREACH %>%
  pivot_longer(-Date, names_to = "Action", values_to = "Count")%>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023" ,"Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  filter(Date == "Quarter 1 2025"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Action))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  geom_label(aes(group = Action, label = Count), position = position_dodge(width = 1), fill = "white")+
  labs(y = " ", x = " ")+
  ggtitle("LIHEAP Outreach Quarter 1 2025 and Quarter 1 2024")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "", fontface = "italic")


outreach_previous_year

#### WAP ####

names(WAP)

wap_previous_year <- WAP %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023","Quarter 4 2023","Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024", "Quarter 1 2025")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 1 2025"|
           Date == "Quarter 1 2024")%>%
  ggplot(aes(x = fct_rev(Date), y = `Total Cost`, fill = County))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = County,label = `Units Completed`), position = position_dodge(width = 1), fill = "white",  hjust = -0.75)+
  geom_label(aes(group = County, label = paste("$",`Total Cost`, sep = "")), position = position_dodge(width = 1), fill = "navy", color = "white",vjust = 3)+
  geom_label(aes(group = County, label = `Units in Progress`), position = position_dodge(width = 1), fill = "maroon", hjust = 0.75, color = "white")+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda()+
  labs(y = " ", x = " ")+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(legend.position = "none")+
  labs(subtitle =  "Navy box is total job cost, White box is number of jobs completed, Maroon box is number of jobs in progress", fontface = "italic")+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  ggtitle("Weatherization Quarter 1 2025 and Quarter 1 2024")

wap_previous_year

#### COMMODITIES ####

names(Commodities)

commodities_plot_previous_year <- Commodities %>%
  mutate(Date = factor(Date, levels = c("Quarter 1 2022", "Quarter 2 2022", "Quarter 3 2022", "Quarter 4 2022","Quarter 1 2023" ,"Quarter 2 2023", "Quarter 3 2023", "Quarter 1 2024",
                                        "Quarter 2 2024", "Quarter 3 2024", "Quarter 4 2024")))%>%
  mutate(County = factor(County, levels = c("UETHDA", "Carter", "Greene", "Hancock", "Hawkins", "Johnson",
                                            "Sullivan", "Unicoi", "Washington")))%>%
  filter(Date == "Quarter 4 2024"|
           Date == "Quarter 4 2023")%>%
  pivot_longer(-c(Date, County), names_to = "Neighbors", values_to = "Count")%>%
  ggplot(aes(x = fct_rev(Date), y = Count, fill = Neighbors))+
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  facet_wrap(~County, scales = "free_y")+
  geom_label(aes(group = Neighbors, label = Count), fill = "white", position = position_dodge(width = 1))+
  theme(text = element_text("Calibri"))+
  scale_fill_uethda(name = "")+
  labs(y = " ", x = " ")+
  theme(plot.title = element_text(size = rel(2.25)))+
  theme(plot.title = element_text(face = "bold"))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(plot.subtitle = element_text(face = "italic"))+
  theme(strip.text.x = element_text(size = rel(1.5)))+
  theme(strip.text.x = element_text(face = "bold"))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.text = element_text(face = "bold"))+
  labs(subtitle =  "There were 2 distributions in Quarter 3 2024", fontface = "italic")+
  ggtitle("Commodities Quarter 4 2024 and Quarter 4 2023")

commodities_plot_previous_year
