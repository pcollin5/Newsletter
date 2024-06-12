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

