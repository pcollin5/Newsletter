`Quarter 3 2022` <- Quarter_3_2022

names(`Quarter 3 2022`) <- c("Households", "Head of Household","Household Size", "County", "Program", "Service", "Cost of Service")

`Quarter 3 2022` <- `Quarter 3 2022` %>%
  mutate(Households = 1)

  table(`Quarter 3 2022`$`Head of Household`)

#### THIS WORKS FOR MAKING THE ACTIVITY SUMMARY REPORT CORRECT ####  
  
`Quarter 3 2022` %>%
  group_by(Program, `Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Service Cost by Household` = sum(`Cost of Service`))%>%
  unique()%>%
  ungroup()%>%
  group_by(Program)%>%
  mutate(`Service Total` = sum(`Total Service Cost by Household`),
         number_of_households = 1,
         `Households Served` = sum(number_of_households),
         `Individuals Served` = sum(`Household Size`))%>%
  filter(str_detect(Program, "WALMART"))

#### xxxxx####  
    
  
`Quarter 3 2022` %>%
  group_by(Program, Service, `Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Service Cost by Household` = sum(`Cost of Service`))%>%
  unique()




#### csbg board numbers ####

`Quarter 4 2022` <- Quarter_4_2022

names(`Quarter 4 2022`)

names(`Quarter 4 2022`) <- c("Households", "Head of Household","Household Size", "County", "Program", "Service", "Cost of Service")


`Quarter 4 2022` <- `Quarter 4 2022` %>%
  mutate(Households = 1)

`Quarter 4 2022` %>%
  group_by(Program)%>%
  summarise(sum(Households), sum(`Household Size`), sum(`Cost of Service`))
