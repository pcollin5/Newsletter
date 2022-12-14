#### Quarter 3 2022 ####


`Quarter 3 2022` <- Quarter_3_2022

names(`Quarter 3 2022`) <- c("Households", "Head of Household","Household Size", "County", "Program", "Service", "Cost of Service")

`Quarter 3 2022` <- `Quarter 3 2022` %>%
  mutate(Households = 1)

#### THIS WORKS FOR MAKING THE ACTIVITY SUMMARY REPORT CORRECT ####  
  
program_totals <- `Quarter 3 2022` %>%
  group_by(Program, `Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Service Cost by Household` = sum(`Cost of Service`))%>%
  unique()%>%
  ungroup()%>%
  group_by(Program)%>%
  mutate(`Service Total` = sum(`Total Service Cost by Household`),
         number_of_households = 1,
         `Households Served` = sum(number_of_households),
         `Individuals Served` = sum(`Household Size`))%>%
  select(Program, `Households Served`, `Individuals Served`, `Service Total`)%>%
    unique()

UETHDA_totals <- program_totals %>%
  ungroup()%>%
  select(`Households Served`, `Individuals Served`, `Service Total`)%>%
  summarise(sum(`Households Served`), sum(`Individuals Served`), sum(`Service Total`))
  
program_table <- table_function(program_totals)  
  
#### Cost per Individual and Households ####  
    
  
spending_per_household <- `Quarter 3 2022` %>%
  group_by(Program, Service, `Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Service Cost by Household` = sum(`Cost of Service`))%>%
  unique()


cost_per_household_ind <- spending_per_household %>%
  mutate(`Cost per Household` = round(`Total Service Cost by Household` / length(`Head of Household`),2),
         `Cost per Person` = round(`Total Service Cost by Household` / `Household Size`,2))

program_cost_per_hh_ind <- program_totals %>%
  group_by(Program)%>%
  mutate(`Program Cost per HH` = round(`Service Total` / `Households Served`,2),
         `Program Cost per Neighbor` = round(`Service Total`/`Individuals Served`,2))

#### costs per service ####
  

cost_by_program_by_service <- `Quarter 3 2022` %>%
  group_by(Program, Service ,`Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Cost by Household by Program per Service` = sum(`Cost of Service`))%>%
  unique()%>%
  ungroup()%>%
  group_by(Program, Service)%>%
  mutate(`Program Service Total` = sum(`Total Cost by Household by Program per Service`),
         number_of_households = 1,
         `Households Served` = sum(number_of_households),
         `Individuals Served` = sum(`Household Size`))%>%
  select(Program, `Households Served`, `Individuals Served`, `Program Service Total`)%>%
  unique()

cost_by_program_by_service_per_hh_and_ind <- cost_by_program_by_service %>%
  group_by(Program, Service)%>%
  mutate(`Program Service Cost per HH` = round(`Program Service Total` / `Households Served`,2),
         `Program Service Cost per Neighbor` = round(`Program Service Total` / `Individuals Served`,2))



cost_by_service <- `Quarter 3 2022` %>%
  group_by(Service ,`Head of Household`)%>%
  summarise(`Head of Household`,`Household Size`,`Total Cost by Household per Service` = sum(`Cost of Service`))%>%
  unique()%>%
  ungroup()%>%
  group_by(Service)%>%
  mutate(`Service Total` = sum(`Total Cost by Household per Service`),
         number_of_households = 1,
         `Households Served` = sum(number_of_households),
         `Individuals Served` = sum(`Household Size`))%>%
  select(Service, `Households Served`, `Individuals Served`, `Service Total`)%>%
  unique()


cost_by_service_by_hh_and_ind <- cost_by_service %>%
  mutate(`Service Cost per HH` = round(`Service Total` / `Households Served`,2),
         `Service Cost per Neighbor` = round(`Service Total` / `Individuals Served`,2))



#### make it so utilities, housing, etc is grouped into seperate domains ####




cost_by_service$Service








#### csbg board numbers ####

`Quarter 4 2022` <- Quarter_4_2022

names(`Quarter 4 2022`)

names(`Quarter 4 2022`) <- c("Households", "Head of Household","Household Size", "County", "Program", "Service", "Cost of Service")


`Quarter 4 2022` <- `Quarter 4 2022` %>%
  mutate(Households = 1)

`Quarter 4 2022` %>%
  group_by(Program)%>%
  summarise(sum(Households), sum(`Household Size`), sum(`Cost of Service`))
