


library(dplyr)
library(tidyverse)
library(scales)

library(readr)
Electricity_generation_by_age_of_reactor <- read_csv("~/R Scripts/Excel Files/Nuclear Energy Dataset/Electricity generation by age of reactor.csv")
View(Electricity_generation_by_age_of_reactor)

options(scipen = 999)



# Who had the highest electricity generation per type of reactor 2025?

Electricity_generation <- Electricity_generation_by_age_of_reactor |> 
   filter(Year == "2025") |>
    select("Type", `Electricity produced MWh`) |>
     mutate(`Electricity produced MWh` = as.numeric(str_remove_all(`Electricity produced MWh`," "))) |>
      arrange(desc(`Electricity produced MWh`))

ggplot(Electricity_generation, aes(x = reorder(Type, `Electricity produced MWh`), y = `Electricity produced MWh`, fill = Type)) +                                    
                                      geom_col(show.legend = FALSE, width = 0.7) +
                                          labs(x = "Type of Reactor", y = "Production MWh") +
                                            theme_minimal(base_size = 13) +  scale_y_continuous(labels = comma) 
                                              
                                                 

ggsave("productionbyreactor.png")







# What about total electricity generation since 1969?
Electricity_generation <- Electricity_generation_by_age_of_reactor |>
 select("Type", `Electricity produced MWh`) |>
  mutate(`Electricity produced MWh` = as.numeric(str_remove_all(`Electricity produced MWh`, " "))) |>
   mutate(`Electricity produced MWh` = replace_na(`Electricity produced MWh`, 0)) |>
   group_by(Type) |>
    summarise(Total_Production = sum(`Electricity produced MWh`)) |>
     arrange(desc(Electricity_generation$Total_Production))

ggplot(Electricity_generation,aes(x = reorder(Type, Total_Production), y = Total_Production, fill = Type)) +
  geom_col(show.legend = FALSE, width = 0.7) + labs(x = "Type of Reactor", y = "Total Production (MWh)") +
    theme_minimal() + scale_y_continuous(labels = comma)

ggsave("TotalProduction.png")
  






# What years were the highest electricity production?
Electricity_generation <- Electricity_generation_by_age_of_reactor
  Electricity_generation <- Electricity_generation |> select("Year", Type,`Electricity produced MWh`) |>
    arrange(desc(`Electricity produced MWh`)) |>
     mutate(`Electricity produced MWh` = as.numeric(str_remove_all(`Electricity produced MWh`, " "))) |>
       arrange(desc(`Electricity produced MWh`)) |>
        slice_max(`Electricity produced MWh`, n = 5)
  
  
  ggplot(Electricity_generation,aes(x = reorder(Year, `Electricity produced MWh`), y = `Electricity produced MWh`, fill = Type)) +
           geom_col(show.legend = FALSE, width = 0.7) + theme_minimal() + 
             labs(x = "Year", y = "Total Production(MWh)") + scale_y_continuous(labels = comma)
  
  
  ggsave("TotalProductionYears1.png")
  
  
  
 


# Who has the highest operation capacity?

options(scipen = 999) # This allows for digits instead of scientific notation.

Operated_Capacity <- Electricity_generation_by_age_of_reactor
  Operated_Capacity <- Operated_Capacity |> filter(Year =="2025") |>
    select("Type", "Operated capacity MW(e)") |>
      mutate(`Operated capacity MW(e)` = as.numeric(str_remove_all(`Operated capacity MW(e)`, " ")))

ggplot(Operated_Capacity, aes(x = reorder(Type, `Operated capacity MW(e)`), y = `Operated capacity MW(e)`, fill = Type)) +
                                    geom_col(show.legend = FALSE, width = 0.7) + labs(x="Type of Reactor", y = "Operation Capacity") + 
                                                     theme_minimal() + scale_y_continuous(labels = label_comma())
ggsave("operatedcapacity.png")









# Correlation between operated capacity and electricity generated:


Electricity_generation <- Electricity_generation_by_age_of_reactor |>
  mutate(`Oper_Capac` = as.numeric(str_remove_all(`Operated capacity MW(e)`, " "))) |> 
    mutate(`elec_prod` = as.numeric(str_remove_all(`Electricity produced MWh`, " ")))
 

cor(Electricity_generation$Oper_Capac, Electricity_generation$elec_prod, use = "complete.obs")

# The correlation is .995


# Correlation graph?

ggplot(Electricity_generation, aes(Oper_Capac, elec_prod)) +
  geom_point(size = 2, color = "black") + geom_smooth() + 
    labs(x = "Capacity(MWh)", y = "Production(MWh)") + theme_classic() +
      scale_y_continuous(labels = comma) + scale_x_continuous(label = comma)

ggsave("corgraph1.png")








# Creating a nuclear share bar chart


Nuclear_share_of_electrcity <- read_csv("~/R Scripts/Excel Files/Nuclear Energy Dataset/Nuclear share of electrcity.csv")
View(Nuclear_share_of_electrcity)


nuclear_shares <- Nuclear_share_of_electrcity 
nuclear_shares <- nuclear_shares |> 
  select("Country", "Nuclear share %") |>
   
ggplot(nuclear_shares, aes(x = `Nuclear share %`, y = Country, fill = Country)) +
  geom_col(show.legend = FALSE, width = 0.7) +
    labs(x = "Nuclear share %", y = "Country") + 
       theme_minimal()
 
ggsave("Nuclearshare.png")

# which reactor has the highest EAF?



  
  
  
  

  
    

# Average Availability factor per reactor
    
Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
    filter(Type == "BWR") |>
     mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))
  
  
  mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# BWR = 76.14%
  
  Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
    filter(Type == "FBR") |>
      mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

  mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# FBR = 59.39% 
  
  Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
     filter(Type == "GCR") |>
      mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

   mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# GCR = 68.63%

Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
  filter(Type == "HTGR") |>
   mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# HTGR = 38.10%

Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
  filter(Type == "LWGR") |>
  mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# LWGR = 65.38%

Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
  filter(Type == "PHWR") |>
  mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# PHWR = 74.43%

Electricity_generation_filtered <- Electricity_generation_by_age_of_reactor |>
  filter(Type == "PWR") |>
  mutate(`Energy availability factor (EAF)` = parse_number(as.character(`Energy availability factor (EAF)`)))

mean(Electricity_generation_filtered$`Energy availability factor (EAF)`)
# PWR = 76.56%




# creating a bar chart for the averages

Reactor_Dataframe <- data.frame(Reactor = c("BWR", "FBR", "GCR", "HTGR", "LWGR", "PHWR", "PWR"), 
                    Avg = c(76.14, 59.39, 68.63, 38.10, 65.38, 74.43, 76.56))

ggplot(Reactor_Dataframe, aes(x = Reactor, y = Avg, fill = Reactor)) +
  geom_col(show.legend = FALSE) + 
   labs(x = "Reactor", y = "Percentage(%)") + 
    theme_minimal()

ggsave("EAFaveraage.png")


























       