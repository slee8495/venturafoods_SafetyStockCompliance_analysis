library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(openxlsx)
library(reshape2)
library(lubridate)
library(gt)
library(timetk)


# Functions

vignette("tibble")
options(scipen = 100000000)

###### Reading Planner Category ###### 

Planner_Category_SSC <- read_excel("C:/Users/SLee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/Safety Stock Compliance/Planner Category - Safety Stock Compliance.xlsx")

names(Planner_Category_SSC) <- str_replace_all(names(Planner_Category_SSC), c(" " = "_"))

Planner_Category_SSC %>% 
  dplyr::select(4:5) -> Planner_Category

Planner_Category %>% 
  dplyr::mutate(Planners = gsub(" ", "", Planners)) %>% 
  dplyr::mutate(Planners = gsub("Â", "", Planners)) %>%
  dplyr::mutate(Planners = gsub("MMullins", "MMULLINS", Planners)) -> Planner_Category
                
save(Planner_Category, file = "Planner_Category.RData")
                
                
###### Reading SS Data ###### 
                
SS_Data <- read_excel("C:/Users/SLee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/Safety Stock Compliance/SS_data_forR.xlsx")

names(SS_Data) <- str_replace_all(names(SS_Data), c(" " = "_"))
names(SS_Data) <- str_replace_all(names(SS_Data), c("-" = "_"))


SS_Data %>% 
  dplyr::mutate(Planner_Name = gsub("LA ROSA, GREG", "LAROSA, GREG", Planner_Name)) %>% 
  dplyr::mutate(Planner_Name = gsub("Mullins, Mary Ellen", "MULLINS, MaryEllen", Planner_Name)) %>% 
  dplyr::mutate(Planner_Name = gsub("MIHARA, NAOMI N.", "MIHARA, NAOMI", Planner_Name)) %>% 
  dplyr::mutate(Planner_Name = gsub("DUENASÂ, PAULA", "DUENAS, PAULA", Planner_Name)) -> SS_Data
                
save(SS_Data, file = "SS_Data.RData")

### SS_Data - Planner name data wrangling ###

SS_Data %>% 
  tidyr::separate(Planner_Name, c("Last_Name", "First_Name")) %>% 
  dplyr::mutate(Last_Name = gsub("Mullins", "MULLINS", Last_Name)) %>% 
  dplyr::mutate(First_Name = ifelse(Last_Name == "MCALOON", "MCALOON", First_Name)) %>% 
  dplyr::mutate(Planners = paste0(stringr::str_sub(First_Name, 1, 1), Last_Name)) %>% 
  dplyr::relocate(Planners, .after = "First_Name") %>% 
  dplyr::left_join(Planner_Category, by = "Planners")-> SS_Data_2


SS_Data_2 %>% 
  dplyr::filter(!is.na(Planner_Category)) -> SS_Data_3


SS_Data_3 %>% 
  dplyr::filter(Planner_Category != "Eliminate") -> SS_Data_3


SS_Data_3 -> wrangled_SS_Data

colnames(wrangled_SS_Data)[20] <- "Safety_Stock"
colnames(wrangled_SS_Data)[21] <- "Balance_Usable"
colnames(wrangled_SS_Data)[29] <- "Current_SS_Alert"
colnames(wrangled_SS_Data)[39] <- "SKU_greaterthan_SS"


wrangled_SS_Data %>% 
  dplyr::mutate(Month = lubridate::month(Date)) %>% 
  dplyr::mutate(Year  = lubridate::year(Date)) %>% 
  dplyr::mutate(year_month = paste(Year, "-", Month)) %>% 
  dplyr::mutate(year_month = gsub(" ", "", year_month)) %>% 
  dplyr::filter(year_month %in% c("2021-10", "2021-11", "2021-12", "2022-1", "2022-2", "2022-3")) -> wrangled_SS_Data



wrangled_SS_Data

wrangled_SS_Data %>% 
  dplyr::filter(Current_SS_Alert != "N/A") %>% 
  dplyr::mutate(Current_SS_Alert = gsub(" ", "_", Current_SS_Alert)) %>% 
  dplyr::mutate(current_ss_alert_dummy = ifelse(Current_SS_Alert == "Below_SS", 1, 0)) %>%
  dplyr::mutate(current_ss_total_dummy = 1) %>% 
  dplyr::relocate(current_ss_alert_dummy, .after = Current_SS_Alert) %>% 
  dplyr::relocate(current_ss_total_dummy, .after = current_ss_alert_dummy) -> wrangled_SS_Data
              

wrangled_SS_Data %<>% 
  dplyr::mutate(SS_Compliance_percent = SKU_greaterthan_SS / SKU_has_SS)


save(wrangled_SS_Data, file = "wrangled_SS_Data.RData")


########## Data Visualization ##########
Total_Overview
Total_Overview_gt
Total_Overview_bar

Monthly_Trend
Monthly_Trend_gt
Monthly_Trend_line

percent_of_ss_compliance_category_level
percent_of_ss_compliance_category_level_gt
percent_of_ss_compliance_category_level_graph

percent_of_ss_compliance_planner_level_SC
percent_of_ss_compliance_planner_level_SC_gt
percent_of_ss_compliance_planner_level_SC_graph

percent_of_ss_compliance_planner_level_Plants
percent_of_ss_compliance_planner_level_Plants_gt
percent_of_ss_compliance_planner_level_Plants_graph



# Total Overview
wrangled_SS_Data  %>% 
  dplyr::group_by(Planner_Category) %>% 
  dplyr::summarise(below_ss = sum(current_ss_alert_dummy), 
                   total = sum(current_ss_total_dummy), 
                   ok = total - below_ss, 
                   percent_of_below_ss = below_ss / total) %>% 
  dplyr::relocate(Planner_Category, below_ss, ok, total) -> Total_Overview

Total_Overview %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md("__Total Overview__")) -> Total_Overview_gt

Total_Overview %>%
  dplyr::select(-percent_of_below_ss) %>% 
  tidyr::pivot_longer(2:4, names_to = "Attribute", values_to = "value") %>% 
  ggplot2::ggplot(aes(x = Planner_Category, y = value, fill = Attribute)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::theme_minimal() +
  ggplot2::geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.25) +
  ggplot2::labs(title = "Total Overview Bar Graph", x = "") -> Total_Overview_bar


# Monthly Trend


wrangled_SS_Data %>% 
  dplyr::group_by(year_month, Planner_Category) %>% 
  dplyr::summarise(number_of_below_ss = sum(current_ss_alert_dummy), 
                   number_of_total = sum(current_ss_total_dummy), 
                   percent_of_below_ss = number_of_below_ss / number_of_total) %>% 
  dplyr::arrange(Planner_Category) -> Monthly_Trend 

Monthly_Trend %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md("__Monthly Trend__")) -> Monthly_Trend_gt


Monthly_Trend %>%
  ggplot2::ggplot(aes(x = year_month, y = percent_of_below_ss, color = Planner_Category, group = Planner_Category)) +
  ggplot2::geom_line() +
  coord_cartesian(ylim=c(0.1, 0.175)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Monthly Trend - Percent of Below SS by Planner Category", x = "") -> Monthly_Trend_line



# % of SS Compliance - Category Level

wrangled_SS_Data %>% 
  dplyr::group_by(Planner_Category, year_month) %>% 
  dplyr::summarise(test = sum(SS_Compliance_percent), total_test = sum(current_ss_total_dummy), percent_test = test/total_test) %>% 
  dplyr::select(Planner_Category, year_month, percent_test) %>% 
  tidyr::pivot_wider(names_from = year_month, values_from = percent_test) -> percent_of_ss_compliance_category_level

percent_of_ss_compliance_category_level %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md("__% of SS Compliance - Category Level__")) %>% 
  gt::fmt_percent(1:ncol(percent_of_ss_compliance_category_level)) -> percent_of_ss_compliance_category_level_gt

percent_of_ss_compliance_category_level %>% 
  tidyr::pivot_longer(2:ncol(percent_of_ss_compliance_category_level), names_to = "year_month", values_to = "percent_of_ss_compliance") %>%
  ggplot2::ggplot(aes(x = year_month, y = percent_of_ss_compliance, fill = Planner_Category)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::facet_wrap(facets = ~ Planner_Category,
                      scales = "free_y",
                      ncol = 1)+
  ggplot2::coord_cartesian(ylim = c(0.75, 0.9)) +
  ggplot2::theme_light() +
  ggplot2::labs(title = "% of SS Compliance - Planner Category Lavel", xlab = "")-> percent_of_ss_compliance_category_level_graph


  

# % of SS Compliance - Planner Level (SC)
wrangled_SS_Data %>%
  dplyr::filter(Planner_Category == "SC") %>% 
  dplyr::group_by(Planner_Category, Planners, year_month) %>% 
  dplyr::summarise(test = sum(SS_Compliance_percent), total_test = sum(current_ss_total_dummy), percent_test = test/total_test) %>% 
  dplyr::select(Planner_Category, year_month, percent_test) %>% 
  tidyr::pivot_wider(names_from = year_month, values_from = percent_test) -> percent_of_ss_compliance_planner_level_SC

percent_of_ss_compliance_planner_level_SC %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md("__% of SS Compliance - Planner Level - SC__")) %>% 
  gt::fmt_percent(1:ncol(percent_of_ss_compliance_planner_level_SC)) -> percent_of_ss_compliance_planner_level_SC_gt

percent_of_ss_compliance_planner_level_SC %>% 
  tidyr::pivot_longer(3:ncol(percent_of_ss_compliance_planner_level_SC), names_to = "year_month", values_to = "percent_of_ss_compliance") %>%
  ggplot2::ggplot(aes(x = year_month, y = percent_of_ss_compliance, fill = Planners)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::facet_wrap(facets = ~ Planners,
                      scales = "free_y",
                      ncol = 5)+
  ggplot2::coord_cartesian(ylim = c(0.5, 1)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(title = "% of SS Compliance - Planner Lavel - SC", xlab = " ") -> percent_of_ss_compliance_planner_level_SC_graph


# % of SS Compliance - Planner Level (Plants)
wrangled_SS_Data %>%
  dplyr::filter(Planner_Category == "Plants") %>% 
  dplyr::group_by(Planner_Category, Planners, year_month) %>% 
  dplyr::summarise(test = sum(SS_Compliance_percent), total_test = sum(current_ss_total_dummy), percent_test = test/total_test) %>% 
  dplyr::select(Planner_Category, year_month, percent_test) %>% 
  tidyr::pivot_wider(names_from = year_month, values_from = percent_test) -> percent_of_ss_compliance_planner_level_Plants

percent_of_ss_compliance_planner_level_Plants %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md("__% of SS Compliance - Planner Level - Platns__")) %>% 
  gt::fmt_percent(1:ncol(percent_of_ss_compliance_planner_level_Plants)) -> percent_of_ss_compliance_planner_level_Plants_gt

percent_of_ss_compliance_planner_level_Plants %>% 
  tidyr::pivot_longer(3:ncol(percent_of_ss_compliance_planner_level_Plants), names_to = "year_month", values_to = "percent_of_ss_compliance") %>%
  ggplot2::ggplot(aes(x = year_month, y = percent_of_ss_compliance, fill = Planners)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::facet_wrap(facets = ~ Planners,
                      scales = "free_y",
                      ncol = 5)+
  ggplot2::coord_cartesian(ylim = c(0.5, 1)) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(title = "% of SS Compliance - Planner Lavel - Plants", xlab = " ") -> percent_of_ss_compliance_planner_level_Plants_graph


