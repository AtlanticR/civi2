library(readxl)
library(writexl)
final <- read_excel("C:/Users/HARBINJ/Downloads/updated_CIVI.xlsx")



indicator_names <- c("ind_sea_level_change_Value","ind_ice_day_change_Value","ind_coastal_sensitivity_index_Value",
                     "ind_harbour_condition_Value","ind_degree_of_protection_Value", "ind_replacement_cost_Value",
                     "ind_harbour_utilization_Value","ind_sch_proximity_Value",
                     "ind_sea_level_change_Score","ind_ice_day_change_Score","ind_coastal_sensitivity_index_Score",
                     "ind_harbour_condition_Score","ind_degree_of_protection_Score", "ind_replacement_cost_Score",
                     "ind_harbour_utilization_Score","ind_sch_proximity_Score", 'vulnw', "CIVI", 'exposure', 'sensitivity', 'adaptive_capacity')

for (i in seq_along(indicator_names)) {
  if (!(indicator_names[i] %in% c('ind_sea_level_change_Value', 'ind_coastal_sensitivity_index_Value','ind_sea_level_change_Score', 'ind_coastal_sensitivity_index_Score'))) {
    final[[indicator_names[i]]][which(is.na(final[[indicator_names[i]]]))] <- 'Data not available'
  } else {
    data_not_applicable <- which(is.na(final[[indicator_names[i]]]) & final$MarineInland == 'Marine')
    if (!(length(data_not_applicable) == 0)) {
      final[[indicator_names[i]]][which(is.na(final[[indicator_names[i]]]) & final$MarineInland == 'Marine')] <- 'Data not Available'
    }

    final[[indicator_names[i]]][which(is.na(final[[indicator_names[i]]]) & final$MarineInland == 'Inland')] <- 'Not applicable'
  }
}

final$ind_sch_proximity_Value[which(final$ind_sch_proximity_Value == "Data not available")] <- 'No SCH locations within 25 km'
final$ind_sch_proximity_Score[which(final$ind_sch_proximity_Score == "Data not available")] <- 'No SCH locations within 25 km'


writexl::write_xlsx(final,"C:/Users/HARBINJ/Downloads/2026-11-02_CIVI.xlsx")

