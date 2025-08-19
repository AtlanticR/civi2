if(!require(librarian)) install.packages("librarian")
pkgs <- c("targets",
          "sf",
          "stars",
          "qs",
          "qs2",
          "dplyr")
shelf(pkgs)
tar_option_set(packages = pkgs)

if(dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2")){
  store = "//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2"
} else if(dir.exists("/srv/sambashare/CIVI/civi2")){
  store = "/srv/sambashare/CIVI/civi2"
} else {
  warning("CIVI data store not found. Please check the directory paths.")
  store = getwd()
}

tar_config_set(store = store)



list(
  tar_target(data_CIVI_Sites,
             command={
               NULL
             }),

  # Indicators

  tar_target(ind_coastal_sensitivity_index,
             command={
               NULL
             }),

  tar_target(ind_harbour_condition,
             command={
               NULL
             }),

  tar_target(ind_degree_of_protection,
             command={
               NULL
             }),

  tar_target(ind_sea_level_change,
             command={
               NULL
             }),

  tar_target(ind_ice_day_change,
             command={
               NULL
             }),

  tar_target(ind_replacement_cost,
             command={
               NULL
             }),

  tar_target(ind_harbour_utilization,
             command={
               NULL
             }),

  tar_target(ind_proximity,
             command={
               NULL
             }),

  # Components
  tar_target(comp_sensitivity,
             command={
               NULL
             }),

  tar_target(comp_exposure,
             command={
               NULL
             }),

  tar_target(comp_adaptive_capacity,
             command={
               NULL
             }),

  # CIVI

  tar_target(CIVI,
             command={
               NULL
             })
)
