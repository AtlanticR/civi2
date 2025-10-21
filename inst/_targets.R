if(!require(librarian)) install.packages("librarian")
pkgs <- c("AtlanticR/civi2",
          "targets",
          "readxl",
          "sf",
          "stars",
          "ncmeta",
          "qs",
          "qs2",
          "dplyr",
          "purrr",
          "leaflet")
shelf(pkgs)
tar_option_set(packages = basename(pkgs))

# Set the store path for targets
if(dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2")){
  store = "//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2"
} else if(dir.exists("/srv/sambashare/CIVI/civi2")){
  store = "/srv/sambashare/CIVI/civi2"
} else {
  warning("CIVI data store not found. Please check the directory paths.")
  store = getwd()
}

tar_config_set(store = store)



# Define the targets pipeline
list(
  # Raw Data
  tar_target(data_CIVI_Sites,
             command={
               # file from Yanice Berkane (SCH)
               file <- read_excel(file.path(store, "data", "2025-08-19 - SCH Harbours - Latitude and Longitude.xlsx")) |>
                 filter(!is.na(Latitude) & !is.na(Longitude))

               x <- data.frame(HarbourCode=file$`Harb Code`, HarbourName=file$`Harb Name`, Administration=file$`Harb Responsibility`, Province=file$`Harbour Province Name`,
                               HarbourType=file$`Harb Type Desc`, MarineInland="Marine", Lat=file$Latitude, Long=file$Longitude, Zone=file$`Harbour Region Name`)|>
                 sf::st_as_sf(coords = c("Long", "Lat"),
                              crs = 4326,
                              remove = FALSE)
               x

             }),

  tar_target(data_sea_level,
             command={
               # the download url is the HTTPServer link from the link sent by Carrington Pomeroy (ECCC)
               # https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/catalog/birdhouse/disk2/cccs_portal/indices/Final/SEA_LEVEL_RISE/catalog.html?dataset=birdhouse/disk2/cccs_portal/indices/Final/SEA_LEVEL_RISE/Decadal_CMIP6_ensemble-percentiles_allssps_2020-2150_rslc_uplift_YS.nc
               download_url <- "https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/fileServer/birdhouse/disk2/cccs_portal/indices/Final/SEA_LEVEL_RISE/Decadal_CMIP6_ensemble-percentiles_allssps_2020-2150_rslc_uplift_YS.nc"

               # Download the file
               temp_file <- tempfile(fileext = ".nc")
               download.file(download_url, temp_file, mode = "wb")

               read_ncdf(temp_file,var ="ssp245_rslc_p50") |>
                 slice("time",9) |> # should be 2100
                 st_set_crs(4326)
             }),

  tar_target(data_CIVI_ReplacementCost,
             command={
               # file from Annie Boudreau (SCH)
               read_excel(file.path(store, "data", "2025-08-05 - CIVI Replacement Cost.xlsx"))
             }),

  tar_target(data_CIVI_HarbourCondition,
             command={
               # file from Annie Boudreau (SCH)
               read_excel(file.path(store, "data", "2025-08-05 - CIVI Harbour Condition use and capacity.xlsx"))
             }),


  tar_target(data_ice_days,
             command={
               # data from:
               # https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/catalog/datasets/simulations/bias_adjusted/cmip6/climatedata_ca/CanDCS-M6/ensemble_members/30yAvg/catalog.html?dataset=datasets/simulations/bias_adjusted/cmip6/climatedata_ca/CanDCS-M6/ensemble_members/30yAvg/annual_CanDCS-M6_climindices_ensemble_members_30yAvg.ncml
               # THREDDS OPeNDAP URL for the climate indices dataset
               url <- "https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/dodsC/datasets/simulations/bias_adjusted/cmip6/climatedata_ca/CanDCS-M6/ensemble_members/30yAvg/annual_CanDCS-M6_climindices_ensemble_members_30yAvg.ncml"

               ssp245_ice_days <- read_ncdf(url, var = "ssp245_ice_days")

               ssp245_ice_days_1971 <- st_apply(
                 ssp245_ice_days[,,,, 3],
                 c("lon", "lat"),
                 mean,
                 na.rm = TRUE
               ) |>
                 st_as_stars()

               ssp245_ice_days_2071 <- st_apply(
                 ssp245_ice_days[,,,, 13],
                 c("lon", "lat"),
                 mean,
                 na.rm = TRUE
               ) |>
                 st_as_stars()

               ice_days_delta <- ssp245_ice_days_2071 - ssp245_ice_days_1971

               ice_days_delta
             }),

  # Coastal Sensitivity Index
  tar_target(data_CanCoast2.0,
             command = {
               #download zip from https://ostrnrcan-dostrncan.canada.ca/entities/publication/caff0f1b-6adb-4470-be2c-d4461cf29793
               # Manson, G. K., Couture, N. J. & James, T. S. (2019). CanCoast 2.0: data and indices to describe the sensitivity of Canada's marine coasts to changing climate. Geological Survey of Canada, Open File, 8551, 18. https://doi.org/10.4095/314669
               url <- "https://geoscan.nrcan.gc.ca/download/gid_314669.zip?_gl=1*1c0ko2t*_ga*MTA3MTYyNTAwNy4xNzE5MzI3MTk4*_ga_C2N57Y7DX5*MTcyMzE0MjcwMi42LjEuMTcyMzE0MjcxOS4wLjAuMA.."
               destfile <- file.path(tempdir(),"gid_314669.zip")
               if (!file.exists(destfile)) {
                 GET(url, write_disk(destfile, overwrite = TRUE))
               }

               # unzip
               cancoast <- file.path(store,
                                     "data",
                                     "cancoast")
               if(!dir.exists(cancoast)){
                 dir.create(cancoast,recursive=TRUE)
               }
               unzip(destfile,exdir = cancoast)
               file.path(cancoast,list.files(cancoast,recursive=TRUE))
             },
             format = "file",
             cue = tar_cue("never"),
             packages = "httr" # this is probably the only time we'll use this package so it doesn't nee to load every time!
  ),

  tar_target(data_CANCOAST_CSI_V2_5_6,
             command = st_read(data_CanCoast2.0[endsWith(data_CanCoast2.0,"CANCOAST_CSI_V2_5_6.shp")]) |>
               st_transform(4326)
             ),

  tar_target(data_CSIScore_Intersection,
             command = {
               siteBufferToMultiLineIntersection_future(
                 sites = data_CIVI_Sites[1:5,],
                 name_sites = "HarbourCode",
                 sfLines = data_CANCOAST_CSI_V2_5_6,
                 name_sfLines_variable = "CSI_diff",
                 n_cores = 5
               )
             }),



  # Indicators

  tar_target(ind_coastal_sensitivity_index,
             command={
               data_CSIScore_Intersection |>
                 mutate(HarbourCode = as.numeric(harbourCode),
                        Value = weighted.mean.CSI_diff,
                        Score=as.numeric(cut(Value,breaks=5, labels=1:5)))|>
                 select(HarbourCode,Value,Score)
             }),

  tar_target(ind_harbour_condition,
             command={
               data_CIVI_HarbourCondition |>
                 mutate(HarbourCode = `Harb Code`,
                        Value = as.numeric(`Harb Condition`),
                        Score = Value)|>
                 select(HarbourCode,Value,Score)
             }),

  tar_target(ind_degree_of_protection,
             command={
               NULL
             }),

  tar_target(ind_sea_level_change,
             command={
               st_extract(data_sea_level, data_CIVI_Sites) |>
                 as.data.frame() |>
                 mutate(Value = as.numeric(ssp245_rslc_p50),
                        Score = as.numeric(cut(as.vector(transformSkewness(abs(Value))), breaks=5, labels=1:5)),
                        HarbourCode = data_CIVI_Sites$HarbourCode) |>  #TODO document that the Values are in cm
                 select(HarbourCode,Value,Score)
             }),

  tar_target(ind_ice_day_change,
             command={
               st_extract(data_ice_days, data_CIVI_Sites)|>
                 as.data.frame() |>
                 mutate(Value = as.numeric(mean),
                        Score = as.numeric(cut(as.vector(transformSkewness(abs(Value))), breaks=5, labels=1:5)),
                        HarbourCode = data_CIVI_Sites$HarbourCode) |>  #TODO document that the Values are in days
                 select(HarbourCode,Value,Score)

             }),

  tar_target(ind_replacement_cost,
             command={
               data_CIVI_ReplacementCost |>
                 mutate(HarbourCode = `Harb Code`) |>
                 group_by(HarbourCode) |>
                 reframe(Value = sum(`Facility Replacement Cost`, na.rm=TRUE)) |>
                 mutate(Score = as.numeric(cut(as.vector(transformSkewness(Value)), breaks=5, labels=1:5))) |>
                 select(HarbourCode,Value,Score)
             }),

  tar_target(ind_harbour_utilization,
             command={
               data_CIVI_HarbourCondition |>
                 mutate(HarbourCode = `Harb Code`,
                        Value = as.numeric(`Harbour Utilization`),
                        Score = as.numeric(cut(as.vector(transformSkewness(Value)), breaks=5, labels=1:5))) |>
                 select(HarbourCode,Value,Score)
             }),

  tar_target(ind_sch_proximity,
             command={
                ind_proximity(data_CIVI_Sites, data_CIVI_Sites, ors_api_key=read.table(file.path(store,"data","ors_api_key.txt"))$V1)
             }),

  tar_target(ind_proximity_full_debug,
             command={
               ind_proximity(data_CIVI_Sites=data_CIVI_Sites, ors_api_key=read.table(file.path(store,"data","ors_api_key.txt"))$V1, full_results=TRUE)
             }),


  # Components
  tar_target(comp_sensitivity,
             command={
               list(ind_coastal_sensitivity_index = ind_coastal_sensitivity_index,
                    ind_harbour_condition = ind_harbour_condition) |>
                 join_comps() |>
                 rowwise() |>
                 mutate(sensitivity = geometricMean(
                   c(ind_coastal_sensitivity_index_Score,
                     abs(6-ind_harbour_condition_Score))))
             },
             tidy_eval = FALSE),

  tar_target(comp_exposure,
             command={
               list(ind_sea_level_change = ind_sea_level_change,
                    ind_ice_day_change = ind_ice_day_change) |>
                 join_comps() |>
                 rowwise() |>
                 mutate(exposure = geometricMean(
                   c(ind_sea_level_change_Score,
                     ind_ice_day_change_Score)))


             },
             tidy_eval = FALSE),

  tar_target(comp_adaptive_capacity,
             command={
               list(ind_replacement_cost = ind_replacement_cost,
                    ind_harbour_utilization = ind_harbour_utilization,
                    ind_sch_proximity = ind_sch_proximity) |>
                 join_comps() |>
                 rowwise() |>
                 mutate(adaptive_capacity = geometricMean(
                   c(abs(6-ind_replacement_cost_Score),
                     abs(6-ind_harbour_utilization_Score),
                     ind_sch_proximity_Score)))
             },
             tidy_eval = FALSE),

  # CIVI

  tar_target(CIVI,
             command={
               comp_sensitivity |>
                 full_join(comp_exposure, by="HarbourCode") |>
                 full_join(comp_adaptive_capacity, by="HarbourCode") |>
                 rowwise() |>
                 mutate(CIVI = geometricMean(
                   c(sensitivity,
                     exposure,
                     abs(6-adaptive_capacity))))
             })
)
