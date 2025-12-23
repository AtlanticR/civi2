if(!require(librarian)) install.packages("librarian")
pkgs <- c("AtlanticR/civi2",
          "targets",
          "readxl",
          "sf",
          "stars",
          "ncmeta",
          "qs",
          "qs2",
          "future",
          "future.apply",
          "dplyr",
          "purrr",
          #"crew",
          "geosphere",
          "rnaturalearth",
          "raster",
          "fasterize",
          "gdistance",
          "ggplot2",
          "openrouteservice",
          "htmltools",
          "leaflet")
shelf(pkgs)
tar_option_set(packages = basename(pkgs),
               # controller = crew_controller_local(workers = 3),
               error = "continue")

# Set the store path for targets
path_to_store <- function(){
  if(dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2")){
    store = "//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/CIVI/civi2"
  } else if(dir.exists("/srv/sambashare/CIVI/civi2")){
    store = "/srv/sambashare/CIVI/civi2"
  } else {
    warning("CIVI data store not found. Please check the directory paths.")
    store = getwd()
  }
  return(store)
}

tar_config_set(store = path_to_store())



# Define the targets pipeline
list(
  # Raw Data
  tar_target(data_CIVI_Sites,
             command={
               # file from Yanice Berkane (SCH)
               file <- read_excel(file.path(path_to_store(), "data", "2025-08-19 - SCH Harbours - Latitude and Longitude.xlsx")) |>
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
               read_excel(file.path(path_to_store(), "data", "2025-08-05 - CIVI Replacement Cost.xlsx"))
             }),

  tar_target(data_CIVI_HarbourCondition,
             command={
               # file from Annie Boudreau (SCH)
               read_excel(file.path(path_to_store(), "data", "2025-08-05 - CIVI Harbour Condition use and capacity.xlsx")) |>
                 filter(`Harb Code` %in% data_CIVI_Sites$HarbourCode)
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
               cancoast <- file.path(path_to_store(),
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
               future_siteBufferToMultiLineIntersection(
                 sites = data_CIVI_Sites |>
                   filter(HarbourCode %in% context_ind_MarineInland$HarbourCode[context_ind_MarineInland$MarineInland=="Marine"]),
                 name_sites = "HarbourCode",
                 sfLines = data_CANCOAST_CSI_V2_5_6,
                 name_sfLines_variable = "CSI_diff",
                 n_cores = 7,
                 max_size = 10000*1024^2

               )
             }),

  tar_target(data_CSIScore_Intersection_2000s,
             command = {
               future_siteBufferToMultiLineIntersection(
                 sites = data_CIVI_Sites |>
                   filter(HarbourCode %in% context_ind_MarineInland$HarbourCode[context_ind_MarineInland$MarineInland=="Marine"]),
                 name_sites = "HarbourCode",
                 sfLines = data_CANCOAST_CSI_V2_5_6,
                 name_sfLines_variable = "CSI_2000s",
                 n_cores = 3,
                 max_size = 10000*1024^2

               )
             }),

####KF#
tar_target(data_distance,
           command={
             ors_api_keys <- read.csv(file.path(path_to_store(),"data","ors_api_key.csv")) |>
               pull(key) |>
               unlist()

             ind_proximity_lakes(data_CIVI_Sites=data_CIVI_Sites, ors_api_key=ors_api_keys, full_results=TRUE)
           }),

  # Indicators

  tar_target(ind_coastal_sensitivity_index,
             command={
               breaks <- c(-10000000, -500, -150, 150, 500, 10000000)
               x <- data_CSIScore_Intersection_2000s |>
                 mutate(HarbourCode = as.numeric(HarbourCode),
                        Value = weighted.mean.CSI_2000s,
                        Score=as.numeric(cut(Value,breaks=breaks, labels=1:5)))|>
                 dplyr::select(HarbourCode,Value,Score)
               x
             }),

  tar_target(ind_harbour_condition,
             command={
               data_CIVI_HarbourCondition |>
                 mutate(HarbourCode = `Harb Code`,
                        Value = as.numeric(`Harb Condition`),
                        Score = as.numeric(cut(Value,breaks=5, labels=1:5)))|>
                 dplyr::select(HarbourCode,Value,Score)
             }),

  tar_target(ind_degree_of_protection,
             command={
               x <- read_excel(file.path(path_to_store(), "data", "DoP.xlsx"))

               df <- data.frame(HarbourCode=data_CIVI_Sites$HarbourCode, "Value"=NA, "Score"=NA)

               for (i in seq_along(df$HarbourCode)) {
                 keep <- which(x$HarbourCode == df$HarbourCode[i])
                 if (!(length(keep) == 0)) {
                 df$Value[i] <- x$DegreeOfProtection[keep]
                 }
               }

               df$Score <- as.numeric(cut(as.vector(transformSkewness(df$Value)), breaks=5, labels=1:5))
               df
             }),

  tar_target(ind_sea_level_change,
             command={
               st_extract(data_sea_level, data_CIVI_Sites) |>
                 as.data.frame() |>
                 mutate(Value = as.numeric(ssp245_rslc_p50),
                        Score = as.numeric(cut(as.vector(transformSkewness(abs(Value))), breaks=5, labels=1:5)),
                        HarbourCode = data_CIVI_Sites$HarbourCode) |>  #TODO document that the Values are in cm
                 dplyr::select(HarbourCode,Value,Score)
             }),

  tar_target(ind_ice_day_change,
             command={
               ice <- st_extract(data_ice_days, data_CIVI_Sites) |>
                 as.data.frame() |>
                 mutate(Value = as.numeric(mean))
               d <- 10000
               while(any(is.na(ice$Value))){
                 message(paste0("Buffering for missing ice day values with distance = ", d, " meters"))
                 bufferedsites <- data_CIVI_Sites[is.na(ice$Value), ] |>
                   rowwise() |>
                   mutate(geometry = st_buffer(geometry,dist = d) |>
                            st_bbox() |>
                            st_as_sfc())
                 # Extract with buffer for NA locations
                 neighbor_vals <- map_dbl(seq_len(nrow(bufferedsites)), \(i) {
                   aggregate(data_ice_days, bufferedsites[i,], FUN = mean, na.rm = TRUE) |>
                     as.numeric()
                 })


                 ice$Value[is.na(ice$Value)] <- neighbor_vals
                 d <- d + 10000
               }


               ice |>
                 mutate(Score = as.numeric(cut(as.vector(transformSkewness(abs(Value))), breaks=5, labels=1:5)),
                        HarbourCode = data_CIVI_Sites$HarbourCode) |>  #TODO document that the Values are in days
                 dplyr::select(HarbourCode,Value,Score)

             }),

  tar_target(ind_replacement_cost,
             command={
               data_CIVI_ReplacementCost |>
                 mutate(HarbourCode = `Harb Code`) |>
                 group_by(HarbourCode) |>
                 reframe(Value = sum(`Facility Replacement Cost`, na.rm=TRUE)) |>
                 mutate(Score = as.numeric(cut(as.vector(transformSkewness(Value)), breaks=5, labels=1:5))) |>
                 dplyr::select(HarbourCode,Value,Score)
             }),

  tar_target(ind_harbour_utilization,
             command={
               data_CIVI_HarbourCondition |>
                 mutate(HarbourCode = `Harb Code`,
                        Value = as.numeric(`Harbour Utilization`),
                        Score = as.numeric(cut(as.vector(transformSkewness(Value)), breaks=5, labels=1:5))) |>
                 dplyr::select(HarbourCode,Value,Score)
             }),

  #This is the remenants of the original function that did not include lakes...remove -KF
#  tar_target(ind_sch_proximity,
#             command={
#               ors_api_keys <- read.csv(file.path(path_to_store(),"data","ors_api_key.csv")) |>
#                 pull(key) |>
#                 unlist()
#
#               sites <- data_CIVI_Sites |>
#                 dplyr::select(-MarineInland) |>
#                 left_join(context_ind_MarineInland, by = "HarbourCode") |>
#                 filter(MarineInland == "Marine")
#
#
#                marineSites <- ind_proximity(data_CIVI_Sites=sites, ors_api_key=ors_api_keys, full_results=TRUE)
#
#                x <- marineSites %>%
#                  mutate(HarbourCode = as.numeric(HarbourCode)) |>
#                  full_join(dplyr::select(context_ind_MarineInland,HarbourCode, MarineInland), by = "HarbourCode") |>
#                  mutate(Value = if_else(MarineInland == "Inland", NaN, Value),
#                         Score = if_else(MarineInland == "Inland", NaN, Score)) |>
#                  dplyr::select(-MarineInland)
#                x
#            }),

#this is the proximity function that we will use.
#The values produced by this function actually represent distance and not proximity,
#so they are in a reversed orientation
#new plan, :
#1)run the distance function separately and generate data as a target (driving and sailint)
#2)score and reverse them to create ind_sch_proximity_lakes
#the function has been altered to only generate distance data so this will go up in the data section , -KF
#  tar_target(ind_sch_proximity_lakes,
#             command={
#               ors_api_keys <- read.csv(file.path(path_to_store(),"data","ors_api_key.csv")) |>
#                 pull(key) |>
#                 unlist()
#
#               ind_proximity_lakes(data_CIVI_Sites=data_CIVI_Sites, ors_api_key=ors_api_keys, full_results=TRUE)
#             }),
#the indicator target will create the scores

####kf
#tar_target(ind_sch_proximity_lakes
tar_target(ind_sch_proximity_lakes,
           command={
            x <-  data_distance |>
               mutate(
                 Value = ifelse(Value > 1.25, NA, Value),# cutoff for extremely long times
                 Score = as.numeric(cut(as.vector(transformSkewness(Value)), breaks=4, labels=1:4)),#redistribute and score non na vals
                 Score = replace(Score, is.na(Score), 5),
                 HarbourCode = as.numeric(HarbourCode),
                 Score = abs(6-Score)) |> #reverse the order of the scores to turn diatance to proximity
               dplyr::select(HarbourCode,Value,Score)
            x
           }),


# Components
  tar_target(comp_sensitivity,
             command={
               ind_coastal_sensitivity_index_cleaned <- ind_coastal_sensitivity_index |>
                 right_join(context_ind_MarineInland, by = "HarbourCode") |>
                 mutate(Score = if_else(MarineInland == "Inland",
                                        NaN,
                                        Score)) |>
                 dplyr::select(-MarineInland)

               list(ind_coastal_sensitivity_index = ind_coastal_sensitivity_index_cleaned,
                    ind_harbour_condition = ind_harbour_condition,
                    ind_degree_of_protection=ind_degree_of_protection) |>
                 join_comps() |>
                 rowwise() |>
                 mutate(sensitivity = geometricMean(
                   c(ind_coastal_sensitivity_index_Score,
                     abs(6-ind_harbour_condition_Score),
                     abs(6-ind_degree_of_protection_Score))))
             },
             tidy_eval = FALSE),

  tar_target(comp_exposure,
             command={
               ind_sea_level_change_cleaned <- ind_sea_level_change |>
                 right_join(context_ind_MarineInland, by = "HarbourCode") |>
                 mutate(Score = if_else(MarineInland == "Inland",
                                        NaN,
                                        Score)) |>
                 dplyr::select(-MarineInland)

               list(ind_sea_level_change = ind_sea_level_change_cleaned,
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
                    ind_sch_proximity = ind_sch_proximity_lakes) |> #### |>  begin removing -KF
                     # dplyr::select(HarbourCode,Value,Score) |>
                     # mutate(HarbourCode = as.numeric(HarbourCode),
                     #        Score = abs(6-Score))) |>#### up to here - KF
                 join_comps() |>
                 rowwise() |>
                 mutate(adaptive_capacity = geometricMean(
                   c(abs(6-ind_replacement_cost_Score),
                     abs(6-ind_harbour_utilization_Score),
                     ind_sch_proximity_Score)))
             },
             tidy_eval = FALSE),


  # Contextual Indicators

  tar_target(context_ind_MarineInland,
             command={

               distmat <- data_CIVI_Sites |>
                 as.data.frame() |>
                 dplyr::select(HarbourCode) |>
                 mutate(
                   coastnearby = st_is_within_distance(
                     data_CIVI_Sites,
                     data_CANCOAST_CSI_V2_5_6,
                     dist = 20000,
                     sparse = TRUE
                   ) |>
                     lengths(),
                   MarineInland = if_else(coastnearby > 1, "Marine", "Inland")) |>
                 dplyr::select(-coastnearby)

             }),

  tar_target(context_ind_csd,
             command={
               # NOTE THIS IS A STATISTIC CANADA SHAPE FILE FOR CSDNAME (google 2025 sub division shape files census)
               csd <- st_read(list.files(file.path(path_to_store(), "data"), pattern="*shp",full.names = TRUE)) |>
                 st_transform(4326) |>
                 st_make_valid()

               csd |>
                 rename(CSDName = CSDNAME) |>
                 dplyr::select(CSDUID,CSDName) |>
                 st_intersection(data_CIVI_Sites) |>
                 as.data.frame() |>
                 dplyr::select(-geometry) |>
                 left_join(dplyr::select(csd,CSDUID), by = "CSDUID") |>
                 as.data.frame() |>
                 dplyr::select(HarbourCode, CSDUID, CSDName, CSD_Shape = geometry)


             }),

  tar_target(context_ind_population,
             command = {
               # Population
               tmp <- tempfile()
               download.file("https://www150.statcan.gc.ca/n1/tbl/csv/17100155-eng.zip",tmp)
               unzip_dir <- tempdir() # Temporary directory for extracted files
               unzip(tmp, exdir = unzip_dir)
               pop <- read.csv(file.path(unzip_dir,"17100155.csv"))

               # Last 7 digits of pop DGUID are CSDUID
               y <- context_ind_csd
               popNew <- NULL
               for (i in seq_along(data_CIVI_Sites$HarbourCode)) {
                 message(i)
                 if (!(is.na(y$CSDUID[i]))) {
                   keep <- pop[which(grepl(y$CSDUID[i], pop$DGUID) ),]
                   if (!(length(keep$REF_DATE)  == 0)) {
                     popNew[[i]] <- keep$VALUE[which(keep$REF_DATE == max(keep$REF_DATE, na.rm=TRUE))]
                     message(paste0("i = ", i , " and keep = ", keep$VALUE[which(keep$REF_DATE == max(keep$REF_DATE, na.rm=TRUE))]))
                   } else {
                     popNew[[i]] <- NA
                   }
                 } else {
                   popNew[[i]] <- NA
                 }
               }


               popNew <- unlist(popNew)
               y$Pop <- popNew
               df <- data.frame("HarbourCode"=y$HarbourCode, "ind_population"=popNew)
               df

             }),

  tar_target(context_ind_fishery_reliant_communities,
             command={
               fishingcomm <- read_excel(file.path(path_to_store(), "data", "DFO_CSBP_FishingCommunities_Final_2022.xlsx")) |>
                 mutate(CSDUID = as.character(CSDcode),
                        ind_fishery_reliant_communities = FishingRel)

               context_ind_csd |>
                 as.data.frame() |>
                 left_join(fishingcomm,
                           by = "CSDUID") |>
                 dplyr::select(HarbourCode, ind_fishery_reliant_communities)
             }),

  tar_target(context_ind_indigenous_community,
             command =
               {

                 url <- "https://geo.sac-isc.gc.ca/geomatics/rest/services/Donnees_Ouvertes-Open_Data/Premiere_Nation_First_Nation/MapServer/0/query"
                 params <- list(
                   where = "1=1",           # Select all features
                   outFields = "*",         # All fields
                   f = "geojson"            # Format GeoJSON
                 )
                 query_url <- paste0(url, "?", paste0(names(params), "=", params, collapse = "&"))
                 data_sf_fn <- st_read(query_url)
                 df_fn <- data_sf_fn[,c("BAND_NAME", "geometry")]
                 df_fn$community <- "First Nations"
                 names(df_fn) <- c("NAME", "geometry", "community")

                 # Get Inuit Community Data
                 inuit_url <- "https://geo.sac-isc.gc.ca/geomatics/rest/services/Donnees_Ouvertes-Open_Data/Communaute_inuite_Inuit_Community/MapServer/0/query"

                 # Query parameters for GeoJSON
                 inuit_params <- list(
                   where = "1=1",        # get all features
                   outFields = "*",      # all attributes
                   f = "geojson"         # request GeoJSON format
                 )

                 # Construct full query URL
                 inuit_query_url <- paste0(inuit_url, "?", paste0(names(inuit_params), "=", inuit_params, collapse = "&"))

                 # Read into an sf object
                 data_sf_inuit <- st_read(inuit_query_url)
                 df_inuit <- data_sf_inuit[,c("NAME", "geometry")]
                 df_inuit$community <- "Inuit"



                 DF <- rbind(df_fn, df_inuit)
                 y <- context_ind_csd
                 ## Census communities
                 IDs <- y$CSDUID
                 results <- data.frame(
                   ID = IDs,
                   n_communities = NA_integer_
                 )

                 for (i in seq_along(IDs)) {
                   message(i)
                   geom <- y$CSD_Shape[[i]]       # get i-th MULTIPOLYGON
                   inside <- st_within(DF, geom, sparse = FALSE)  # spatial check
                   y$indigenous_communities[i] <- sum(inside)  # count points TRUE/FALSE
                 }

                 df <- data.frame("HarbourCode"=y$HarbourCode, "ind_indigenous_community"=y$indigenous_communities)
                 df


               }),

  # CIVI

  tar_target(CIVI,
             command={
               x <- comp_sensitivity |>
                 full_join(comp_exposure, by="HarbourCode") |>
                 full_join(comp_adaptive_capacity, by="HarbourCode") |>
                 rowwise() |>
                 mutate(CIVI = geometricMean(
                   c(sensitivity,
                     exposure,
                     abs(6-adaptive_capacity))))

               y <- x %>%
                 left_join(data_CIVI_Sites %>% dplyr::select(HarbourCode, HarbourName, Province),
                           by = "HarbourCode") %>%
                 mutate("HarbourType"=NA) %>%
                 left_join(data_CIVI_Sites %>% dplyr::select(HarbourCode, Lat, Long, Zone),
                           by = "HarbourCode") %>%
                 rename(SCH_region = Zone) %>%
                 full_join(context_ind_MarineInland, by="HarbourCode") %>%
                 full_join(context_ind_csd, by="HarbourCode") %>%
                 full_join(context_ind_population, by="HarbourCode") %>%
                 full_join(context_ind_fishery_reliant_communities, by="HarbourCode") %>%
                 full_join(context_ind_indigenous_community, by="HarbourCode") %>%
                 full_join(data_distance %>%
                             mutate(HarbourCode = as.numeric(HarbourCode)) %>%
                             dplyr::select(HarbourCode,
                                           ind_sch_proximity_Nearest_Neighbour=Nearest_Neighbour,
                                           ind_sch_proximity_Sailing_Time=Sailing_Time,
                                           ind_sch_proximity_Sailing_Distance=Sailing_Distance,
                                           ind_sch_proximity_Driving_Distance=Driving_Distance,
                                           ind_sch_proximity_Driving_Time=Driving_Time),
                           by="HarbourCode")
               # Turn Province names into acronyms
               full_names <- unique(data_CIVI_Sites$Province)

               for (i in seq_along(full_names)) {
                 if (full_names[i] == "Nova Scotia") {
                   ac <- "NS"
                 } else if (full_names[i] == "Quebec") {
                   ac <- "QC"
                 } else if (full_names[i] == "Saskatchewan") {
                   ac <- "SK"
                 } else if (full_names[i] == "Manitoba") {
                   ac <- "MB"
                 } else if (full_names[i] == "Alberta") {
                   ac <- "AB"
                 } else if (full_names[i] == "British Columbia") {
                   ac <- "BC"
                 } else if (full_names[i] == "New Brunswick") {
                   ac <- "NB"
                 } else if (full_names[i] == "Ontario") {
                   ac <- "ON"
                 } else if (full_names[i] == "Prince Edward Island") {
                   ac <- "PE"
                 } else if (full_names[i] == "Northwest Territories") {
                   ac <- "NT"
                 } else if (full_names[i] == "Nunavut") {
                   ac <- "NU"
                 } else if (full_names[i] == "Newfoundland & Labrador") {
                   ac <- "NL"
                 }
                 y$Province[which(y$Province == full_names[i])] <- ac
               }

               y |>
                 dplyr::select(-CSD_Shape)

             }),

  tar_target(CIVI.csv,
             command = {
               write.csv(CIVI |>
                           dplyr::select(-geometry.x,-geometry.y),
                         file.path(path_to_store(),"data","CIVI.csv"),
                         row.names = FALSE)
             })

)

