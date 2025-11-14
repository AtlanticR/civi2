#' Compute proximity metrics for SCH harbours
#'
#' This function first calculates proximity between Small Craft Harbours (SCH) sites
#' based on sailing distances (least-cost paths avoiding land) while assuming ~
#' a speed of 10 knots. It identifies
#' neighbouring harbours within 20 km straight-line distance, then estimates
#' sailing routes while avoiding land using raster least-cost path analysis.
#' The function then calculates the driving distances
#' using the openrouteservices package. If a waypoint falls on land during the
#' sailing computation or a driving distance is not available, it defaults back
#' to a straight-line distance and way cross land. For driving distance if
#' the straight line computation is used a driving distance of 70 km/h is assumed.
#' The final output takes the larger of the sailing and driving distances.
#'
#' @param data_CIVI_Sites A data frame likely from [data_CIVI_Sites()]
#' @param full_results a Boolean indicating if you want a more fulsome result
#' including the following: "HarbourName","Sailing_Nearest_Neighbour", "Sailing_Time", "Sailing_Distance",
#' "Driving_Nearest_Neighbour", "Driving_Distance","Driving_Time","Sailing_Plot",
#' "Driving_Plot","Driving_time", "Result"
#' @param ors_api_key your ors_api_key password to obtain driving distance
#' using the openrouteservice package
#'
#' @return A list of data frames (one per harbour) containing:
#' \describe{
#'   \item{Neighbour}{Name of neighbouring harbour}
#'   \item{Distance_Sailing_Km}{Estimated sailing distance in km}
#'   \item{plot}{A ggplot object visualizing the route}
#' }
#'
#' @details
#' Distances are computed in two stages:
#' \enumerate{
#'   \item A straight-line (Haversine) filter to keep neighbours within 20 km.
#'   \item A least-cost path calculation over a raster with land masked as
#'         impassable. If routing fails (e.g., waypoints fall on land), the
#'         function falls back to Haversine distance.
#' }
#'
#' @importFrom geosphere distHaversine distGeo
#' @importFrom sf st_as_sf st_transform st_buffer st_bbox st_coordinates st_crs
#' @importFrom rnaturalearth ne_download
#' @importFrom raster raster extent extract
#' @importFrom fasterize fasterize
#' @importFrom gdistance transition geoCorrection shortestPath
#' @importFrom ggplot2 ggplot geom_sf geom_text labs theme_minimal coord_sf aes
#' @importFrom openrouteservice ors_directions ors_api_key
#' @importFrom leaflet leaflet addTiles addPolylines addMarkers
#' @importFrom htmltools tags
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## CIVI Results
#' sites <- data_CIVI_Sites()
#' out <- ind_proximity(sites, ors_api_key="your_ors_api_key")
#'
#' ## FULL Results
#' out <- ind_proximity(sites, ors_api_key="your_ors_api_key", full_results=TRUE)
#' }


ind_proximity <- function(data_CIVI_Sites=data_CIVI_Sites, ors_api_key=NULL, full_results=FALSE) {
  if (is.null(ors_api_key)) {
    stop("Must provide a ors_api_key to obtain driving distances using the openroutesservice package")
  }
  ors_api_key(ors_api_key)
  proj <- "+proj=eqdc +lon_0=-96.328125 +lat_1=45.5659042 +lat_2=76.9551598 +lat_0=61.260532 +datum=WGS84 +units=m +no_defs"


  # FIND SAILING DISTANCES
  sch_df <- data_CIVI_Sites[which(data_CIVI_Sites$Administration == "SCH"),] |>
    st_transform(proj)


  # Initialize failure log

  sailing_output <- vector("list", length(sch_df$HarbourName))
  driving_output <- vector("list", length(sch_df$HarbourName))
  names(sailing_output) <- sch_df$HarbourName
  names(driving_output) <- sch_df$HarbourName


  # Download and buffer land
  land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") #Downloading the land at large scale
  land_proj <-  "+proj=eqdc +lon_0=-96.328125 +lat_1=45.5659042 +lat_2=76.9551598 +lat_0=61.260532 +datum=WGS84 +units=m +no_defs"
  land_buffered_wgs84 <- st_transform(land, crs = land_proj) # Changing the projection #32620


  # Function to get 2 nearest neighbours (based on sailing)
  for (i in seq_along(sch_df$HarbourName)) {
    message(paste0("i = ", i))
    #others <- sch_df[-i, ]

    # Filter by straight-line distance (Haversine)
    dists_haversine <- distHaversine(
      matrix(c(sch_df$Long[i], sch_df$Lat[i]), ncol=2),
      matrix(c(sch_df$Long, sch_df$Lat), ncol=2)
    ) / 1000

    within_20 <- sch_df[dists_haversine <= 20, , drop = FALSE] # NOTE: THIS COULD BE REMOVED

    if (nrow(within_20) == 1) {
      sailing_output[[i]] <- data.frame(
        Neighbour = NA,
        Distance_Sailing_Km = NA,
        plot=NA,
        SailingTime_Hours=NA
      )

      driving_output[[i]] <- data.frame(
        Neighbour = NA,
        Distance_Driving_Km = NA,
        plot=NA,
        Time_Driving_Km=NA
      )

    } else {
      sailing_output[[i]] <- data.frame(
        Neighbour = within_20$HarbourName,
        Distance_Sailing_Km = NA,
        plot=NA,
        SailingTime_Hours=NA
      )

      driving_output[[i]] <- data.frame(
        Neighbour = within_20$HarbourName,
        Distance_Driving_Km = NA,
        plot=NA,
        Time_Driving_Km=NA
      )
      sailing_output[[i]]$plot <- vector("list", nrow(sailing_output[[i]]))
      driving_output[[i]]$plot <- vector("list", nrow(driving_output[[i]]))

      vessel_speed_kmh <- 18  ### Roughly 10 knots

      waypoints_proj <- st_transform(within_20, crs=proj)

      # 3. Create raster grid
      bb <- st_bbox(waypoints_proj) # puts a box around your points of interest (within_20 and sch_df[i])
      margin <- 20000 # This is for when we need to go outside of our bounding box
      r <- raster(extent(bb$xmin - margin, bb$xmax + margin,
                         bb$ymin - margin, bb$ymax + margin),
                  res = 100) # (m) - this resolution would make the line within 100 m of the point
      r[] <- 1  # water = 1

      # 4. Rasterize buffered land
      land_raster <- fasterize(land_buffered_wgs84, r) # This shows us where the land is
      r[!is.na(land_raster[])] <- 99999999999  # land = impassable

      # Determine how easy to move from one waypoint to another
      # if both water (1) it's easy, if one is land it's impassible
      tr <- transition(r, transitionFunction = function(x) 1 / mean(x), directions = 8) #(cost of moving from one raster cell to another)
      # Above it assumes that all 'steps' are the same size. Below takes into account real geographic distances
      tr <- geoCorrection(tr, type = "c")

      p1 <- st_coordinates(sch_df[i,])

      others <- within_20[!(within_20$HarbourCode == sch_df$HarbourCode[i]),]

      for (j in seq_along(others$HarbourCode)) {
        p2 <- st_coordinates(others[j, ])

        if (others$MarineInland[j] == "Marine") {

        # Calculating shortest path across a raster.
        path <- tryCatch({
          gdistance::shortestPath(tr, p1, p2, output = "SpatialLines")
        }, error = function(e) {
          message("⚠️ Pathfinding failed: ", e$message)
          return(NULL)
        })
        browser()

        # 9. Plot and calculate distance if path is valid
        if (!is.null(path)) {
          route_proj <- st_as_sf(path)
          st_crs(route_proj) <- proj
          route_sf <- st_transform(route_proj, crs=4326)
          land_buffered_wgs84 <- st_transform(land_buffered_wgs84, crs=4326)


          waypoints_sf <- rbind(sch_df[i,], others[j,]) |> st_transform(4326)
          # Create leaflet map
          title_text <- "sailing route"
          ggp  <- leaflet() %>%
            addTiles() %>%
            # Add sailing route
            addPolylines(data = route_sf,
                         color = "blue",
                         weight = 3,
                         opacity = 1) %>%
            # Add waypoints
            addCircleMarkers(data = waypoints_sf,
                             color = "red",
                             radius = 6,
                             fillOpacity = 1,
                             label = ~HarbourName) %>%
            # Set view to focus on the area
            setView(lng = mean(waypoints_sf$Long), lat = mean(waypoints_sf$Lat), zoom = 9) %>%
            addControl(html = paste0("<h3>", title_text, "</h3>"), position = "topright") %>%
            addPolygons(data=land_buffered_wgs84, col='brown')


          coords <- st_coordinates(route_sf)
          distance <- sum(geosphere::distGeo(coords[-nrow(coords), 1:2], coords[-1, 1:2]))

          sailing_output[[i]]$plot[[j]] <- ggp
          coords <- st_coordinates(route_sf)
          distance <- sum(geosphere::distGeo(coords[-nrow(coords), 1:2], coords[-1, 1:2]))
          sailing_output[[i]]$Distance_Sailing_Km[j] <- round(distance/1000, 2)
          sailing_output[[i]]$SailingTime_Hours[j] <- round(sailing_output[[i]]$Distance_Sailing_Km[j] / vessel_speed_kmh, 2)  ### 🔴 new line
          cat("✅ Total sailing distance (avoiding land):", round(distance / 1000, 2), "km\n")
        } else {
          # Not Marine
          sailing_output[[i]] <- data.frame(
            Neighbour = NA,
            Distance_Sailing_Km = NA,
            plot=NA,
            SailingTime_Hours=NA
          )

        }
          } else {
          cat("❌ No valid sailing route found. Try adjusting resolution or waypoint positions.\n")
        }


        ## CONSIDERING DRIVING DISTANCES
        dest_lat <- within_20$Lat[j]
        dest_lon <- within_20$Long[j]
        dest_name <- within_20$HarbourName[j]
        coords <- list(origin_coords, c(dest_lon, dest_lat))

        res <- tryCatch({
          ors_directions(coords, profile = "driving-car")
        }, error = function(e) NULL)

        driving_plot <- tryCatch({
          ors_directions(coords, profile = "driving-car", output = "sf")
        }, error = function(e) NULL)

        if (is.null(res)) {
          # No driving route available
          dist_km <- distHaversine(origin_coords, c(dest_lon, dest_lat)) / 1000  # in km
          speed <- 70
          duration_hr <- dist_km/speed

        } else {

          dist_m <- tryCatch({
            res$features[[1]]$properties$summary$distance
          }, error = function(e) NA)


          dist_km <- dist_m/1000


          # TIME
          duration_sec <- res$features[[1]]$properties$summary$duration

          # Convert to hours
          duration_hr <- duration_sec / 3600

        }

        driving_output[[i]]$Distance_Driving_Km[j] <- dist_km
        driving_output[[i]]$Time_Driving_Km[j] <- duration_hr

        if (!(is.null(driving_plot)) && !(is.null(res))) {
          map <- leaflet() %>%
            addTiles() %>%
            addPolylines(
              data = driving_plot,
              color = "blue",
              weight = 4,
              opacity = 0.8
            ) %>%
            addMarkers(
              lng = st_coordinates(driving_plot)[1, "X"],
              lat = st_coordinates(driving_plot)[1, "Y"],
              popup = sch_df$HarbourName[i],
              label = sch_df$HarbourName[i]
            ) %>%
            addMarkers(
              lng = st_coordinates(driving_plot)[nrow(st_coordinates(driving_plot)), "X"],
              lat = st_coordinates(driving_plot)[nrow(st_coordinates(driving_plot)), "Y"],
              popup = driving_output[[i]]$Neighbour[j],
              label = driving_output[[i]]$Neighbour[j]
            )

          driving_output[[i]]$plot[[j]] <- map
        } else {
          driving_output[[i]]$plot[[j]] <- 1

        }
      }



    }
  }

  ind_proximety <- data.frame(HarbourName=names(sailing_output), Sailing_Nearest_Neighbour=NA, Sailing_Time=NA, Sailing_Distance=NA, Driving_Nearest_Neighbour=NA, Driving_Distance=NA, Driving_Time=NA, Sailing_Plot=NA, Driving_Plot=NA)
  for (i in seq_along(sailing_output)) {
    message("sailing output i = ", i)
    if (!(all(is.na(sailing_output[[i]]$Neighbour)))) {
    keep <- which(sailing_output[[i]]$Distance_Sailing_Km == min(sailing_output[[i]]$Distance_Sailing_Km))
    if (length(keep) == 1) {
    ind_proximety$Sailing_Nearest_Neighbour[i] <- sailing_output[[i]]$Neighbour[keep]
    } else {
      ind_proximety$Sailing_Nearest_Neighbour[i] <- paste0(sailing_output[[i]]$Neighbour[keep], collapse=" & ")

    }
    ind_proximety$Sailing_Distance[i] <- sailing_output[[i]]$Distance_Sailing_Km[keep][1]
    ind_proximety$Sailing_Plot[i] <- sailing_output[[i]]$plot[keep][1]
    ind_proximety$Sailing_Time[i] <- sailing_output[[i]]$SailingTime_Hours[keep][1]
    }


    if (!(all(is.na(driving_output[[i]]$Neighbour)))) {
      keep <- which(driving_output[[i]]$Distance_Driving_Km == min(driving_output[[i]]$Distance_Driving_Km))
      ind_proximety$Driving_Nearest_Neighbour[i] <- driving_output[[i]]$Neighbour[keep]
      ind_proximety$Driving_Distance[i] <- driving_output[[i]]$Distance_Driving_Km[keep]
      ind_proximety$Driving_Time[i] <- driving_output[[i]]$Time_Driving_Km[keep]

      ind_proximety$Driving_Plot[i] <- driving_output[[i]]$plot[keep]
    }
  }

  ind_proximety$Result <- NA

  ind_proximety_short <- data.frame(HarbourName=names(sailing_output), Value=NA, Score=NA)

  for (i in seq_along(ind_proximety$HarbourName)) {
    message("proximity output i = ", i)

    if (!(is.na(ind_proximety$Sailing_Time[i]) && is.na(ind_proximety$Driving_Time[i]))) {
      max_result <- max(ind_proximety$Sailing_Time[i], ind_proximety$Driving_Time[i],na.rm=TRUE)

    ind_proximety_short$Value[i] <- max_result
    if (is.na(max_result)) {
      ind_proximety$Result[i] <- "Neither"
    } else if (ind_proximety$Sailing_Time[i] == max_result && ind_proximety$Driving_Time[i] == max_result) {
      ind_proximety$Result <- "Same"
    } else if (ind_proximety$Sailing_Time[i] == max_result) {
      ind_proximety$Result <- "Sailing"
    } else {
      ind_proximety$Result <- "Driving"
    }
  } else {
    ind_proximety$Result[i] <- "Neither"
    ind_proximety_short$Value[i] <- NA
  }
  }


  # Score
  ind_proximety_short$Score <- cut(as.vector(transformSkewness(ind_proximety_short$Value)), breaks=5, labels=1:5)


  # New Changing Names
  ind_proximety_short$HarbourCode <- NA
  if (!(full_results)) {
  for (i in seq_along(ind_proximety_short$HarbourName)) {
    message(i)
    keep <- which(data_CIVI_Sites$HarbourName == ind_proximety$HarbourName[i])
    if (!(length(keep) == 0)) {
    ind_proximety_short$HarbourCode[i] <- data_CIVI_Sites$HarbourCode[keep]
    }
  }

    ind_proximety_short <- ind_proximety_short[,c("HarbourCode", "Value", "Score")]


  }
  # End New

  if (full_results) {
    return(ind_proximety)
  } else {
    return(ind_proximety_short)
  }



}
