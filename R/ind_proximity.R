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

  # FIND SAILING DISTANCES
  cfw <- data_CIVI_Sites[which(data_CIVI_Sites$Administration == "SCH"),]
  sch_df <- cfw[,c("HarbourName", "Lat", "Long")]

  # Initialize failure log

  sailing_output <- vector("list", length(sch_df$HarbourName))
  driving_output <- vector("list", length(sch_df$HarbourName))
  names(sailing_output) <- sch_df$HarbourName
  names(driving_output) <- sch_df$HarbourName


  # Function to get 2 nearest neighbours (based on sailing)
  for (i in seq_along(sch_df$HarbourName[1:50])) {
    message(paste0("i = ", i))
    origin_lat <- sch_df$Lat[i]
    origin_lon <- sch_df$Long[i]
    origin_name <- sch_df$HarbourName[i]

    others <- sch_df[-i, ]

    # Filter by straight-line distance (Haversine)
    dists_haversine <- distHaversine(
      matrix(c(origin_lon, origin_lat), ncol=2),
      matrix(c(others$Long, others$Lat), ncol=2)
    ) / 1000

    within_20 <- others[dists_haversine <= 10, , drop = FALSE] # NOTE: THIS COULD BE REMOVED

    if (nrow(within_20) == 0) {
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


      origin_coords <- c(origin_lon, origin_lat)
      # Compute sailing distance for all sch within 20 km.

      vessel_speed_kmh <- 18  ### Roughly 10 knots

      for (j in seq_along(within_20$HarbourName)) {
        message("j = ",j)
        #message(paste0("j = ", j))
        compared_coords <- c(within_20$Long[j],within_20$Lat[j])

        waypoints <- data.frame(
          name = c(origin_name, within_20$HarbourName[j]),
          lon = c(origin_coords[1], compared_coords[1]),
          lat = c(origin_coords[2], compared_coords[2])
        )

        waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)
        proj <- "+proj=eqdc +lon_0=-96.328125 +lat_1=45.5659042 +lat_2=76.9551598 +lat_0=61.260532 +datum=WGS84 +units=m +no_defs"
        waypoints_proj <- st_transform(waypoints_sf, crs=proj)
        # 2. Download and buffer land
        land <- ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") #Downloading the land at medium scale
        land_proj <- st_transform(land, crs = "+proj=eqdc +lon_0=-96.328125 +lat_1=45.5659042 +lat_2=76.9551598 +lat_0=61.260532 +datum=WGS84 +units=m +no_defs") # Changing the projection #32620
        #plot(land_proj$geometry)
        land_buffered_wgs84 <- land_proj
        #land_buffered <- st_buffer(land_proj, dist = -100)  # inward buffer (take each land and move it inwards by 100 units)
        #land_buffered_wgs84 <- st_transform(land_proj, crs = "") # Switch to another projection (this could be one problem)

        # 3. Create raster grid
        bb <- st_bbox(waypoints_proj) # puts a box around your points of interest
        margin <- 100000   # 10 m buffer around waypoints
        r <- raster(extent(bb$xmin - margin, bb$xmax + margin,
                           bb$ymin - margin, bb$ymax + margin),
                    res = 100) # (m) - this resolution would make the line within 100 m of the point
        r[] <- 1  # water = 1

        # 4. Rasterize buffered land
        land_raster <- fasterize(land_buffered_wgs84, r) # This shows us where the land is
        r[!is.na(land_raster[])] <- 99999999999  # land = impassable

        # 6. Check if waypoints fall on valid raster cells
        extract_vals <- raster::extract(r, st_coordinates(waypoints_proj)) # look up each waypoint value to see which raster (land or water it's in)
        # Determine how easy to move from one waypoint to another
        # if both water (1) it's easy, if one is land it's impassible
        tr <- transition(r, transitionFunction = function(x) 1 / mean(x), directions = 8)
        # Above it assumes that all 'steps' are the same size. Below takes into account real geographic distances
        tr <- geoCorrection(tr, type = "c")


        p1 <- st_coordinates(waypoints_proj[1, ])
        p2 <- st_coordinates(waypoints_proj[2, ])
        # snap points to nearest water cell if on land

        # snap_flag <- FALSE  # track if snapping happened
        #
        # # Below checks if the point is on land. It then flattens the raster to find which are not land.
        # # it uses xyFromCell() to convert lat to lng and then uses: distGeo() to see how far it is from water. It then moves the point
        # snap_to_water <- function(coord, raster_layer) {
        #   if (is.infinite(raster::extract(raster_layer, coord))) {
        #     water_cells <- which(raster_layer[] != Inf, arr.ind = TRUE)
        #     xy <- xyFromCell(raster_layer, water_cells)
        #     dists <- geosphere::distGeo(matrix(coord, ncol = 2), xy)
        #     snap_flag <<- TRUE  # mark that snapping happened
        #     return(xy[which.min(dists), ])
        #   } else {
        #     return(coord)
        #   }
        # }
        #
        # p1 <- snap_to_water(p1, r)
        # p2 <- snap_to_water(p2, r)

        # Calculating shortest path across a raster.
        path <- tryCatch({
          shortestPath(tr, p1, p2, output = "SpatialLines")
        }, error = function(e) {
          message("⚠️ Pathfinding failed: ", e$message)
          return(NULL)
        })

        # 9. Plot and calculate distance if path is valid
        if (!is.null(path)) {
          route_proj <- st_as_sf(path)
          st_crs(route_proj) <- proj
          route_sf <- st_transform(route_proj, crs=4326)
          land_buffered_wgs84 <- st_transform(land_buffered_wgs84, crs=4326)

          #st_crs(waypoints_sf
          #st_crs(land) <- 4326

          # Create leaflet map
          #title_text <- if (snap_flag) "Snap to water applied" else "Sailing route not altered"
          title_text <= "sailing route"
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
                             label = ~name) %>%
            # Set view to focus on the area
            setView(lng = mean(waypoints$lon), lat = mean(waypoints$lat), zoom = 9) %>%
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
          #browser()
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
