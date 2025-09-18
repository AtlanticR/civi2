library(targets)
library(sf)
library(raster)
library(gdistance)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(fasterize)
library(geosphere)

# TEST
tar_load("CIVI_FinalWebsite")
cfw <- CIVI_FinalWebsite[which(CIVI_FinalWebsite$Administration == "SCH"),]
sch_df <- cfw[,c("HarbourName", "Lat", "Long")]

# Initialize failure log

sailing_output <- vector("list", length(sch_df$HarbourName))
names(sailing_output) <- sch_df$HarbourName

# Function to get 2 nearest neighbours (based on sailing)
for (i in seq_along(sch_df$HarbourName[1:3])) {
  message(i)
  origin_lat <- sch_df$Lat[i]
  origin_lon <- sch_df$Long[i]
  origin_name <- sch_df$HarbourName[i]

  others <- sch_df[-i, ]

  # Filter by straight-line distance (Haversine)
  dists_haversine <- distHaversine(
    matrix(c(origin_lon, origin_lat), ncol=2),
    matrix(c(others$Long, others$Lat), ncol=2)
  ) / 1000

  within_20 <- others[dists_haversine <= 20, , drop = FALSE]

  if (nrow(within_20) == 0) {
    sailing_output[[i]] <- data.frame(
      Neighbour = NA,
      Distance_Sailing_Km = NA,
      plot=NA
    )
  } else {
    sailing_output[[i]] <- data.frame(
      Neighbour = within_20$HarbourName,
      Distance_Sailing_Km = NA,
      plot=NA
    )
    sailing_output[[i]]$plot <- vector("list", nrow(sailing_output[[i]]))

  origin_coords <- c(origin_lon, origin_lat)


  # Compute sailing distance for all sch within 30 km.

  for (j in seq_along(within_20$HarbourName)) {
  message(j)
  compared_coords <- c(within_20$Long[j],within_20$Lat[j])

  waypoints <- data.frame(
    name = c(origin_name, within_20$HarbourName[j]),
    lon = c(origin_coords[1], compared_coords[1]),
    lat = c(origin_coords[2], compared_coords[2])
  )

  waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)

  # 2. Download and buffer land
  land <- ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")
  land_proj <- st_transform(land, crs = 32620)
  land_buffered <- st_buffer(land_proj, dist = -100)  # inward buffer
  land_buffered_wgs84 <- st_transform(land_buffered, crs = 4326)

  # 3. Create raster grid that automatically covers waypoints + buffer
  bb <- st_bbox(waypoints_sf)
  margin <- 0.5   # half a degree buffer around waypoints
  r <- raster(extent(bb$xmin - margin, bb$xmax + margin,
                     bb$ymin - margin, bb$ymax + margin),
              res = 0.01)
  r[] <- 1  # water = 1

  # 4. Rasterize buffered land
  land_raster <- fasterize(land_buffered_wgs84, r)
  r[!is.na(land_raster[])] <- Inf  # land = impassable

  # 5. Visualize raster and waypoints
  #plot(r, main = "Raster with Land Masked (Inf)")
  #plot(st_geometry(land_buffered_wgs84), add = TRUE, col = "gray")
  #plot(st_geometry(waypoints_sf), add = TRUE, col = "red", pch = 19)

  # 6. Check if waypoints fall on valid raster cells
  extract_vals <- raster::extract(r, st_coordinates(waypoints_sf))
  #JAIM
  if (any(is.infinite(extract_vals) | is.na(extract_vals))) {
    #stop("One or more waypoints fall on land or NA cells. Adjust coordinates or resolution.")

    message("⚠️ One or more waypoints fall on land or NA cells. Falling back to straight-line distance.")

    # fallback: straight-line Haversine distance
    distance <- distHaversine(p1, p2) / 1000  # in km

    sailing_output[[i]]$Distance_Sailing_Km[j] <- round(distance, 2)
    sailing_output[[i]]$plot[[j]] <- ggplot() +
      geom_sf(data = land, fill = "gray80", color = "black") +
      geom_sf(data = waypoints_sf, color = "red", size = 2) +
      geom_text(data = waypoints, aes(x = lon, y = lat, label = name),
                nudge_y = 0.05, size = 3.5, fontface = "bold") +
      labs(title = "Fallback Route (Straight Line)",
           subtitle = paste(waypoints$name, collapse = " → "),
           x = "Longitude", y = "Latitude") +
      theme_minimal()

    next  # skip least-cost routing, continue loop


  }

  # 7. Create transition object
  tr <- transition(r, transitionFunction = function(x) 1 / mean(x), directions = 8)
  tr <- geoCorrection(tr, type = "c")

  # 8. Compute least-cost path
  p1 <- as.numeric(st_coordinates(waypoints_sf[1, ]))
  p2 <- as.numeric(st_coordinates(waypoints_sf[2, ]))
  path <- tryCatch({
    shortestPath(tr, p1, p2, output = "SpatialLines")
  }, error = function(e) {
    message("⚠️ Pathfinding failed: ", e$message)
    return(NULL)
  })

  # 9. Plot and calculate distance if path is valid
  if (!is.null(path)) {
    route_sf <- st_as_sf(path)
    st_crs(route_sf) <- 4326

    ggp <- ggplot() +
      geom_sf(data = land, fill = "gray80", color = "black") +
      geom_sf(data = route_sf, color = "blue", size = 1.2) +
      geom_sf(data = waypoints_sf, color = "red", size = 2) +
      geom_text(data = waypoints, aes(x = lon, y = lat, label = name),
                nudge_y = 0.05, size = 3.5, fontface = "bold") +
      coord_sf(xlim = c(bb$xmin - margin, bb$xmax + margin),
               ylim = c(bb$ymin - margin, bb$ymax + margin), expand = FALSE) +
      labs(title = "Simulated Sailing Route",
           subtitle = paste(waypoints$name, collapse = " → "),
           x = "Longitude", y = "Latitude") +
      theme_minimal()

    sailing_output[[i]]$plot[[j]] <- ggp
    coords <- st_coordinates(route_sf)
    distance <- sum(geosphere::distGeo(coords[-nrow(coords), 1:2], coords[-1, 1:2]))
    sailing_output[[i]]$Distance_Sailing_Km[j] <- round(distance/1000, 2)

    #cat("✅ Total sailing distance (avoiding land):", round(distance / 1000, 2), "km\n")
  } else {
    cat("❌ No valid sailing route found. Try adjusting resolution or waypoint positions.\n")
  }
  }
  }
}

ind_proximety <- data.frame(HarbourName=names(sailing_output), Value=NA, Scoring=NA)
