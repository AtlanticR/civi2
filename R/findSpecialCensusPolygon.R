#' Assign census subdivision polygons to harbour locations
#'
#' Downloads 2021 Statistics Canada census subdivision (CSD) polygons and
#' assigns harbour locations to census subdivisions using a combination of
#' spatial intersection, nearest polygon centroid, and buffered overlap logic.
#'
#' The function is designed to handle edge cases where harbour locations fall
#' near nested or adjacent census subdivision boundaries. In cases where the
#' intersecting polygon and nearest polygon centroid disagree, a 1000 m buffer
#' is created around the harbour location and the polygon with the greatest
#' overlap area is selected.
#'
#' Optionally returns diagnostic leaflet maps showing the intersecting and
#' nearest census subdivision polygons for problematic locations.
#'
#' @param df A data frame containing harbour locations.
#' Must contain:
#' \describe{
#'   \item{Long}{Longitude coordinates in decimal degrees.}
#'   \item{Lat}{Latitude coordinates in decimal degrees.}
#'   \item{longitude}{Longitude coordinates in decimal degrees.}
#'   \item{latitude}{Latitude coordinates in decimal degrees.}
#'   \item{harbourCode}{Unique harbour identifier.}
#'   \item{harbourName}{Harbour name.}
#' }
#'
#' @param plot Logical indicating whether to return diagnostic
#' leaflet maps instead of the updated data frame.
#' Default is \code{FALSE}.
#'
#' @return
#' If \code{plot = FALSE}, returns the input data frame with:
#' \describe{
#'   \item{CSDUID}{Census subdivision unique identifier.}
#'   \item{CSDName}{Census subdivision name.}
#' }
#'
#' If \code{plot = TRUE}, returns a list of leaflet maps for locations where
#' the nearest polygon centroid and intersecting polygon disagree.
#'
#' @details
#' Workflow:
#' \enumerate{
#'   \item Download and read 2021 census subdivision polygons from Statistics Canada.
#'   \item Convert harbour coordinates to an sf object.
#'   \item Determine intersecting census subdivision polygons.
#'   \item Determine nearest census subdivision centroids.
#'   \item Identify locations where nearest and intersecting polygons differ.
#'   \item Create 1000 m buffers around harbour locations.
#'   \item Compare overlap areas between buffers and candidate polygons.
#'   \item Assign the census subdivision with the greatest overlap.
#' }
#'
#' @seealso
#' \code{\link[sf]{st_join}},
#' \code{\link[sf]{st_intersection}},
#' \code{\link[sf]{st_buffer}},
#' \code{\link[leaflet]{leaflet}}
#'
#' @import sf
#' @import dplyr
#' @import leaflet
#'
#' @export
findSpecialCensusPolygon <- function(df=NULL, plot=FALSE) {
  file <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000a21a_e.zip"
  tmp <- tempfile()
  download.file(file,tmp)
  unzip_dir <- tempdir() # Temporary directory for extracted files
  unzip(tmp, exdir = unzip_dir)

  extracted_files <- list.files(unzip_dir, full.names = TRUE)

  subdivisions <- st_read(extracted_files[grep("\\.shp$", extracted_files)]) # 2021 census polygons

  # STEP 2: Intersect the latitude and longitude from the harbour locations (found in CIVI_Sites) to see which polygon they intersect

  CIVI_sites_sf <- st_as_sf(df,
                            coords = c("Long", "Lat"),  # specify coordinate columns
                            crs = 4326)  # assuming coordinates are in WGS84 (EPSG:4326)
  CIVI_sites_sf <- st_transform(CIVI_sites_sf, st_crs(subdivisions))

  CIVI_with_subdivisions <- st_join(CIVI_sites_sf, subdivisions, left = FALSE) #POTENTIAL PROBLEM HERE JAIM

  # STEP 3: Assign the CSDUID and name based on this intersection

  df$CSDUID <- CIVI_with_subdivisions$CSDUID
  df$CSDName <- CIVI_with_subdivisions$CSDName

  subdivisions_centroids <- subdivisions %>%
    st_centroid() %>%
    mutate(centroid_coords = st_coordinates(geometry)) # Extract coordinates for centroids


  # 2. Determine the closest centroid to each point in CIVI_sites_sf
  # Use st_distance to compute distances between points and centroids
  dist_matrix <- st_distance(CIVI_sites_sf, subdivisions_centroids)

  # Find the index of the closest centroid for each point
  closest_centroid_idx <- apply(dist_matrix, 1, which.min)

  ## 🔴 NEW
  closest_centroid_idx <- apply(dist_matrix, 1, which.min)
  closest_centroid_dist_km <- dist_matrix[cbind(seq_len(nrow(dist_matrix)), closest_centroid_idx)] / 1000

  ## END NEW



  # Extract the closest centroid CSDNAME and coordinates
  CIVI_sites_sf$CSDNAME_closest <- subdivisions_centroids$CSDNAME[closest_centroid_idx]
  CIVI_sites_sf$CSDUID_closest <- subdivisions_centroids$CSDUID[closest_centroid_idx]


  # 3. Determine which points intersect with which subdivisions
  # Use st_intersects to find points that intersect polygons
  intersects_list <- st_intersects(CIVI_sites_sf, subdivisions)

  # Extract the CSDNAME of intersecting polygons
  CSDNAME_intersect <- sapply(intersects_list, function(x) {
    if (length(x) > 0) {
      subdivisions$CSDNAME[x[1]] # Take the first intersecting polygon, if any
    } else {
      NA # No intersection
    }
  })

  CSDUID_intersect <- sapply(intersects_list, function(x) {
    if (length(x) > 0) {
      subdivisions$CSDUID[x[1]] # Take the first intersecting polygon, if any
    } else {
      NA # No intersection
    }
  })

  CIVI_sites_sf$CSDNAME_intersect <- CSDNAME_intersect
  CIVI_sites_sf$CSDUID_intersect <- CSDUID_intersect


  # 4. Combine the results into a data frame

  # STEP 1: IDENTIFY WHEN A HARBOUR LOCATION HAS A POLYGON THAT IT OVERLAPS WITH BUT
  # A DIFFERENT POLYGON THAT IS CLOSEST TO THE CENTER FOR

  # output_df <- CIVI_sites_sf %>%
  #   mutate(
  #     latitude = st_coordinates(geometry)[,2],
  #     longitude = st_coordinates(geometry)[,1]
  #   ) %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(latitude, longitude, CSDNAME_closest, CSDNAME_intersect,CSDUID_closest, CSDUID_intersect)
  output_df <- CIVI_sites_sf %>%
    mutate(
      latitude = st_coordinates(geometry)[,2],
      longitude = st_coordinates(geometry)[,1]
    ) %>%
    st_drop_geometry()

  output_df$CSD_geometry <- subdivisions$geometry[
    match(output_df$CSDUID_intersect, subdivisions$CSDUID)
  ]

  maps <- list()
  closest <- NULL
  intersect <- NULL
  for (i in seq_along(df$HarbourCode)) {
    message(i)
    if (!(output_df$CSDNAME_closest[i] == output_df$CSDNAME_intersect[i])) {
      KEEP_closest <- st_transform(subdivisions$geometry[which(subdivisions$CSDNAME == output_df$CSDNAME_closest[i] & subdivisions$CSDUID == output_df$CSDUID_closest[i])], crs = 4326) # FIXME: Look at i=64
      closest[[i]] <- KEEP_closest

      KEEP_intersect <- st_transform(subdivisions$geometry[which(subdivisions$CSDNAME == output_df$CSDNAME_intersect[i] & subdivisions$CSDUID == output_df$CSDUID_intersect[i])], crs = 4326) # FIXME: Look at i=6
      intersect[[i]] <- KEEP_intersect

      if (length(KEEP_closest) > 1 | length(KEEP_intersect) > 1) {
        message(i)
      }
      maps[[i]] <- leaflet() %>%
        addTiles() %>%
        addControl(
          paste0(
            df$HarbourName[i],
            ": ", i
          ),
          position = "topleft"
        ) %>%
        # Closest polygon
        addPolygons(
          data = KEEP_closest,
          color = "black",
          popup = paste0(
            "<b>CSD:</b> ",
            output_df$CSDNAME_closest[i]
          )
        ) %>%

        # Intersect polygon
        addPolygons(
          data = KEEP_intersect,
          color = "green",
          popup = paste0(
            "<b>CSD:</b> ",
            output_df$CSDNAME_intersect[i]
          )
        ) %>%
        # Harbour point
        addCircleMarkers(
          lat = df$Lat[i],
          lng = df$Long[i],
          color = "blue",
          popup = paste0(
            "<b>Harbour:</b> ", df$HarbourName[i],
            "<br>",
            "<b>Assigned CSD:</b> ", output_df$CSDNAME_closest[i]
          )
        )

    }
  }

  # if (plot) {
  #   return(maps)
  # }
  #tagList(maps)


  # STEP 2: TAKE A 1000 M BUFFER AROUND THE HARBOUR LOCATION
  message('step2')
  buffer <- NULL

  for (i in seq_along(df$Lat)) {
    point <- st_sfc(st_point(c(df$Long[i], df$Lat[i])), crs = 4326)  # WGS 84 CRS (lat/lon)

    # Transform the point to a CRS that uses meters (e.g., UTM Zone 20N)
    point_transformed <- st_transform(point, crs = 32620)  # UTM Zone 20N for this region

    # Create a 1000 meter buffer around the point
    buffer_polygon <- st_buffer(point_transformed, dist = 1000)
    buffer[[i]] <- st_transform(buffer_polygon, crs=4326)
  }


  # STEP 3: SEE THE FRACTION OF THE HARBOUR+BUFFER THAT INTERSECTS WITH
  # EACH CENSUS POLYGON
  inter_i <- NULL
  inter_c <- NULL

  for (i in seq_along(closest)) {
    if (!(is.null(closest[[i]]))) {
      #message(i)

      if (!(st_is_valid(intersect[[i]]))) {
        if (!(length(intersect[[i]]) == 0)) {
          intersect[[i]] <- st_make_valid(intersect[[i]])
        }
      }

      if (!(st_is_valid(closest[[i]]))) { # this has a length of 2 (i=64)
        if (!(length(closest[[i]]) == 0)) {
          closest[[i]] <- st_make_valid(closest[[i]])
        }
      }

      inter_i[[i]] <- st_area(st_intersection(buffer[[i]], intersect[[i]]))
      if (!(length(closest) == 0)) {
        inter_c[[i]] <- st_area(st_intersection(buffer[[i]],closest[[i]]))
      } else {
        # This means that even with the buffer there isn't an overlap with the neighbouring close polygon
        inter_c[[i]] <- 0
      }
    }
  }

  # See which is greater
  # if it's NULL, ignore completely
  # if one is length 0 it's the bigger one
  # if they both have a number, choose the bigger one

  # STEP 4: CHOOSE THE AREA THAT THE HARBOUR+BUFFER OVERLAPS WITH THE MOST
  message('step 4')
  final <- NULL
  # for (i in seq_along(inter_i)) {
  #   #message(i)
  #   if (!(is.null(inter_c[[i]]))) {
  #     if (length(inter_c[[i]]) == 0) {
  #       # The buffer doesn't overlap with close
  #       final[[i]] <- "intercept"
  #     } else {
  #       # they both have numbers
  #       if (inter_c[[i]] > inter_i[[i]]) {
  #         # strange case
  #         final[[i]] <- "close"
  #       } else {
  #         final[[i]] <- "intercept"
  #       }
  #     }
  #
  #   } else {
  #     # values were the same
  #     final[[i]] <- "intercept"
  #   }
  # }


  # STEP 4: CHOOSE THE AREA THAT THE HARBOUR+BUFFER OVERLAPS WITH THE MOST

  #browser()

  for (i in seq_along(inter_i)) {

    message(i)

    if (!is.null(inter_c[[i]])) {

      # Default = keep intersect polygon
      final[[i]] <- "intercept"

      # Skip if no candidate polygon
      if (length(inter_c[[i]]) == 0)
        next

      ############################################
      # NEW CONDITION
      ############################################

      # centroid of the "closest" polygon
      closest_centroid_geom <-
        subdivisions_centroids$geometry[
          match(
            output_df$CSDUID_closest[i],
            subdivisions_centroids$CSDUID
          )
        ]

      # intersect polygon geometry
      intersect_geom <-
        subdivisions$geometry[
          match(
            output_df$CSDUID_intersect[i],
            subdivisions$CSDUID
          )
        ]

      intersect_geom <- st_sfc(
        lapply(st_geometry(intersect_geom), function(mp) {

          # each MULTIPOLYGON can contain multiple polygons
          polys <- lapply(mp, function(poly) {

            # poly[[1]] = outer ring only (ignore holes)
            st_polygon(list(poly[[1]]))
          })

          st_multipolygon(polys)
        }),
        crs = st_crs(intersect_geom)
      )


      # ensure validity
      #intersect_geom <- st_make_valid(intersect_geom)

      # Is centroid(B) inside polygon(A)?
      centroid_inside <-
        lengths(
          st_within(
            closest_centroid_geom,
            intersect_geom
          )
        ) > 0

      ############################################

      if (
        inter_c[[i]] > inter_i[[i]] &&
        centroid_inside
      ) {
        final[[i]] <- "close"
      }

    } else {

      final[[i]] <- "intercept"

    }
  }



  final <- unlist(final)

  df <- st_as_sf(
    df,
    coords = c("Long", "Lat"),
    crs = 4326
  )

  df$CSDName <- 0
  df$CSDUID <- 0
  df$CSD_Shape <-  st_sfc(rep(st_geometrycollection(), nrow(df)), crs = 4326)

  for (i in seq_along(df$CSDName)) {
    #message(i)
    if (i %in% which(final == "close")) {
      df$CSDUID[i] <- output_df$CSDUID_closest[i]
      df$CSDName[i] <- output_df$CSDNAME_closest[i]
      df$CSD_Shape[i] <- output_df$CSD_geometry[i]
    } else {
      df$CSDUID[i] <- output_df$CSDUID_intersect[i]
      df$CSDName[i] <- output_df$CSDNAME_intersect[i]
      df$CSD_Shape[i] <- output_df$CSD_geometry[i]
    }
  }

  if (plot) {
    return(maps[grepl("close", final)])
  }


  return(df)
}
