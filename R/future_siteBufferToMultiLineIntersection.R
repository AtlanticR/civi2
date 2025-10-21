#' Take the weighted.mean of an sf MULTILINESTRING variable inside a buffer around sites
#'
#' If there is no MULTILINESTRING within the buffer zone, this function will assign the nearest MULTILINESTRING to that site
#'
#' @param sites sf data.frame with site locations and names
#' @param name_sites character string for the column name that contains the site names/IDs in `sites`
#' @param buffer numeric value for the buffer zone around sites in meters. Defaults to 1000 m.
#' @param sfLines sf data.frame with the MULTILINESTRING and the variable of interest
#' @param name_sfLines_variable character string for the column name that contains the values of the variable of interest in `sfLines`
#' @param name_sfLines_geometry character string for the column name that contains the geometry column in `sfLines`
#' @param nearest logical to indicate if the function assigns the nearest MULTILINESTRING to that site instead of the default weighted average. (e.g. used for coastal materials)
#' @param n_cores number of cores to use for parallel processing. If NULL, uses all available cores minus one.
#' @param max_size maximum size of global variables allowed for future processing (in bytes). Default is 2000 MB.
#'
#' @returns sf data.frame
#' @export
#'
#' @examples
#'
#' future_siteBufferToMultiLineIntersection(
#'    sites = CCVI_RealPropertySites  |>
#'    filter(`Coastal or Inland`=="Coastal"),
#'    name_sites = "RPIMSSiteCode",
#'    sfLines = CANCOAST_TIDALRANGE_V6,
#'    name_sfLines_variable = "TideRange",
#'    n_cores = 4
#'    )
future_siteBufferToMultiLineIntersection <- function(sites, name_sites, buffer = 1000,
                                                     sfLines, name_sfLines_variable,
                                                     name_sfLines_geometry = "geometry",
                                                     nearest = FALSE,
                                                     n_cores = NULL,
                                                     max_size = 2000 * 1024^2) {  # 2000 MB default

  # Set up parallel processing
  if (is.null(n_cores)) {
    n_cores <- future::availableCores() - 1
  }

  # Save current setting and temporarily increase max size
  old_max_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = max_size)
  on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)

  # Buffer the sites
  bufferedSites <- sites |>
    sf::st_buffer(buffer)  # Fixed: was hardcoded to 1000

  # Parallel intersection computation
  plan(multisession, workers = n_cores)

  intersection <- future_lapply(seq_len(nrow(bufferedSites)),
                                function(i) {
                                  geo <- bufferedSites[[name_sfLines_geometry]][[i]]
                                  # Cropping before the intersection speeds things up
                                  cancoastcrop <- sf::st_crop(sfLines, geo)
                                  suppressWarnings(
                                    sf::st_intersection(cancoastcrop,
                                                        sf::st_sfc(geo, crs = st_crs(sites)))
                                  )
                                },
                                future.seed = TRUE)

  # Handle empty intersections or nearest feature
  empty_indices <- which(sapply(intersection, nrow) == 0)

  if (length(empty_indices) > 0 || nearest) {
    indices_to_update <- if (nearest) seq_len(length(intersection)) else empty_indices

    # Parallel nearest feature computation
    nearest_results <- future_lapply(indices_to_update,
                                     function(i) {
                                       sfLines[st_nearest_feature(
                                         bufferedSites[[name_sfLines_geometry]][i],
                                         sfLines
                                       ), ]
                                     },
                                     future.seed = TRUE)

    intersection[indices_to_update] <- nearest_results
  }

  # Reset to sequential processing
  plan(sequential)

  names(intersection) <- bufferedSites[[name_sites]]

  # Aggregate results
  lapply(intersection, as.data.frame) |>
    dplyr::bind_rows(.id = name_sites) |>
    sf::st_as_sf(crs = st_crs(sites)) |>
    dplyr::mutate(length = as.numeric(st_length(x = geometry))) |>
    dplyr::group_by(!!sym(name_sites)) |>
    dplyr::reframe("weighted.mean.{name_sfLines_variable}" :=
                     weighted.mean(!!sym(name_sfLines_variable), length),
                   geometry = st_combine(geometry)) |>
    dplyr::ungroup()
}
