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
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#'
#' siteBufferToMultiLineIntersection(
#'   sites = CCVI_RealPropertySites  |>
#'     filter(`Coastal or Inland`=="Coastal"),
#'   name_sites = "RPIMSSiteCode",
#'   sfLines = CANCOAST_TIDALRANGE_V6,
#'   name_sfLines_variable = "TideRange"
#' )
siteBufferToMultiLineIntersection <- function(sites, name_sites, buffer = 1000, sfLines, name_sfLines_variable, name_sfLines_geometry = "geometry", nearest = FALSE){
  # buffer the sites
  bufferedSites <- sites  |>
    sf::st_buffer(1000)

  # take the intersection
  intersection <- lapply(bufferedSites[[name_sfLines_geometry]],
                         function(geo){
                           #cropping before the intersection speeds things up slightly
                           cancoastcrop <- sf::st_crop(sfLines,geo)
                           suppressWarnings(sf::st_intersection(cancoastcrop,sf::st_sfc(geo,crs=st_crs(sites))))
                         })

  if ( any(lapply(intersection,nrow)==0)|nearest ) {
    if ( nearest ) {
      for ( i in 1:length(intersection) ){
        intersection[[i]] <- sfLines[st_nearest_feature(bufferedSites[[name_sfLines_geometry]][i],sfLines),]
      }
    } else {
      for ( i in which(lapply(intersection,nrow)==0) ){
        intersection[[i]] <- sfLines[st_nearest_feature(bufferedSites[[name_sfLines_geometry]][i],sfLines),]
      }
    }


  }
  names(intersection) <- bufferedSites[[name_sites]]

  # take the weighted mean of the
  lapply(intersection, as.data.frame)|>
    dplyr::bind_rows(.id=name_sites) |>
    sf::st_as_sf(crs=st_crs(sites)) |>
    dplyr::mutate(length=as.numeric(st_length(x=geometry))) |>
    dplyr::group_by(!!sym(name_sites)) |>
    dplyr::reframe("weighted.mean.{name_sfLines_variable}" := weighted.mean(!!sym(name_sfLines_variable),length),
                   geometry=st_combine(geometry)) |>
    dplyr::ungroup()
}
