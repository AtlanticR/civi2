#' Load and clean CIVI Sites data
#'
#' Reads the CIVI sites Excel file (2025-08-19 - SCH Harbours - Latitude and Longitude.xlsx),
#' selects key columns, renames them into a consistent format, and removes rows without
#' latitude/longitude.
#'
#' @return A data frame containing site information with columns:
#' \describe{
#'   \item{HarbourCode}{Unique harbour code}
#'   \item{HarbourName}{Name of the harbour}
#'   \item{Administration}{Administrative responsibility}
#'   \item{Province}{Province name}
#'   \item{HarbourType}{Type of harbour}
#'   \item{MarineInland}{Fixed value `"Marine"`}
#'   \item{Lat}{Latitude in decimal degrees}
#'   \item{Long}{Longitude in decimal degrees}
#'   \item{Zone}{Harbour region name}
#' }
#' @importFrom readxl read_excel
#' @export
#'
#' @examples
#' \dontrun{
#' sites <- data_CIVI_Sites()
#' head(sites)
#' }



data_CIVI_Sites <- function() {
  #file <-  read_excel(file.path(store, "data", "2025-08-19 - SCH Harbours - Latitude and Longitude.xlsx"))
  file <- read_excel(list.files(file.path(Sys.getenv("OneDriveCommercial"),"civi2"), full.names = TRUE)[which(grepl("2025-08-19",list.files(file.path(Sys.getenv("OneDriveCommercial"),"civi2"), full.names = TRUE)))])

  x <- data.frame(HarbourCode=file$`Harb Code`, HarbourName=file$`Harb Name`, Administration=file$`Harb Responsibility`, Province=file$`Harbour Province Name`,
                   HarbourType=file$`Harb Type Desc`, MarineInland="Marine", Lat=file$Latitude, Long=file$Longitude, Zone=file$`Harbour Region Name`)

  x <- x[-which(is.na(x$Lat) | is.na(x$Long)),]

  return(x)
}
