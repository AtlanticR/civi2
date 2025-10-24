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

## Census communities
IDs <- y$CSDUID
library(sf)
library(dplyr)

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
