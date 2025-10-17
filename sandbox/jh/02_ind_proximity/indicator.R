tar_load(data_CIVI_Sites)
ors_api_key <- read.table(file.path(store,"data","ors_api_key.txt"))$V1
out <- ind_proximity(data_CIVI_Sites, ors_api_key=ors_api_key, full_results=TRUE)


# NEW TESTING
library(leaflet)
library(gridExtra)
library(htmltools)
for (i in seq_along(out$HarbourName)) {

  map1 <- out$Driving_Plot[[i]]
  map2 <- out$Sailing_Plot[[i]]

  # Arrange maps vertically
  browsable(tagList(
    tagList(map1, map2)
  ))


}



