library(leaflet)
library(sf)

# Ensure category is treated as a factor
civi$vulnw_cat <- as.factor(civi$vulnw_cat)

# Color palette
pal <- colorFactor(
  palette = c("green", "orange", "red"),
  domain = levels(civi$vulnw_cat)
)

leaflet(data = civi) %>%
  addTiles() %>%

  addCircleMarkers(
    radius = 6,
    stroke = TRUE,
    weight = 1,
    color = "black",
    fillOpacity = 0.8,
    fillColor = ~pal(vulnw_cat),
    popup = ~paste0(
      "<b>Category:</b> ", vulnw_cat
    )
  ) %>%

  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~vulnw_cat,
    title = "vulnw_cat",
    opacity = 1
  )
