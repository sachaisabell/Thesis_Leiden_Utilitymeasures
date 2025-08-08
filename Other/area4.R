# Load libraries
library(sf)
library(dplyr)
library(giscoR)
library(osmdata)
library(ggplot2)
library(viridis)

# 1. Get Utrecht municipality boundaries
utrecht <- gisco_get_nuts(nuts_level = 2, country = "NL", resolution = "3") %>%
  filter(NUTS_NAME == "Utrecht")

utrecht_munis <- gisco_get_nuts(nuts_level = 3, country = "NL", resolution = "3") %>%
  st_intersection(utrecht)

# 2. Get bounding box and query schools in Utrecht
bbox <- st_bbox(utrecht)

q <- opq(bbox = bbox) %>%
  add_osm_feature(key = "amenity", value = "school")

osm_data <- osmdata_sf(q)
school_points <- osm_data$osm_points %>%
  st_transform(st_crs(utrecht_munis))

# 3. Spatial join: assign municipality to each school
schools_with_muni <- st_join(school_points, utrecht_munis, left = FALSE)

# 4. Extract city names (if available)
schools_with_muni$city <- schools_with_muni$`addr:city`

# 5. Count schools per municipality
schools_per_muni <- schools_with_muni %>%
  st_drop_geometry() %>%
  count(NUTS_NAME, name = "n_schools_muni") %>%
  arrange(desc(n_schools_muni))

# 6. Count schools per city (OSM tag)
schools_per_city <- schools_with_muni %>%
  st_drop_geometry() %>%
  filter(!is.na(city)) %>%
  count(city, name = "n_schools_city") %>%
  arrange(desc(n_schools_city))

# 7. Map schools per municipality (optional)
munis_with_counts <- left_join(utrecht_munis, schools_per_muni, by = "NUTS_NAME")

ggplot(munis_with_counts) +
  geom_sf(aes(fill = n_schools_muni)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Number of Schools per Municipality in Utrecht",
    fill = "Schools"
  ) +
  theme_minimal()

# 8. View or export data
head(schools_per_muni)
head(schools_per_city)

# Optional: save point data and counts
# st_write(schools_with_muni, "utrecht_schools_with_muni.gpkg")
# write.csv(schools_per_muni, "schools_per_municipality.csv", row.names = FALSE)
# write.csv(schools_per_city, "schools_per_city.csv", row.names = FALSE)

##########################
# Install packages (only once)
install.packages(c("sf", "dplyr", "giscoR", "osmdata", "ggplot2", "viridis"))

# Load libraries
library(sf)
library(dplyr)
library(giscoR)
library(osmdata)
library(ggplot2)
library(viridis)

# 1. Get Utrecht province polygon (NUTS level 2)
utrecht_province <- gisco_get_nuts(nuts_level = 2, country = "NL", resolution = "3") %>%
  filter(NUTS_NAME == "Utrecht")

# 2. Get NUTS level 3 municipalities, and subset only those in Utrecht
all_munis <- gisco_get_nuts(nuts_level = 3, country = "NL", resolution = "3")
utrecht_munis <- st_intersection(all_munis, utrecht_province)

# 3. Query OSM for school points within Utrecht's bounding box
bbox <- st_bbox(utrecht_province)

q <- opq(bbox = bbox) %>%
  add_osm_feature(key = "amenity", value = "school")

osm_data <- osmdata_sf(q)
school_points <- osm_data$osm_points

# 4. Transform coordinate system to match municipality polygons
school_points <- st_transform(school_points, st_crs(utrecht_munis))

# 5. Spatial join: assign municipality to each school point
schools_with_muni <- st_join(school_points, utrecht_munis, left = FALSE)

# 6. Extract city (if available) from OSM address tags
schools_with_muni$city <- schools_with_muni$`addr:city`

# 7. Count schools per municipality
schools_per_muni <- schools_with_muni %>%
  st_drop_geometry() %>%
  count(NUTS_NAME, name = "n_schools_muni") %>%
  arrange(desc(n_schools_muni))

# 8. Count schools per OSM-tagged city
schools_per_city <- schools_with_muni %>%
  st_drop_geometry() %>%
  filter(!is.na(city)) %>%
  count(city, name = "n_schools_city") %>%
  arrange(desc(n_schools_city))

# 9. Optional: join count back to municipality polygons and map
munis_with_counts <- left_join(utrecht_, schools_per_city, by = "NUTS_NAME")

ggplot(munis_with_counts) +
  geom_sf(aes(fill = n_schools_muni)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Number of Schools per Municipality in Utrecht Province",
    fill = "Schools"
  ) +
  theme_minimal()

# 10. Preview outputs
head(schools_per_muni)
head(schools_per_city)

# Optional: export results
# st_write(schools_with_muni, "utrecht_schools_with_municipalities.gpkg")
# write.csv(schools_per_muni, "schools_per_municipality.csv", row.names = FALSE)
# write.csv(schools_per_city, "schools_per_city.csv", row.names = FALSE)


#### THIS WORKS
UT = municipalities[municipalities$NAME_1 == "Utrecht",]
utrecht_schools = areal_data_func(school_points, UT)
plot(utrecht_schools)

ggplot(utrecht_schools) +
  geom_sf(aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Number of Schools per Municipality in Utrecht Province",
    fill = "Schools"
  ) +
  theme_minimal()




city_q <- opq(bbox = st_bbox(utrecht_province)) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "8")

city_polygons <- osmdata_sf(city_q)$osm_multipolygons

# Filter for valid place types
city_polygons <- city_polygons %>%
  filter(place %in% c("city", "town", "village"))


plot(city_polygons$geometry, axes = TRUE)

city_polygons$

cbs_get_sf(region= "wijk", year="2022")

cbs_maps <- cbs_get_maps()
unique(cbs_maps$region)

gemeente_2023 <- cbs_get_sf("wijk", 2023)
plot(gemeente_2023$geometry)
