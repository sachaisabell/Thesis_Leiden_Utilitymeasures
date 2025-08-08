
library(cbsodataR)
library(sf)
library(dplyr)
library(giscoR)
library(osmdata)
library(ggplot2)
library(viridis)

## retrieve wijk coordinates from utrecht
gemeente_2023 <- cbs_get_sf("wijk", 2023)
plot(gemeente_2023$geometry, axes  = TRUE)

### get coordiinates from UTRECHT
utrecht_province <- gisco_get_nuts(nuts_level = 2, country = "NL", resolution = "3") %>%
  filter(NUTS_NAME == "Utrecht")

all_munis <- gisco_get_nuts(nuts_level = 3, country = "NL", resolution = "3")
utrecht_munis <- st_intersection(all_munis, utrecht_province)

bbox <- st_bbox(utrecht_province)

q <- opq(bbox = bbox) %>%
  add_osm_feature(key = "amenity", value = "school")

osm_data <- osmdata_sf(q)
school_points <- osm_data$osm_points

school_points <- st_transform(school_points, st_crs(utrecht_munis))

#### now create the counts
areal_data_func = function(points, areas){
  
  per_area = st_join(points, areas) %>%
    st_drop_geometry() %>%  # drop spatial geometry for grouping
    group_by(statnaam) %>% # assuming there is a column NAME_2 which indicated the name of the areas (provincce/municipality)
    summarise(count = n())
  
  linked = areas %>%
    left_join(per_area, by = c("statnaam" = "statnaam"))
  
  linked$count = ifelse(is.na(linked$count), 0, linked$count)
  
  return(linked)
  
}


### UTRECHT
UT_outline = provinces[provinces$NAME_1 == "Utrecht",] %>%st_transform(st_crs(gemeente_2023))
UT_wijken = st_intersection(UT_outline, gemeente_2023)
plot(UT_wijken$geometry)

school_points = st_transform(school_points, st_crs(gemeente_2023))
wijk_school_UT = areal_data_func(school_points, st_as_sf(UT_wijken))

ggplot() +
  geom_sf(data = wijk_school_UT, aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Number of Cities per Province with City Locations",
       fill = "City Count")


### Brabant
UT_outline = provinces[provinces$NAME_1 == "Noord-Brabant",] %>%st_transform(st_crs(gemeente_2023))
UT_wijken = st_intersection(UT_outline, gemeente_2023)
plot(UT_wijken$geometry)

school_points = st_transform(school_points, st_crs(gemeente_2023))
wijk_school_UT = areal_data_func(school_points, st_as_sf(UT_wijken))

ggplot() +
  geom_sf(data = wijk_school_UT, aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Number of Cities per Province with City Locations",
       fill = "City Count")

############
school_data_func = function(province){
  utrecht_province <- gisco_get_nuts(nuts_level = 2, country = "NL", resolution = "3") %>%
    filter(NUTS_NAME == province)
  
  all_munis <- gisco_get_nuts(nuts_level = 3, country = "NL", resolution = "3")
  utrecht_munis <- st_intersection(all_munis, utrecht_province)
  
  bbox <- st_bbox(utrecht_province)
  
  q <- opq(bbox = bbox) %>%
    add_osm_feature(key = "amenity", value = "school")
  
  osm_data <- osmdata_sf(q)
  school_points <- osm_data$osm_points
  
  return(school_points)
}

utrecht_point = school_data_func("Utrecht")

###########

UT_coords <- do.call(rbind, st_geometry(UT_point)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) %>% as.data.frame(UT_coords)
names(UT_dist) = c("x", "y")

UT_max = max(dist(UT_dist))
OV_max = max(dist(UT_dist))
NB_max = max(dist(UT_dist))


get_distances = function(province){
  
  pv_coords <- do.call(rbind, st_geometry(province)) %>% 
    as_tibble() %>% setNames(c("lon","lat")) %>% as.data.frame(pv_coords)
  names(pv_coords) = c("x", "y")
  
  distances = max(dist(pv_coords))
  
  return(list(
    small = 0.1 * distances,
    medium = 0.3 * distances,
    large = 0.5 * distances
  ))
}

UT_distances = get_distances(UT_point)
OV_distances = get_distances(OV_point)
NB_distances = get_distances(NB_point)

## now add the buffer
sample_UT = UT_point[1,"geometry"]

UT.w.bf = st_buffer(sample_UT, UT_distances$large) %>% st_intersection(UT_outline) 
UT.np = st_sample(UT.w.bf,1)
plot(UT.w.bf$geometry); plot(UT_outline$geometry, add = TRUE);plot(sample_UT, add= TRUE, col = "red");plot(UT.np, add = TRUE, col = "blue")

## now for 100 points
UT.100 = UT_point[1:100,"geometry"]
UT.100.bf = sapply(UT.100, function(x) st_buffer(x, UT_distances$large) %>% st_intersection(UT_outline))
UT.100.nps = sapply(UT.100.bf, function(x) st_sample(x, 1))


plot(UT_outline$geometry)
plot((UT.100.nps$geometry), add = TRUE, col = "blue") ; plot(UT_point, add = TRUE, col = "red")

plot(UT_outline$geometry)
lapply(UT.100.nps, function(x) plot(x, col = "blue", add = TRUE))

plot(OV_outline$geometry)
plot(OV_point$geometry, add= TRUE, col = "red")

plot(UT_outline$geometry)
plot(UT_point$geometry, add= TRUE, col = "red")

plot(NB_outline$geometry)
plot(NB_point$geometry, add= TRUE, col = "red")


#######
area_sdc_func = function(pv){
  
  province = get(paste0(pv, "_point"))
  outline = get(paste0(pv,"_outline"))
        
  distances = get_distances(province)
  pts = province[,"geometry"]
  
  ## small
  pts.bf = sapply(pts, function(x) {st_buffer(x, distances$small) %>% st_intersection(outline)})
  pv.small = sapply(pts.bf, function(x) st_sample(x, 1))
  
  ## medium
  pts.bf = sapply(pts, function(x) {st_buffer(x, distances$medium) %>% st_intersection(outline)})
  pv.medium = sapply(pts.bf, function(x) st_sample(x, 1))
  
  ## large
  pts.bf = sapply(pts, function(x) {st_buffer(x, distances$large) %>% st_intersection(outline)})
  pv.large = sapply(pts.bf, function(x) st_sample(x, 1))
  
  return(list(
    sdc_small = pv.small %>% point2df(outline),
    sdc_medium = pv.medium  %>% point2df(outline),
    sdc_large = pv.large  %>% point2df(outline)
  ))
}

point2df = function(data, outline){
  small_sf <- do.call(rbind, data) %>% as.data.frame()
  names(small_sf) <- c("x","y")
  
  df <- st_as_sf(small_sf, coords = c("x","y")) 
  st_crs(df) = st_crs(outline)
  output = st_transform(df, st_crs(outline))
  
  return(output)
}



UT_out = area_sdc_func("UT")

plot(UT_outline$geometry, axes = TRUE)
plot(UT_out$sdc_large, axes = TRUE, add = TRUE) 


























