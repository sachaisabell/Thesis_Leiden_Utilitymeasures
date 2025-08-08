
#file:///C:/Users/1glas/Downloads/Quality_assessment_and_object_matching_of_OpenStreetMap_in_.....pdf (on the accuracy of buildings about 90%)

library(cbsodataR)
library(sf)
library(dplyr)
library(giscoR)
library(osmdata)
library(ggplot2)
library(viridis)


### function to get school data per province
school_data_func = function(province){
  utrecht_province <- gisco_get_nuts(nuts_level = 2, country = "NL", resolution = "3") %>%
    filter(NUTS_NAME == province)
  
  #all_munis <- gisco_get_nuts(nuts_level = 3, country = "NL", resolution = "3")
  #utrecht_munis <- st_intersection(all_munis, utrecht_province)
  
  bbox <- st_bbox(utrecht_province) + c(-.1,-.1,.1,.1)

  q <- opq(bbox = bbox) %>%
    add_osm_feature(key = "amenity", value = "school")
  
  osm_data <- osmdata_sf(q)
  school_points <- osm_data$osm_points
  
  return(school_points)
}

#### get counts per area
areal_data_func = function(points, areas){
  
  per_area = st_join(points, areas) %>%
    st_drop_geometry() %>%  # drop spatial geometry for grouping
    group_by(statnaam) %>% # assuming there is a column statnaam which indicates the name of the areas (provincce/municipality)
    summarise(count = n())
  
  linked = areas %>%
    left_join(per_area, by = c("statnaam" = "statnaam"))
  
  linked$count = ifelse(is.na(linked$count), 0, linked$count)
  
  return(linked)
  
}

area_plot_func = function(dataset, all){
  ggplot() +
    geom_sf(data = dataset, aes(fill = count)) +
    scale_fill_gradient(name = "Value", low = "white", high = "black", limits = c(min(all), max(all))) +
    theme_minimal() +
    theme(legend.position = "right", axis.line.y.right = element_line(), axis.line.x.top = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white", color = NA))+
    labs(fill = "Value")
  
}

get_distances = function(province){
  
  # retrieve coordinates of all point on the original map
  pv_coords <- do.call(rbind, st_geometry(province)) %>% 
    as_tibble() %>% setNames(c("lon","lat")) %>% as.data.frame(pv_coords)
  names(pv_coords) = c("x", "y")
  
  # maximum distance between any two points
  distances = max(dist(pv_coords))
  
  return(list(
    small = 0.1 * distances,
    medium = 0.3 * distances,
    large = 0.5 * distances
  ))
}

# function to create the protected maps
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


############################################
set.seed(1)

#### Base data
## retrieve wijk coordinates and provinces
provinces <- st_read("C:/Users/1glas/Downloads/gadm41_NLD_shp (1)/gadm41_NLD_1.shp")
provinces$NAME_1[14] =  "Zuid-Holland"

gemeente_2023 <- cbs_get_sf("wijk", 2023)
plot(gemeente_2023$geometry, axes  = TRUE)


##### province outlines and wijken
UT_outline = provinces[provinces$NAME_1 == "Utrecht",] %>%st_transform(st_crs(gemeente_2023))
UT_wijken = st_intersection(UT_outline, gemeente_2023)

OV_outline = provinces[provinces$NAME_1 == "Overijssel",] %>%st_transform(st_crs(gemeente_2023))
OV_wijken = st_intersection(OV_outline, gemeente_2023)

NB_outline = provinces[provinces$NAME_1 == "Noord-Brabant",] %>%st_transform(st_crs(gemeente_2023))
NB_wijken = st_intersection(NB_outline, gemeente_2023)

##### coordinates for each province
UT_point = school_data_func("Utrecht")%>%st_transform(st_crs(gemeente_2023)) %>% st_intersection(UT_outline$geometry)
OV_point = school_data_func("Overijssel")%>%st_transform(st_crs(gemeente_2023)) %>% st_intersection(OV_outline$geometry)
NB_point = school_data_func("Noord-Brabant")%>%st_transform(st_crs(gemeente_2023)) %>% st_intersection(NB_outline$geometry)

UT_count = areal_data_func(UT_point, UT_wijken)
OV_count = areal_data_func(OV_point, OV_wijken)
NB_count = areal_data_func(NB_point, NB_wijken)


################# now for applying the SDC, small, medium, large
UT_out = area_sdc_func("UT")
OV_out = area_sdc_func("OV")
NB_out = area_sdc_func("NB")

UT_count_small = areal_data_func(UT_out$sdc_small, UT_wijken)
UT_count_medium = areal_data_func(UT_out$sdc_medium, UT_wijken)
UT_count_large = areal_data_func(UT_out$sdc_large, UT_wijken)

OV_count_small = areal_data_func(OV_out$sdc_small, OV_wijken)
OV_count_medium = areal_data_func(OV_out$sdc_medium, OV_wijken)
OV_count_large = areal_data_func(OV_out$sdc_large, OV_wijken)

NB_count_small = areal_data_func(NB_out$sdc_small, NB_wijken)
NB_count_medium = areal_data_func(NB_out$sdc_medium, NB_wijken)
NB_count_large = areal_data_func(NB_out$sdc_large, NB_wijken)

# necessary for the plotting
all_UT = c(UT_count_small$count, UT_count_medium$count, UT_count_large$count, UT_count$count)
all_OV = c(OV_count_small$count, OV_count_medium$count, OV_count_large$count, OV_count$count)
all_NB = c(NB_count_small$count, NB_count_medium$count, NB_count_large$count, NB_count$count)


###### and now the plots for each
UT_og_plot = area_plot_func(UT_count, all_UT)
UT_small_plot = area_plot_func(UT_count_small, all_UT)
UT_medium_plot = area_plot_func(UT_count_medium, all_UT)
UT_large_plot = area_plot_func(UT_count_large, all_UT)

OV_og_plot = area_plot_func(OV_count, all_OV)
OV_small_plot = area_plot_func(OV_count_small, all_OV)
OV_medium_plot = area_plot_func(OV_count_medium, all_OV)
OV_large_plot = area_plot_func(OV_count_large, all_OV)

NB_og_plot = area_plot_func(NB_count, all_NB)
NB_small_plot = area_plot_func(NB_count_small, all_NB)
NB_medium_plot = area_plot_func(NB_count_medium, all_NB)
NB_large_plot = area_plot_func(NB_count_large, all_NB)

UT_list = list(UT_count_small$count, UT_count_medium$count, UT_count_large$count, UT_count$count)
OV_list = list(OV_count_small$count, OV_count_medium$count, OV_count_large$count, OV_count$count)
NB_list = list(NB_count_small$count, NB_count_medium$count, NB_count_large$count, NB_count$count)

UT_geolist = list(UT_count, UT_count_small, UT_count_medium, UT_count_large)
OV_geolist = list(OV_count, OV_count_small, OV_count_medium, OV_count_large)
NB_geolist = list(NB_count, NB_count_small, NB_count_medium, NB_count_large)



#################################### saving the data tbs
# library("openxlsx")
# 
# all_area_data = rbind(UT_out$sdc_small, UT_out$sdc_medium, UT_out$sdc_large,
#                       OV_out$sdc_small, OV_out$sdc_medium, OV_out$sdc_large,
#                       NB_out$sdc_small, NB_out$sdc_medium, NB_out$sdc_large)
# all_area_data$pv = c(rep("UTs", 6102), rep("UTm", 6102), rep("UTl", 6102),
#                      rep("OVs", 6738), rep("OVm", 6738), rep("OVl", 6738),
#                      rep("NBs", 12047), rep("NBm", 12047), rep("NBl", 12047))
# 
# all_area_coord = do.call(rbind, all_area_data$geometry) %>% as.data.frame()
# all_area_coord$pv = all_area_data$pv
# 
# write.xlsx(all_area_coord, "area_maps_data_survey.xlsx")
# 
# 
# #### now saving all the plots
# sdc = c("og","small","medium","large")
# data_types = c("UT", "OV","NB")
# 
# for (i in 1:length(data_types)) {
#   for (j in 1:length(sdc)) {
#     
#     ggsave(paste0(data_types[i],"_", sdc[j],"_plot.png"), get(paste0(data_types[i],"_", sdc[j],"_plot"))) 
#     
#   }
# }


