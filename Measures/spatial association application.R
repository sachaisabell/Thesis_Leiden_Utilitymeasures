
#### Applying global measures to areal maps
### VMR
sapply(UT_list[2:4], function(x) VMR_function(UT_list[[1]], x))
sapply(OV_list[2:4], function(x) VMR_function(OV_list[[1]], x))
sapply(NB_list[2:4], function(x) VMR_function(NB_list[[1]], x))

### MMR
sapply(UT_list[2:4], function(x) MMR_function(UT_list[[1]], x, option = "MMR"))
sapply(OV_list[2:4], function(x) MMR_function(OV_list[[1]], x, option = "MMR"))
sapply(NB_list[2:4], function(x) MMR_function(NB_list[[1]], x, option = "MMR"))

### MMR*
sapply(UT_list[2:4], function(x) MMR_function(UT_list[[1]], x, option = "MMR*"))
sapply(OV_list[2:4], function(x) MMR_function(OV_list[[1]], x, option = "MMR*"))
sapply(NB_list[2:4], function(x) MMR_function(NB_list[[1]], x, option = "MMR*"))

### MMR**
sapply(UT_list[2:4], function(x) MMR_function(UT_list[[1]], x, option = "MMR**"))
sapply(OV_list[2:4], function(x) MMR_function(OV_list[[1]], x, option = "MMR**"))
sapply(NB_list[2:4], function(x) MMR_function(NB_list[[1]], x, option = "MMR**"))

# morans I
sapply(UT_geolist[1:4], function(x) moransI_area_func(x, version = "global"))
sapply(OV_geolist[1:4], function(x) moransI_area_func(x, version = "global"))
sapply(NB_geolist[1:4], function(x) moransI_area_func(x, version = "global"))

# GEARY'S c
sapply(UT_geolist[1:4], function(x) Geary_area_func(x, version = "global"))
sapply(OV_geolist[1:4], function(x) Geary_area_func(x, version = "global"))
sapply(NB_geolist[1:4], function(x) Geary_area_func(x, version = "global"))

# getis ord
sapply(UT_geolist[1:4], function(x) Gi_area_func(x, version = "global"))
sapply(OV_geolist[1:4], function(x) Gi_area_func(x, version = "global"))
sapply(NB_geolist[1:4], function(x) Gi_area_func(x, version = "global"))


##### Creating local maps for areal data
titles = c("Original", "Small", "Medium", "Large")

local_area_func = function(dataset, all, vals, title){
  ggplot() +
    geom_sf(data = dataset, aes(fill = vals)) +
    scale_fill_gradient(name = "Value", low = "white", high = "black", limits = c(min(all), max(all))) +
    theme_minimal() +
    theme(legend.position = "right", axis.line.y.right = element_line(), axis.line.x.top = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white", color = NA), plot.title = element_text(size = 30))+
    labs(fill = "Value") + 
    ggtitle(title)
  
}

# Gi statistic
GiUT = sapply(UT_geolist[1:4], function(x) Gi_area_func(x, version = "local")) 
GiOV = sapply(OV_geolist[1:4], function(x) Gi_area_func(x, version = "local"))
GiNB = sapply(NB_geolist[1:4], function(x) Gi_area_func(x, version = "local"))

# Gi* 
GisUT = sapply(UT_geolist[1:4], function(x) Gi_area_func(x, version = "local", star = TRUE)) 
GisOV = sapply(OV_geolist[1:4], function(x) Gi_area_func(x, version = "local", star = TRUE))
GisNB = sapply(NB_geolist[1:4], function(x) Gi_area_func(x, version = "local", star = TRUE))

# local Moran's I
lmUT = sapply(UT_geolist[1:4], function(x) moransI_area_func(x, version = "local")) 
lmOV = sapply(OV_geolist[1:4], function(x) moransI_area_func(x, version = "local"))
lmNB = sapply(NB_geolist[1:4], function(x) moransI_area_func(x, version = "local"))

# localgeary C
gcUT = sapply(UT_geolist[1:4], function(x) Geary_area_func(x, version = "local")) 
gcOV = sapply(OV_geolist[1:4], function(x) Geary_area_func(x, version = "local"))
gcNB = sapply(NB_geolist[1:4], function(x) Geary_area_func(x, version = "local"))

measure_names = c("Gi","Gis","lm","gc")
data_types = c("UT","OV","NB")

for (i in 1:length(measure_names)) {
  for (j in 1:length(data_types)) {
    current = get(paste0(measure_names[i],data_types[j])) 
    
    current_geo = get(paste0(data_types[j],"_geolist"))
    
    og = local_area_func(current_geo[[1]], current, current[,1], titles[1])
    sm = local_area_func(current_geo[[1]], current, current[,2], titles[2])
    md = local_area_func(current_geo[[1]], current, current[,3], titles[3])
    la = local_area_func(current_geo[[1]], current, current[,4], titles[4])
    
    jpeg(paste0(measure_names[i],data_types[j],".jpeg"), width = 1200, height = 350)
    grid.arrange(grobs = list(og, sm, md, la), layout_matrix = lay)
    dev.off()
  }
}


#### Applying global measures to grid maps
### VMR
sapply(normal_maps$raster_data[2:5], function(x) VMR_function(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) VMR_function(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) VMR_function(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) VMR_function(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) VMR_function(consistent_maps$raster_data$original$count, x$count))

### MMR
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count))

### MMR*
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count, option = "MMR*"))

### MMR*
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count, option = "MMR**"))

### Morans I global
sapply(normal_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))

### Getis ord global
sapply(normal_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))

### Geary global
sapply(normal_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))


############ Now local for the grid maps
titles = c("Original", "Random",'Grid',"Vonoroi","Weighted")
datasets = c("norm","clus", "rand", "small","cons")

## Local morans I
lmnorm = sapply(normal_maps$raster_data, function(x) moransI_grid_func(x$count, version = "local", type = "queen"))
lmclus = sapply(clustered_maps$raster_data, function(x) moransI_grid_func(x$count, version = "local", type = "queen"))
lmrand = sapply(random_maps$raster_data, function(x) moransI_grid_func(x$count, version = "local", type = "queen"))
lmsmall = sapply(smallclusters_maps$raster_data, function(x) moransI_grid_func(x$count, version = "local", type = "queen"))
lmcons = sapply(consistent_maps$raster_data, function(x) moransI_grid_func(x$count, version = "local", type = "queen"))

# geary C
gcnorm = sapply(normal_maps$raster_data, function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gcclus = sapply(clustered_maps$raster_data, function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gcrand = sapply(random_maps$raster_data, function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gcsmall = sapply(smallclusters_maps$raster_data, function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gccons = sapply(consistent_maps$raster_data, function(x) Geary_grid_func(x$count, version = "local", type = "queen"))

# Gi
ginorm = sapply(normal_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen"))
giclus = sapply(clustered_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen"))
girand = sapply(random_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen"))
gismall = sapply(smallclusters_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen"))
gicons = sapply(consistent_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen"))

# Gi*
gisnorm = sapply(normal_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen", star = TRUE))
gisclus = sapply(clustered_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen", star = TRUE))
gisrand = sapply(random_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen", star = TRUE))
gissmall = sapply(smallclusters_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen", star = TRUE))
giscons = sapply(consistent_maps$raster_data, function(x) Gi_grid_func(x$count, version = "local", type = "queen", star = TRUE))


measure_names = c("gi","gis","lm","gc")
data_types = c("norm","clus","rand","small","cons")
titles = c("Original", "Random",'Grid',"Vonoroi","Weighted")

for (i in 1:length(measure_names)) {
  for (j in 1:length(data_types)) {
    current = get(paste0(measure_names[i],data_types[j])) ;print(current)
    
    for (k in 1:5) {
      assign(paste0("plot",titles[k]), local_grid_func(current[,k], c(current), titles[k]))
    }
    
    
    jpeg(paste0(measure_names[i],data_types[j],".jpeg"), width = 2000, height = 350)
    grid.arrange(grobs = list(plotOriginal, plotRandom, plotGrid, plotVonoroi, plotWeighted), ncol = 5)
    dev.off()
  }
  
  
  
}
