

## libraries
library(dplyr)
library(ggplot2)
library(lidaRtRee)
library(sf)
library(purrr)
library(sdcSpatial)
library(tigers)
library(lwgeom)
library(raster)


### general functions
scale.center.poly = function(polygon){
  # calculate centroid and center on origin
  centroid = st_centroid(polygon)
  c.polygon = polygon - centroid
  
  # calculate area and rescale
  area = st_area(polygon)
  sc.polygon = c.polygon * sqrt(1/area)
  
  return(sc.polygon)
}

bounded_hotspots_hdr = function(plot, minimum){
  
  # extracting information from the plot
  ld.df = layer_data(plot) 
  ld.df = ld.df[ld.df$probs >= paste0(minimum, "%"),]
  
  if(nrow(ld.df) == 0) stop("No hotspots due to minimum value")
  
  # separating the different polygons based on subgroup
  ld.df$pol <- paste0(ld.df$subgroup, "_", ld.df$group)
  ids <- unique(ld.df$pol)
  
  # Split contours based on the id
  pols <- lapply(ids, function(x){
    topol <- ld.df[ld.df$pol == x, ]
    closepol <- rbind(topol, topol[1, ])
    pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
    df <- unique(topol[, grepl("group", names(topol))])
    tofeatures <- st_as_sf(df, geometry=st_sfc(pol))
    return(tofeatures)
  })
  
  final_pols <- do.call(rbind, pols)
  
  # gets no overlapping, separate polygons
  parts <- st_cast(st_union(final_pols),"POLYGON")
  clust <- unlist(st_intersects(final_pols, parts))
  
  diss <- cbind(final_pols, clust) %>%
    group_by(clust) %>%
    summarize()
  
  return(diss)
}

output_measure = function(func, safe, og, result = "avg", ...){
  s2o = st_nearest_feature(st_centroid(safe)$geometry, st_centroid(og)$geometry)
  matches = data.frame("Safe ID" = 1:length(s2o),
                       "Closest Og ID" = s2o)
  
  # 'func' is the utility measure function, we apply to the matches dataset by taking per row the corresponding polygons/hotspots
  results = apply(matches, 1, function(x) func(safe[x[1],], og[x[2],], ...))
  
  if(result == "avg") return(mean(results))
  else if(result == "min") return(min(results))
  else if(result == "max") return(max(results))
  else return(results)
  
}


grid_hotspot = function(input, value, adjacent = 8) {
  rast = raster(nrow = 35, ncol = 35); extent(rast) = c(rep(c(0,1),2))
  values(rast) = input
  
  matrix = raster::as.matrix(rast)
  binary_grid <- matrix >= value # always bigger than in case of a hotspot to decide the contour
  
  # Convert binary grid to raster object
  raster_grid <- raster(binary_grid)
  
  # Identify clusters of 1's (above value) using connected component labeling
  cluster_labels <- clump(raster_grid, directions = adjacent) # 4 is rooks case, 8 is queens
  
  # Convert clusters to polygons
  polygons <- rasterToPolygons(cluster_labels, fun=function(x){x > 0}, dissolve=TRUE)
  
  return(st_as_sf(polygons))
}


coordinates_matrix = function(hotspot){
  st_cast(hotspot, "MULTIPOINT") %>% as("Spatial") %>% coordinates() %>% as.matrix()
}


######################### actual measures
###### Symmetric difference
symmetric_diff_function = function(sf.hs1, og.hs1){
  sc.og.hs = scale.center.poly(og.hs1$geometry)
  sc.sf.hs = scale.center.poly(sf.hs1$geometry)
  symdiff1 = st_sym_difference(sc.og.hs, sc.sf.hs)
  
  #plot(sc.sf.hs, axes = FALSE, col = "red") # the safe hotspots
  #plot(sc.og.hs, axes = FALSE, col = "blue")              # the original hotspots
  plot(symdiff1, col = "green")
  
  # the to calculate the area of the symmetric difference
  return(st_area(symdiff1))
}


###### circumference function
circ_func = function(sf.hs1, og.hs1){
  
  return(abs(st_perimeter_lwgeom(sf.hs1) - st_perimeter_lwgeom(og.hs1)))
}

###### volume function
volume_func = function(sf.hs1, og.hs1){
  
  return(abs(st_area(sf.hs1) - st_area(og.hs1)))
}























































