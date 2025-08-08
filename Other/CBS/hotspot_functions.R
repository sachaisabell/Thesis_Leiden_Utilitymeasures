library(dplyr)
library(ggplot2)
library(lidaRtRee)
library(sf)
library(purrr)

################################################################################
### scale and centre function
################################################################################

scale.center.poly = function(polygon){
  # calculate centroid and center on origin
  centroid = st_centroid(polygon)
  c.polygon = polygon - centroid
  
  # calculate area and rescale
  area = st_area(polygon)
  sc.polygon = c.polygon * sqrt(1/area)
  
  return(sc.polygon)
}

################################################################################
### UNbounded hotspots
################################################################################

d2d = function(data, var1, var2, exp=0.1, minimum) {
  nn = 2
  # Create plot, but expand x and y ranges well beyond data
  p=ggplot(data, aes_string(var1, var2)) +
    geom_density_2d() +
    scale_x_continuous(limits=c(min(data[,var1]) - nn*diff(range(data[,var1])),
                                max(data[,var1]) + nn*diff(range(data[,var1])))) +
    scale_y_continuous(limits=c(min(data[,var2]) - nn*diff(range(data[,var2])),
                                max(data[,var2]) + nn*diff(range(data[,var2]))))
  
  # Get min and max x and y values among all density contours
  pb = ggplot_build(p)
  
  xyscales = lapply(pb$data[[1]][,c("x","y")], function(var) {
    rng = range(var) 
    rng + c(-exp*diff(rng), exp*diff(rng))
  })
  
  # Set x and y ranges to include complete density contours
  gg = ggplot(data, aes_string(var1, var2)) +
    #geom_density_2d_filled() +
    geom_density_2d_filled(contour = TRUE,breaks = c(minimum, 1)) +
    geom_density_2d() + #bins = 10) +
    scale_x_continuous(limits=xyscales[[1]]) +
    scale_y_continuous(limits=xyscales[[2]]) 
  return(gg)
}

unbounded_hotspots = function(df, minimum) {
  df.plot = d2d(df, "x","y", minimum = minimum) # minimum is the defined border of the 
  
  # extracting information from the plot
  ld.df = layer_data(df.plot)
  
  
  # separating the different polygons based on subgroup
  ld.df$pol <- paste0(ld.df$subgroup, "_", ld.df$group)
  ids <- unique(ld.df$pol)
  
  # Split contours based on the id
  pols <- lapply(ids, function(x){
    topol <- ld.df[ld.df$pol == x, ]
    closepol <- rbind(topol, topol[1, ])
    pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
    df <- unique(topol[, grepl("level", names(topol))])
    tofeatures <- st_as_sf(df, geometry=st_sfc(pol))
    return(tofeatures)
  })
  
  
  final_pols <- do.call(rbind, pols)
  
  # gets no overlapping, separate polygons, yay!
  parts <- st_cast(st_union(final_pols),"POLYGON")
  
  
  clust <- unlist(st_intersects(final_pols, parts))
  
  diss <- cbind(final_pols, clust) %>%
    group_by(clust) %>%
    summarize()
}


################################################################################
### bounded hotspots
################################################################################
bounded_hotspots = function(df, minimum){
  # create a plot using filled argument
  point.plot = ggplot(df, aes(x = x, y = y)) +
    geom_density_2d_filled(contour = TRUE, breaks = c(minimum, 1))
  
  # extracting information from the plot
  ld.df = layer_data(point.plot)
  
  # separating the different polygons based on subgroup
  ld.df$pol <- paste0(ld.df$subgroup, "_", ld.df$group)
  ids <- unique(ld.df$pol)
  
  # Split contours based on the id
  pols <- lapply(ids, function(x){
    topol <- ld.df[ld.df$pol == x, ]
    closepol <- rbind(topol, topol[1, ])
    pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
    df <- unique(topol[, grepl("level", names(topol))])
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


################################################################################
### Grid hotspots
################################################################################


grid_hotspot = function(matrix, value, adjacent = 8) {
  binary_grid <- matrix > value # always bigger than in case of a hotspot to decide the contour
  
  
  # Convert binary grid to raster object
  raster_grid <- raster(binary_grid)
  
  # Identify clusters of 1's (above value) using connected component labeling
  cluster_labels <- clump(raster_grid, directions = adjacent) # 4 is rooks case, 8 is queens
  
  # Convert clusters to polygons
  polygons <- rasterToPolygons(cluster_labels, fun=function(x){x > 0}, dissolve=TRUE)
  
  return(polygons)
}

##### HDR version
bounded_hotspots_hdr = function(df, minimum){
  # create a plot using filled argument
  point.plot = ggplot(df, aes(x = x, y = y)) +
    ggdensity:: geom_hdr(probs = minimum)
  
  # extracting information from the plot
  ld.df = layer_data(point.plot)
  
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
