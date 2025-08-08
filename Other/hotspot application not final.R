
consistent_maps$grid[[1]]
normal_maps$grid$original

grid_layer = layer_data(normal_maps$grid[[1]])
plot(grid_layer$xbin, grid_layer$ybin, col = (grid_layer$value + 1))

plot(grid_layer$x, grid_layer$y, col = grid_layer$value)

par(mfrow = c(1,1))


grid_layer2 = layer_data(normal_maps$grid[[2]])
plot(grid_layer2$x, grid_layer2$y, col = (grid_layer2$value+1))


layers = lapply(normal_maps$grid[1:5], layer_data)
xbins = lapply(layers, function(x) range(x[,2:3]))


lapply(normal_maps$grid[1:5], function(x) {
  new = layer_data(x) 
  print()
  #plot(new[,2], new[,3], col = c("red", rep("black",(length(new[,2]))-1)), pch = 20)
  })





normal_maps$grid[[1]]$coordinates$limits
normal_maps$grid[[2]]$coordinates$limits
normal_maps$grid[[3]]$coordinates$limits
normal_maps$grid[[4]]$coordinates$limits
normal_maps$grid[[5]]$coordinates$limits



map_raster = raster(nrow = 35, ncol = 35); extent(map_raster) = unlist(normal_maps$grid[[1]]$coordinates$limits)
values(map_raster) = rep(0,35*35)
map_raster[1,1]= 2

plot(map_raster)

34*35
length(grid_layer$value)







#####################
bounded_hotspots_hdr2 = function(plot, minimum){
  
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

hs.norm = bounded_hotspots_hdr2(normal_maps2$heatmap$original, 20)
plot(hs.norm)

hs.norm.safe = bounded_hotspots_hdr2(normal_maps2$heatmap$random, 20)
plot(hs.norm.safe)


output_measure2 = function(func, safe, og, result = "avg", ...){
  s2o = st_nearest_feature(safe$geometry, og$geometry)
  matches = data.frame("Safe ID" = 1:length(s2o),
                       "Closest Og ID" = s2o)
  
  # 'func' is the utility measure function, we apply to the matches dataset by taking per row the correspoding polygons/hotspots
  results = apply(matches, 1, function(x) func(safe[x[1],], og[x[2],], ...))
  
  if(result == "avg") return(mean(results))
  else if(result == "min") return(min(results))
  else if(result == "max") return(max(results))
  else return(results)
  
}

output_measure2(symmetric_diff_function, hs.norm.safe,hs.norm, "avg")

source("Hotspot based.R")


####### from the "Hotspot based.R"
hotspot_og = bounded_hotspots_hdr2(normal_maps2$heatmap$original, 10)
hotspot_safe = bounded_hotspots_hdr2(normal_maps2$heatmap$weighted, 10)

# symmetric difference: WORKS
output_measure(symmetric_diff_function, hotspot_safe,hotspot_og, "")

# hausdorff distance: WORKS
output_measure(HausdorffDistance_func, hotspot_safe, hotspot_og, "")

# frechet distance: WORKS
output_measure(frechet_distance_func, hotspot_og, hotspot_safe, "")

# correlation dimension: WORKS
output_measure(correlation_dim_func, hotspot_safe, hotspot_og, "", step = 0.001)

# boxcount: WORKS
boxcount1 = output_measure(box_counting_dim_func, hotspot_safe, hotspot_og, "", eps = 5) 
epsis = 2*2^(1:5-1) # 5 from the eps argument
y.safe = (boxcount1[[1]]$safe + boxcount1[[2]]$safe)/max(hotspot_og$clust)
y.og = (boxcount1[[1]]$original + boxcount1[[2]]$original)/max(hotspot_og$clust)
plot(epsis, y.safe, col = "blue", type = "l"); lines(epsis, y.og, col = "red")

# volume:
output_measure(volume_func, hotspot_safe, hotspot_og, "")



##### grid hotspots
(normal_maps$grid$original)

og.grid = grid_hotspot(normal_maps$raster_data$original$count, 1) 
og.safe = grid_hotspot(normal_maps$raster_data$random$count, 1) 

plot(og.grid)

################# and the hotspot area measure
library(lwgeom)
volume_func = function(sf.hs1, og.hs1){
  
  return(abs(st_area(sf.hs1) - st_area(og.hs1)))
}

circ_func = function(sf.hs1, og.hs1){
  
  return(abs(st_perimeter_lwgeom(sf.hs1) - st_perimeter_lwgeom(og.hs1)))
}














