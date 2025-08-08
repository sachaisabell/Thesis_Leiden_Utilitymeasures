


rep_function = function(measure, type = c("avg", "max")){
  datanames = c("normal", "clustered", "random", "smallclusters", "consistent")
  out = data.frame(rbind(rep(NA, 4),rep(NA, 4),rep(NA, 4),rep(NA, 4), rep(NA, 4)))
  rownames(out) = datanames
  
  for (i in 1:length(datanames)) {
    current_hotspot = get(paste0(datanames[i], "_hotspots"))
    out[i,] = c(sapply(current_hotspot[2:5], function(x) output_measure(measure, x, current_hotspot[[1]], type)))
    
    
  }
  
  return(out)
}
#### making the hotspots
normal_hotspots = lapply(normal_maps2$heatmap[1:5], function(x) bounded_hotspots_hdr(x, minimum = 60))
clustered_hotspots = lapply(clustered_maps2$heatmap[1:5], function(x) bounded_hotspots_hdr(x, minimum = 60))
random_hotspots = lapply(random_maps2$heatmap[1:5], function(x) bounded_hotspots_hdr(x, minimum = 60))
smallclusters_hotspots = lapply(smallclusters_maps2$heatmap[1:5], function(x) bounded_hotspots_hdr(x, minimum = 60))
consistent_hotspots = lapply(consistent_maps2$heatmap[1:5], function(x) bounded_hotspots_hdr(x, minimum = 60))


##### symmetric difference 
View(rep_function(symmetric_diff_function, type = "avg"))
View(rep_function(symmetric_diff_function, type = "max"))

##### circumference
View(rep_function(circ_func, type = "avg"))
View(rep_function(circ_func, type = "max"))

##### volume
View(rep_function(volume_func, type = "avg"))
View(rep_function(volume_func, type = "max"))

##### frechet
View((rep_function(frechet_distance_func, type = "avg")))
View((rep_function(frechet_distance_func, type = "max")))

##### hausdorff
View((rep_function(HausdorffDistance_func, type = "avg")))
View((rep_function(HausdorffDistance_func, type = "max")))

##### correlation dimension
View((rep_function(correlation_dim_func,type = "avg")))
View((rep_function(correlation_dim_func,type = "max")))

##### boxcount dimension
boxcount_plot = function(original, safe,title, eps = 5){
  
  plot(original, c(log(2*2^(1:eps-1))), type = "l", col = "blue", main = title, xlab = "log(N)", ylab = "Size")
  lines(safe, c(log(2*2^(1:5-1))), type = "l", col = "red")
}

datanames = c("normal","clustered")#, "random", "smallclusters", "consistent")
box_list = list()

for (i in 1:length(datanames)) {
  current_hotspot = get(paste0(datanames[i], "_hotspots"))
  out = lapply(current_hotspot[2:5], function(x) output_measure(box_counting_dim_func, x, current_hotspot[[1]], ""))
  
  # averaging over the lines to get a single line for safe and for original
  random.s = rowMeans(sapply(out$random, function(x) x$Nsafe))
  grid.s = rowMeans(sapply(out$grid, function(x) x$Nsafe))
  vonoroi.s = rowMeans(sapply(out$vonoroi, function(x) x$Nsafe))
  weighted.s = rowMeans(sapply(out$weighted, function(x) x$Nsafe))
  random.o = rowMeans(sapply(out$random, function(x) x$Nog))
  grid.o = rowMeans(sapply(out$grid, function(x) x$Nog))
  vonoroi.o = rowMeans(sapply(out$vonoroi, function(x) x$Nog))
  weighted.o = rowMeans(sapply(out$weighted, function(x) x$Nog))
  
  par(mfrow = c(1,4))
  boxcount_plot(random.o, random.s, title = paste(datanames[i],"-","Random"))
  boxcount_plot(grid.o, grid.s, title = paste(datanames[i],"-","Grid"))
  boxcount_plot(vonoroi.o, vonoroi.s, title = paste(datanames[i],"-","Vonoroi"))
  boxcount_plot(weighted.o, weighted.s, title = paste(datanames[i],"-","Weighted"))
  
}
names(box_list) <- datanames


#### angles measure

rep_function_grid = function(measure, type = c("avg", "max")){
  datanames = c("normal", "clustered", "random", "smallclusters", "consistent")
  out = data.frame(rbind(rep(NA, 4),rep(NA, 4),rep(NA, 4),rep(NA, 4), rep(NA, 4)))
  rownames(out) = datanames
  
  for (i in 1:length(datanames)) {
    current_hotspot = get(paste0(datanames[i], "_hotspots_grid"))
    out[i,] = c(sapply(current_hotspot[2:5], function(x) output_measure(measure, x, current_hotspot[[1]], type)))
    
    
  }
  
  return(out)
}
#### making the hotspots
normal_hotspots_grid = lapply(normal_maps2$raster_data[1:5], function(x) grid_hotspot(x$count))
clustered_hotspots_grid = lapply(clustered_maps2$raster_data[1:5], function(x) grid_hotspot(x$count))
random_hotspots_grid = lapply(random_maps2$raster_data[1:5], function(x)grid_hotspot(x$count))
smallclusters_hotspots_grid = lapply(smallclusters_maps2$raster_data[1:5], function(x) grid_hotspot(x$count))
consistent_hotspots_grid = lapply(consistent_maps2$raster_data[1:5], function(x) grid_hotspot(x$count))


View(t(rep_function_grid(angle_measure_func, "avg")))
View(t(rep_function_grid(angle_measure_func, "max")))


















