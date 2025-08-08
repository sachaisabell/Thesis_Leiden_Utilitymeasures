

library(raster)
library(sf)

box_counting = function(eps, poly) {
  # Get tight bounding box and compute square
  bb = st_bbox(poly)
  bb_width = bb["xmax"] - bb["xmin"]
  bb_height = bb["ymax"] - bb["ymin"]
  bbsize = max(bb_width, bb_height) / 2
  
  xmid = (bb["xmax"] + bb["xmin"]) / 2
  ymid = (bb["ymax"] + bb["ymin"]) / 2
  
  # Square bounding box covering the polygon
  r <- raster(nrows = eps, ncol = eps, 
              xmn = xmid - bbsize, xmx = xmid + bbsize, 
              ymn = ymid - bbsize, ymx = ymid + bbsize)
  
  vals <- seq(1:length(r))
  
  r2 <- st_as_sf(rasterToPolygons(r))
  st_crs(r2) <- st_crs(poly)  # Ensure same CRS
  
  inter2 <- st_overlaps(poly, r2, byid = TRUE)
  ind <- inter2[[1]]  
  
  values(r) <- NA
  values(r)[ind] <- vals[ind]
  
  plot(r, axes = FALSE, legend = FALSE)
  plot(poly, add = TRUE)
  
  return(list("boxes" = length(ind),
              "size" = 2 * bbsize / eps,
              "eps" = eps))
}

box_counting_dim_func = function(safe, original, eps = 10){
  boxcount_safe = sapply(2*2^(1:eps-1), function (x) box_counting(x, safe$geometry))
  boxcount_og = sapply(2*2^(1:eps-1), function (x) box_counting(x, original$geometry))
  
  # usually log(1/size_of_box), but since size of box is seen as 1/eps (number of rows/cols), reduces to log(eps)
  bcdim_safe = log(unlist(boxcount_safe[1,])) / log(unlist(boxcount_safe[3,]))
  bcdim_og = log(unlist(boxcount_og[1,])) / log(unlist(boxcount_og[3,]))
  
  #plot(unlist(boxcount_safe[3,]), bcdim_safe, type = "l", col = "blue")
  #lines(unlist(boxcount_og[3,]), bcdim_og, col = "red")
  
  plot(log(unlist(boxcount_og[1,])), log(unlist(boxcount_og[3,])), type = "l", col = "blue")
  lines(log(unlist(boxcount_safe[1,])), log(unlist(boxcount_safe[3,])), col = "red")
  
  
  return(list("safe" = bcdim_safe,
              "original" = bcdim_og))
  
}



# first creating the data and the hotspots
hotspot_og = bounded_hotspots_hdr(normal_maps2$heatmap$original, 20)
hotspot_safe = bounded_hotspots_hdr(normal_maps2$heatmap$weighted, 20)

plot(hotspot_og)

boxout = box_counting_dim_func(hotspot_og[1,], hotspot_safe[1,], eps = 3)

# boxcount: WORKS
boxcount1 = output_measure(box_counting_dim_func, hotspot_safe, hotspot_og, "", eps = 5) 
epsis = 2*2^(1:5-1) # 5 from the eps argument
y.safe = (boxcount1[[1]]$safe + boxcount1[[2]]$safe)/max(hotspot_og$clust)
y.og = (boxcount1[[1]]$original + boxcount1[[2]]$original)/max(hotspot_og$clust)
plot(epsis, y.safe, col = "blue", type = "l"); lines(epsis, y.og, col = "red")




getwd()























