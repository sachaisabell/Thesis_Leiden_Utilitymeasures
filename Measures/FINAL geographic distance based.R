



### Displacement distance
# measures the euclidean distance point has been moved
# here original is a dataframe containing original x and y coordinates, and safe is x and y after SDC has been applied
# order of the columns does not matter as long as it's the same for both original and safe

displacement_distance = function(original, safe){
  distances = (sqrt((original[,1] - safe[,1])^2 + (original[,2] - safe[,2])^2))
  mean(distances)
}



### KDW (or earth movers distance)
KWD_func = function(original, safe){
  # number of rows and column should be the same for original and safe datasets
  nr = 35
  nc = 35
  
  # creating coordinates based on dimensions of datasets/rasters
  xs = rep(1:nr,nc)
  ys = rep(1:nc, each = nr)
  coordinates = as.matrix(cbind(xs, ys))
  
  # spatial weights are taken to be the values of the raster cells
  w.original = c(original)
  w.safe = c(safe)
  weights = as.matrix(cbind(w.original, w.safe))
  
  SpatialKWD::compareOneToOne(coordinates, weights)$distance
  
}



### MDi --- only for point data

MDI_func = function(map, safe){
  # calculate the corners of the map, as one of these will be the furthest point away
  limits = data.frame(
    x = rep(map$coordinates$limits$x, 2),
    y = c(map$coordinates$limits$y, map$coordinates$limits$y[2], map$coordinates$limits$y[1])
  )

  mean_coords = colMeans(layer_data(map)[,1:2]) # mean coordinates of the original map
  mean_coords_safe = colMeans(layer_data(safe)[,1:2])
  
  # distances between the two means
  distbewteenmean = sqrt((mean_coords_safe[1] - mean_coords[1])^2 + (mean_coords_safe[2] - mean_coords[2])^2)
  
  # calculate the maximum distance from the original mean to the fathers points away
  distances2mean = max(apply(limits, 1, function(x) (sqrt((x[1] - mean_coords[1])^2 + (x[2] - mean_coords[2])^2))))

  return(distbewteenmean/distances2mean) 
  
  
}




















