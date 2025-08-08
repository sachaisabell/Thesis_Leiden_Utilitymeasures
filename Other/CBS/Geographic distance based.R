



### Displacement distance
# measures the euclidean distance point has been moved
# here original is a dataframe containing original x and y coordinates, and safe is x and y after SDC has been applied
# order of the columns does not matter as long as it's the same for both original and safe

displacement_distance = function(original, safe){
  sum(sqrt((original[,1] - safe[,2])^2 + (original[,1] - safe[,2])^2))
}



### KDW (or earth movers distance)
KWD_func = function(original, safe){
  # number of rows and column should be the same for original and safe datasets
  nr = nrow(original)
  nc = ncol(original)
  
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



### MDi 


https://stackoverflow.com/questions/57778268/how-to-get-the-max-distance-from-a-point-within-a-polygon-in-r





























