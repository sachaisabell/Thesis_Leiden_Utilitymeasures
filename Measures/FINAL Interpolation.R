


library(MASS)
library(pracma)
library(FNN)

### whole function
leave_one_out_interpol <- function(i, coords, bw, lims) {
  xi <- coords[i, 1]
  yi <- coords[i, 2]
  others <- coords[-i, ]
  dens <- kde2d(others$x, others$y, h = bw, n = 200, lims = lims)
  interp2(x = dens$x, y = dens$y, Z = dens$z, xp = xi, yp = yi)
}

interpolation_func = function(map, method = c("interp", "nearest")){
  
  # for all points
  pts <- layer_data(map)[,1:2]  
  limits = c(map$coordinates$limits$x, map$coordinates$limits$y)
  
  bw_x <- MASS::bandwidth.nrd(pts$x) *0.1
  bw_y <- MASS::bandwidth.nrd(pts$y) *0.1
  
  # Full KDE with fixed bandwidth
  dens <- kde2d(pts$x, pts$y, h = c(bw_x, bw_y), n = 200, lims = limits)
  pts$density <- interp2(x = dens$x, y = dens$y, Z = dens$z, xp = pts$x, yp = pts$y) 
  
  if(method == "interp") pts$marks <- sapply(1:nrow(pts), leave_one_out_interpol, coords = pts[,1:2], bw = c(bw_x, bw_y), lims = limits)
  if(method == "nearest") {
    nn <- get.knn(pts[,1:2], k = 2)
    nearest_indices <- nn$nn.index[, 1] # pick the value of the first neighbor
    pts$marks <- pts$density[nearest_indices]
  }
  
  pts$difference <- log(abs(pts$marks - pts$density))
  pts$difference[pts$difference == -Inf] = 0
  
  return(pts)
}

avg_interpolation_func = function(map1, map2, method = c("interp", "nearest")){
  one = interpolation_func(map1, method = method)
  two = interpolation_func(map2, method = method)
  
  mean(abs(one$difference - two$difference))
}







































