
library(sf)
library(stars)

### first the interpolation function
interpolation_func = function(map, n = 100){
  # n is defines the grid resolution
  data = layer_data(map)[,1:2]
  
  xlim <- map$coordinates$limits$x
  ylim <- map$coordinates$limits$y
  
  # Compute density grids
  dens1 <- MASS::kde2d(data$x, data$y, n = n, lims = c(xlim, ylim))

  # create empty grid to plot the results onto
  rdens <- raster(nrows = n, ncol = n, xmn = xlim[1], ymn= ylim[1], xmx = xlim[2], ymx = ylim[2])
  values(rdens) = dens1$z
  
  plot(rdens)
  return((rdens))
  
}

npo = normal_maps$point$original
full = interpolation_func(npo) # all points are present

# now try to extract the values at the points
coordinates = layer_data(npo)[,1:2] %>% as.data.frame()
src = st_as_sf(coordinates, coords = c("x","y"))
st_extract(full, src)



#################
library(MASS)       # For kde2d
library(dplyr)      # For tidy manipulation
library(ggplot2)
library(pracma)  # For geom_hdr

# Your point data
pts <- layer_data(npo)[,1:2]  # Assuming this gives a data.frame with columns x and y

# Compute 2D density estimate on a grid
dens <- kde2d(pts$x, pts$y, n = 200)  # n = grid resolution (increase for finer resolution)

# Interpolate using bilinear interpolation (approx)
pts$density <- interp2(x = dens$x, y = dens$y, Z = dens$z, xp = x, yp = y)

# Plot points colored by estimated density
ggplot(pts, aes(x = x, y = y, color = density)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal()




##############3
# Use MASS::bandwidth.nrd to get fixed bandwidths
pts <- layer_data(normal_maps$point$original)[,1:2]  

bw_x <- MASS::bandwidth.nrd(pts$x) *0.1
bw_y <- MASS::bandwidth.nrd(pts$y) *0.1

# Full KDE with fixed bandwidth
dens <- kde2d(pts$x, pts$y, h = c(bw_x, bw_y), n = 200)
pts$density <- interp2(x = dens$x, y = dens$y, Z = dens$z, xp = pts$x, yp = pts$y)

# Leave-one-out with same bandwidth
leave_one_out_interpol <- function(i, coords, bw) {
  xi <- coords[i, 1]
  yi <- coords[i, 2]
  others <- coords[-i, ]
  dens <- kde2d(others$x, others$y, h = bw, n = 200)
  interp2(x = dens$x, y = dens$y, Z = dens$z, xp = xi, yp = yi)
}

# Run LOO estimates
densities <- sapply(1:nrow(pts), leave_one_out_interpol, coords = pts[,1:2], bw = c(bw_x, bw_y))
pts$marks <- densities
pts$difference <- abs(pts$marks - pts$density)

p1 = ggplot(pts, aes(x = x, y = y, color = (density))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal()

p2 = ggplot(pts, aes(x = x, y = y, color = (marks))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal()

p3 = ggplot(pts, aes(x = x, y = y, color = log(difference))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal()

library(gridExtra)
grid.arrange(p1,p2,p3, ncol = 1)



### whole function
leave_one_out_interpol <- function(i, coords, bw) {
  xi <- coords[i, 1]
  yi <- coords[i, 2]
  others <- coords[-i, ]
  dens <- kde2d(others$x, others$y, h = bw, n = 200, lims = c(map$coordinates$limits$x, map$coordinates$limits$y))
  interp2(x = dens$x, y = dens$y, Z = dens$z, xp = xi, yp = yi)
}

interpolation_func = function(map){
  
  # for all points
  pts <- layer_data(map)[,1:2]  
  
  bw_x <- MASS::bandwidth.nrd(pts$x) *0.1
  bw_y <- MASS::bandwidth.nrd(pts$y) *0.1
  
  # Full KDE with fixed bandwidth
  dens <- kde2d(pts$x, pts$y, h = c(bw_x, bw_y), n = 200, lims = c(map$coordinates$limits$x, map$coordinates$limits$y))
  pts$density <- interp2(x = dens$x, y = dens$y, Z = dens$z, xp = pts$x, yp = pts$y, method = "nearest")
  
  # now leaving each point out
  pts$marks <- sapply(1:nrow(pts), leave_one_out_interpol, coords = pts[,1:2], bw = c(bw_x, bw_y))
  pts$difference <- log(abs(pts$marks - pts$density))
  pts$difference[pts$difference == -Inf] = 0
  
  return(pts)
}

one1 = interpolation_func(normal_maps$point$original)

ggplot(one1, aes(x = x, y = y, color = (difference))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal()

##################

wins = owin(xrange =map$coordinates$limits$x, yrange = map$coordinates$limits$y)
og.ppp = ppp(normal_data$x, normal_data$y, window = wins, marks = one1$density)
idw.og = spatstat.explore::idw(og.ppp, at= "points", p = 15)
plot(idw.og)


abs(idw.og - one1$density)

ggplot(one1, aes(x = x, y = y, color = (abs(density - idw.og)/(density + marks)/2))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal() + theme(legend.position = "none")

########################
# Required libraries
library(spatstat.explore)
library(ggplot2)
library(viridis)
library(spatstat)

# Leave-one-out using spatstat density.ppp
leave_one_out_spatstat <- function(i, coords, sigma, win) {
  others <- coords[-i, ]
  pp_others <- ppp(others$x, others$y, window = win)
  dens <- density(pp_others, sigma = sigma, edge = TRUE, dimyx = 200)
  xi <- coords[i, 1]
  yi <- coords[i, 2]
  return(spatstat.geom::interp.im(dens, xp = xi, yp = yi))
}

interpolation_func_spatstat <- function(map) {
  pts <- layer_data(map)[, 1:2]
  
  # Create spatstat window
  xlim <- map$coordinates$limits$x
  ylim <- map$coordinates$limits$y
  win <- owin(xrange = xlim, yrange = ylim)
  
  # Convert to ppp
  pp <- ppp(pts$x, pts$y, window = win)
  
  # Bandwidth (smaller = more sensitive)
  sigma <- bw.diggle(pp) * 0.5
  
  # Full density
  dens <- density(pp, sigma = sigma, edge = TRUE, dimyx = 200)
  pts$density <- spatstat.geom::interp.im(dens, xp = pts$x, yp = pts$y)
  
  # Leave-one-out
  pts$marks <- sapply(1:nrow(pts), leave_one_out_spatstat, coords = pts, sigma = sigma, win = win)
  
  # Log-difference
  pts$difference <- log(abs(pts$marks - pts$density))
  pts$difference[is.infinite(pts$difference)] <- 0
  
  return(pts)
}

second = interpolation_func_spatstat(normal_maps$point$original)

ggplot(one1, aes(x = x, y = y, color = (abs(density - idw.og)/(density + marks)/2))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal() + theme(legend.position = "none")


############### KNN
library(FNN)
library(ggplot2)
library(viridis)

# KNN-based density
knn_dens <- function(xy, k) {
  d <- get.knn(xy, k = k)
  return(1 / rowMeans(d$nn.dist))
}

# Leave-one-out KNN density
leave_one_out_knn <- function(i, xy, k) {
  others <- xy[-i, , drop = FALSE]
  d <- get.knnx(others, xy[i, , drop = FALSE], k = k)
  return(1 / mean(d$nn.dist))
}

interpolation_func_knn <- function(map, k = 10) {
  pts <- layer_data(map)[, 1:2]
  xy <- as.matrix(pts)
  
  # Full KNN density
  pts$density <- knn_dens(xy, k = k)
  
  # Leave-one-out
  pts$marks <- sapply(1:nrow(pts), leave_one_out_knn, xy = xy, k = k)
  
  # Log-difference
  pts$difference <- log(abs(pts$marks - pts$density))
  pts$difference[is.infinite(pts$difference)] <- 0
  
  return(pts)
}

third = interpolation_func_knn(normal_maps$point$original)


#################################
# tryo some more nearest neighbors but very simple


# Install if you don't have it
# install.packages("FNN")

library(FNN)

# Your data
df <- data.frame(
  x = cpo$x,
  y = cpo$y,
  value = cpo$density
)

# Find the nearest neighbor (excluding self)
# k = 2 means the nearest neighbor including itself and one closest other point
nn <- get.knn(df[, c("x", "y")], k = 2)

# nn$nn.index has indices of nearest neighbors for each point
# The first neighbor is the point itself, so take the second neighbor's index
nearest_indices <- nn$nn.index[, 2]

# Get the value of the nearest neighbor for each point
df$nearest_value <- df$value[nearest_indices]

print(df)



ggplot(df, aes(x = x, y = y, color = log(abs(value - nearest_value)))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal() + theme(legend.position = "none")

ggplot(cpo, aes(x = x, y = y, color = difference)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Estimated KDE Density at Each Point") +
  theme_minimal() + theme(legend.position = "none")












