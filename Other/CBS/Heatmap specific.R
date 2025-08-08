





heatmap_func = function(safe, original, measure = c("simple", "ratio", "hedi"), n = 100){
  # n is defines the grid resolution
  
  xlim <- c(min(safe$x, original$x), max(safe$x, original$x))
  ylim <- c(min(safe$y, original$y), max(safe$y, original$y))
  
  # Compute density grids
  dens1 <- MASS::kde2d(safe$x, safe$y, n = n, lims = c(xlim, ylim))
  dens2 <- MASS::kde2d(original$x, original$y, n = n, lims = c(xlim, ylim))
  
  # create empty grid to plot the results onto
  rdens <- raster(nrows = n, ncol = n, xmn = xlim[1], ymn= ylim[1], xmx = xlim[2], ymx = ylim[2])
  
  if(measure == "hedi") return(mean(abs(dens1$z - dens2$z)))
  else if(measure == "ratio") plot_vals = log(dens2$z / dens1$z)
  else if(measure == "simple") plot_vals = abs(dens1$z - dens2$z)
  
  # for ratio and simple plot the results
  par(mfrow = c(1,3))
  values(rdens) = dens1$z; plot(rdens) 
  values(rdens) = dens2$z; plot(rdens) 
  values(rdens) = plot_vals; plot(rdens)
  
}


#### example
data_og = dwellings[1:5000,1:2]
data_safe = mask_grid(data_og[1:1000,], r = 100)

heatmap_func(data_safe, data_og, "simple")

