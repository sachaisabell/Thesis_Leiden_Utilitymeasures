
library(dplyr)
library(raster)


# input is an image here saved in the same working directory
get_gray_value <- function(hex) {
  red <- substr(hex, 2, 3)           # Extract red component (RR)
  return(as.numeric(strtoi(red, base=16)))  # Convert hex to decimal
}

heatmap_func = function(safe, original, measure = c("simple", "ratio", "hedi"), title = NA){
  heat_safe = safe + theme(legend.position = "none",axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
  heat_original = original + theme(legend.position = "none",axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
  
  ggsave("plot_safe.png", plot = heat_safe, width = 8, height = 8, units = "in")
  ggsave("plot_original.png", plot = heat_original,  width = 8, height = 8, units = "in")
  
  img_safe <- magick::image_read("plot_safe.png") %>% magick:: image_convert(colorspace = "gray")  %>% raster::as.raster()
  img_original <- magick::image_read("plot_original.png") %>% magick:: image_convert(colorspace = "gray") %>% raster::as.raster()
  
  safe_values <- sapply(img_safe, get_gray_value)
  original_values <- sapply(img_original, get_gray_value)
  
  if(measure == "hedi") return(mean(abs(safe_values - original_values)))
  else if(measure == "ratio") {
    safe_values[safe_values == 0] <- 1e-5
    original_values[original_values == 0] <- 1e-5
    plot_vals <- log(original_values / safe_values)
    zlim <- c(-1, 1)
    col_palette <- gray.colors(256, start = 0, end = 1)
  }
  else if(measure == "simple") {
    plot_vals <- abs(safe_values - original_values)
    zlim <- c(0, 255)
    col_palette <- gray.colors(256, start = 0, end = 1)
  }
  
  width = dim(img_safe)[1]
  height = dim(img_safe)[2]
  
  grey.rast = raster(nrow = width, ncol = height)
  raster::extent(grey.rast) = c(0, width, 0, height) # assuming safe and original have the same dimensions
  raster::values(grey.rast) <- plot_vals
  par(mar = c(0, 0, 0, 0))  # No margins
  raster::plot(grey.rast, asp = 1, zlim = zlim, col = col_palette,
               axes = FALSE, box = FALSE, legend = FALSE, main = title)
  
}


#### examples
heatmap_func(normal_maps2$heatmap$original,normal_maps2$heatmap$random,"ratio")





############################## 
### using the kde2d function, makes a map that is not exactly the same as the desired map in the survey
############################## 
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

