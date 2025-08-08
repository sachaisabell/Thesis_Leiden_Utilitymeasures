
##### initial functions
plot_func = function(data, title, type, xlimits, ylimits,bin = NULL, fill_limits = NULL){
  
  p = ggplot(data, aes(x, y))
  
  if(type == "point") p= p + geom_point()
  if(type == "heatmap") p=p + ggdensity::geom_hdr(probs = seq(0.00001,0.99,0.1)) 
  
  if(type == "grid") p=p + stat_bin2d(bins = 35,binwidth = bin, drop = FALSE) + scale_fill_viridis_c(name = "Value", option = "C", limits = fill_limits, na.value = "blue")
  
  plot = p + 
    theme_classic()+
    theme(aspect.ratio = 1, legend.position = "right") +
    #ggtitle(title) +
    coord_cartesian(xlim = xlimits, ylim = ylimits)
  #xlim(xlimits) + ylim(ylimits) 

  
  return(plot)
}

p.g.h.maps = function(data1){
  maxdist = max(dist(data1))
  
  # defining the datasets
  random1 = mask_random(data1[,1:2], 0.1 * maxdist)
  grid1 = mask_grid(data1[,1:2], diff(range(data1$x))/80)
  vonoroi1 = mask_voronoi(data1[,1:2])
  weighted1 = mask_weighted_random(data1[,1:2], k=5, r= 0.5* maxdist)
  
  df_all = rbind(data1, random1, grid1, vonoroi1, weighted1)
  
  xlim = range(df_all[,1])
  ylim = range(df_all[,2])
  
  data_list <- list(original = data1, 
                    random = random1, 
                    grid = grid1, 
                    vonoroi = vonoroi1, 
                    weighted = weighted1)
  
  # point plots
  pplots1 <- lapply(names(data_list), function(nm) plot_func(data_list[[nm]], nm, "point", xlim, ylim))
  names(pplots1) <- names(data_list)
  
  # grid plots
  bins = 35
  bin_width <- c(diff(xlim) / bins, diff(ylim) / bins)
  bin_data <- ggplot(df_all, aes(x, y)) + stat_bin2d(binwidth = bin_width)
  binned_df <- ggplot_build(bin_data)$data[[1]]
  max_count <- max(binned_df$count, na.rm = TRUE)
  
  grids1 <- lapply(names(data_list), function(nm) plot_func(data_list[[nm]], nm, "grid", xlim, ylim, bin = bin_width, fill_limits = c(0,max_count*(1/3))))
  names(grids1) <- names(data_list)
  
  # heatmaps
  hplots1 <- lapply(names(data_list), function(nm) plot_func(data_list[[nm]], nm, "heatmap", xlim, ylim))
  names(hplots1) <- names(data_list)
  
  return(list(point = pplots1,
              grid = grids1,
              heatmap = hplots1))
}


##### datasets
set.seed(123)
normal_data = spData::boston.c[,c("LON","LAT")]; names(normal_data) <- c("x","y")
clustered_data = spData::nydata[,c("X","Y")]; names(clustered_data) <- c("x","y")
random_data = data.frame(x = runif(500), y = runif(500))
smallclusters_data = data.frame(x = c(sapply(runif(100), function(x) rnorm(5,x,0.01))) + rnorm(100,0.5,0.01), 
                                y = c(sapply(runif(100), function(x) rnorm(5,x,0.01))) + rnorm(100,0.5,0.01))
consistent_data = data.frame(x = rep(seq(0,1,0.1), 11) + rnorm(121,0,0.01), y = sort(rep(seq(0,1,0.1),11)) + rnorm(121,0,0.01))


##### now to make the maps apply function
normal_maps = p.g.h.maps(normal_data)
clustered_maps = p.g.h.maps(clustered_data)
random_maps = p.g.h.maps(random_data)
smallclusters_maps = p.g.h.maps(smallclusters_data)
consistent_maps = p.g.h.maps(consistent_data)



cgg_layer = layer_data(consistent_maps$grid$grid)

map_raster = raster(nrow = 35, ncol = 35); extent(map_raster) = unlist(consistent_maps$grid$grid$coordinates$limits)
values(map_raster) = rep(0,35*35)
map_raster[cgg_layer$y,cgg_layer$x]= cgg_layer$value

plot(map_raster)
plot(cgg_layer$y,cgg_layer$x, add = TRUE)

map_raster[1,1] = 1

cell_coord = xyFromCell(map_raster, 1:ncell(map_raster))
cell_coord %in% data.frame("x" = cgg_layer$x,"y" = cgg_layer$y)

plot(cell_coord);points(data.frame("x" = (cgg_layer$x),"y" = cgg_layer$y), col ="red", pch = 20)


rass = (sdc_raster(consistent_data, rep(1,nrow(consistent_data)),r= 1/35))

################
library(raster)

# Initialize empty raster with desired extent and resolution
r <- raster(ncol = 30, nrow = 30);extent(r) = unlist(consistent_maps$grid$grid$coordinates$limits)

# Count points per cell
cells <- cellFromXY(r, consistent_data)
cell_counts <- table(cells)

# Fill raster with counts
r[] <- 0
r[as.numeric(names(cell_counts))] <- as.vector(cell_counts)

# Inspect raster values and coordinates
print(r)
raster::xyFromCell(r, 1:ncell(r))  # coordinates of each cell center
getValues(r)                       # values (counts)

# Optional: Plot
plot(r, main = "Raster Heatmap")
points(cgg_layer$xmin, cgg_layer$ymin, add = TRUE, pch = 20, col = "red")
points(cgg_layer$xmax, cgg_layer$ymax, add = TRUE, pch = 20, col = "red")
points(cgg_layer$x, cgg_layer$y, add = TRUE, pch = 20)


ggplot(consistent_data) +
  geom_tile(aes(x,y, fill = density))



library(raster)
library(ggplot2)

# Extract cell centers and counts from raster
r_df <- data.frame(
  xy = raster::xyFromCell(r, 1:ncell(r)),
  count = raster::getValues(r)
)

# Rename coordinate columns
names(r_df)[1:2] <- c("x", "y")

# Plot heatmap with geom_tile
ggplot(r_df, aes(x = x, y = y, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Raster Heatmap with geom_tile", fill = "Count") +
  theme_minimal()



build = ggplot_build(consistent_maps$grid$grid)$data[[1]]



library(ggplot2)

# Generate sample data
set.seed(123)
n_points <- 100
df <- data.frame(
  x = runif(n_points, min = 0, max = 10),
  y = runif(n_points, min = 0, max = 10)
)

# Plot using stat_bin2d with custom bin counts and limits
p <- ggplot(df, aes(x = x, y = y)) +
  stat_bin2d(
    bins = c(20, 20),                    # number of bins in x and y
    drop = FALSE
  ) +
  coord_cartesian(xlim = c(0, 11), ylim = c(0, 11)) +
  scale_fill_viridis_c() +
  labs(title = "2D Heatmap", x = "X", y = "Y")

# Show plot
print(p)

library(ggplot2)

# Generate sample data
set.seed(123)
n_points <- 50
df <- data.frame(
  x = runif(n_points, min = 0, max = 10),
  y = runif(n_points, min = 0, max = 10)
)

# Plot with fixed axis limits and 20x20 bins
p <- ggplot(df, aes(x = x, y = y)) +
  stat_bin2d(
    bins = c(20, 20),         # number of bins
    drop = FALSE              # include empty bins
  ) +
  scale_x_continuous(limits = c(0, 11), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 11), expand = c(0, 0)) +
  scale_fill_viridis_c() +
  labs(title = "2D Heatmap with Fixed Axis Limits", x = "X", y = "Y")

# Display plot
print(p)

library(dplyr)
library(tidyr)

# Sample data
set.seed(123)
n_points <- 50
df <- data.frame(
  x = runif(n_points, min = 0, max = 10),
  y = runif(n_points, min = 0, max = 10)
)

df = consistent_data

# Parameters
xlim <- consistent_maps$grid$grid$coordinates$limits$x
ylim <- consistent_maps$grid$grid$coordinates$limits$y
x_bins <- 35
y_bins <- 35

# Breaks and labels
x_breaks <- seq(xlim[1], xlim[2], length.out = x_bins + 1)
y_breaks <- seq(ylim[1], ylim[2], length.out = y_bins + 1)

df <- df %>%
  mutate(
    x_bin = cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE),
    y_bin = cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
  )

# Count actual points per bin
bin_counts <- df %>%
  count(x_bin, y_bin, name = "count")

# Generate full grid of bin indices
full_grid <- expand.grid(
  x_bin = 1:x_bins,
  y_bin = 1:y_bins
)

# Merge actual counts with full grid (fill missing with 0)
full_heatmap <- full_grid %>%
  left_join(bin_counts, by = c("x_bin", "y_bin")) %>%
  mutate(count = replace_na(count, 0)) %>%
  mutate(
    x_center = (x_breaks[x_bin] + x_breaks[x_bin + 1]) / 2,
    y_center = (y_breaks[y_bin] + y_breaks[y_bin + 1]) / 2,
    bin_width = diff(x_breaks)[1],
    bin_height = diff(y_breaks)[1]
  )

# ✅ Now full_heatmap contains every bin, even those with zero count
print(head(full_heatmap))
library(ggplot2)

ggplot(full_heatmap, aes(x = x_center, y = y_center, fill = count)) +
  geom_tile(width = full_heatmap$bin_width[1], height = full_heatmap$bin_height[1]) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Full Grid 2D Histogram", x = "X", y = "Y")


library("gplots")
hist2d(df$x, df$y, nbins = c(20, 20), show = FALSE)


ggplot(normal_data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )
