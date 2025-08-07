
library(raster)
library(sdcSpatial)
library(ggplot2)
library(cowplot)

plot_func = function(data, title, type, xlimits, ylimits, bin = NULL, fill_limits = NULL){
  
  p = ggplot()
  
  if(type == "point") {
    p = p + geom_point(data = data, aes(x, y))
  }
  
  if(type == "heatmap") {
    plot1 = ggplot() +
      ggdensity::geom_hdr(data = data, aes(x, y), probs = seq(0.00001,0.99,0.1)) + 
      theme_classic() +
      theme(aspect.ratio = 1, legend.position = "right", legend.text =  element_text(size = 20), legend.title = element_text(size = 20))
    
    legend = get_legend(plot1)
    
    plot2 = ggplot() +
      ggdensity::geom_hdr(data = data, aes(x, y), probs = seq(0.00001,0.9999999,0.01)) + 
      theme_classic() +
      theme(aspect.ratio = 1, legend.position = "none", axis.line.y.right = element_line(), axis.line.x.top = element_line(), 
            panel.border = element_rect(colour = "black", fill = NA),
            panel.background = element_rect(fill = "white", color = NA))
    
    final_plot <- plot_grid(
      plot2, 
      legend, 
      ncol = 2, 
      align = "v", 
      rel_widths = c(1, 0.3)  # Make legend column narrower
    )
    
    return(final_plot)
  }
  
  if(type == "grid") {
    
    if(fill_limits[2] < 5) {
      fake_data <- data.frame(
        x = rep(xlimits[1] * 100, fill_limits[2] + 1),
        y = rep(ylimits[1] * 100, fill_limits[2] + 1),
        count = 0:fill_limits[2]
      )
      data = rbind(data, fake_data)
      
      p = p + geom_tile(data = data, aes(x = x, y = y, fill = factor(count)), width = bin[1], height = bin[2]) + 
        scale_fill_manual(
        values = colorRampPalette(c("white", "black"))(fill_limits[2] + 1),
        drop = FALSE,
        name = "Value"
      )
      
        
    } else {
      p = p + geom_tile(data = data, aes(x = x, y = y, fill = count), width = bin[1], height = bin[2]) + 
        scale_fill_gradient(name = "Value", low = "white", high = "black", limits = fill_limits)
    }
    


  }
  
  plot = p + 
    theme_classic()+
    theme(aspect.ratio = 1, legend.position = "right",legend.text =  element_text(size = 20), legend.title = element_text(size = 20), legend.key.size = unit(1, "cm"),
          axis.line.y.right = element_line(), axis.line.x.top = element_line(), panel.border = element_rect(colour = "black", fill = NA),panel.background = element_rect(fill = "white", color = NA)) +
    coord_cartesian(xlim = xlimits, ylim = ylimits)
  
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
  
  # grid plots with raster counts
  bins = 35
  bin_width <- c(diff(xlim) / bins, diff(ylim) / bins)
  
  # Create raster template
  r <- raster(ncol = bins, nrow = bins)
  extent(r) <- c(xlim[1], xlim[2], ylim[1], ylim[2])
  
  # Function to rasterize/count points per cell and prepare dataframe for plotting
  rasterize_data <- function(df){
    cells <- cellFromXY(r, df[, c("x","y")])
    cell_counts <- table(cells)
    
    # Fill raster with counts
    r_vals <- rep(0, ncell(r))
    r_vals[as.numeric(names(cell_counts))] <- as.vector(cell_counts)
    
    r[] <- r_vals
    
    # Convert raster cells to dataframe for geom_tile
    df_r <- data.frame(xyFromCell(r, 1:ncell(r)), count = getValues(r))
    colnames(df_r) <- c("x", "y", "count")
    
    return(df_r)
  }
  
  # Rasterize all datasets
  raster_data_list <- lapply(data_list, rasterize_data)
  
  # Find max count across all datasets for consistent fill scale
  max_count <- max(sapply(raster_data_list, function(d) max(d$count, na.rm = TRUE)))
  
  # Generate grid plots
  grids1 <- mapply(function(df_r, nm){
    plot_func(df_r, nm, "grid", xlim, ylim, bin = bin_width, fill_limits = c(0, max_count))
  }, raster_data_list, names(raster_data_list), SIMPLIFY = FALSE)
  names(grids1) <- names(data_list)
  
  # heatmaps
  hplots1 <- lapply(names(data_list), function(nm) plot_func(data_list[[nm]], nm, "heatmap", xlim, ylim))
  names(hplots1) <- names(data_list)
  
  return(list(point = pplots1,
              grid = grids1,
              heatmap = hplots1,
              raster_data = raster_data_list,
              point_data = data_list))
}

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


### number of counts per map (used for the Hellinger distance)
sum(normal_maps$raster_data$original$count)
sum(clustered_maps$raster_data$original$count)
sum(random_maps$raster_data$original$count)
sum(smallclusters_maps$raster_data$original$count)
sum(consistent_maps$raster_data$original$count)

length(c(rep(506,4), rep(281,4), rep(500,4), rep(500,4), rep(121,4)))

##### uncomment to save the maps as images
# sdc = c("original","random","grid","vonoroi","weighted")
# data_types = c("normal", "clustered","random","smallclusters","consistent")
# map_types = c("point","grid","heat")
# 
# for (i in 1:length(data_types)) {
#   for (j in 1:length(sdc)) {
#     current = get(paste0(data_types[i], "_maps"))
#     
#     ggsave(paste0(data_types[i],"_", sdc[j],"_", map_types[1],".png"), current[[1]][[j]]) # 1 is point
#     ggsave(paste0(data_types[i],"_", sdc[j],"_", map_types[2],".png"), current[[2]][[j]]) # 2 is grid
#     ggsave(paste0(data_types[i],"_", sdc[j],"_", map_types[3],".png"), current[[3]][[j]]) # 3 is heatmap
#     
#   }
# }
# 
# 
# 

