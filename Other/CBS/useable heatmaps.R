



plot_func_heatog = function(data, title, type, xlimits, ylimits, bin = NULL, fill_limits = NULL){
  
  p = ggplot()
  
  if(type == "point") {
    p = p + geom_point(data = data, aes(x, y))
  }
  
  if(type == "heatmap") {
    plot1 = ggplot() +
      ggdensity::geom_hdr(data = data, aes(x, y), probs = seq(0.00001,0.9999999,0.01)) + 
      theme_classic() +
      theme(aspect.ratio = 1, legend.position = "none")
    
    
    
    return(plot1)
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



p.g.h.maps2 = function(data1){
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
  pplots1 <- lapply(names(data_list), function(nm) plot_func_heatog(data_list[[nm]], nm, "point", xlim, ylim))
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
  hplots1 <- lapply(names(data_list), function(nm) plot_func_heatog(data_list[[nm]], nm, "heatmap", xlim, ylim))
  names(hplots1) <- names(data_list)
  
  return(list(point = pplots1,
              grid = grids1,
              heatmap = hplots1,
              raster_data = raster_data_list))
}

set.seed(123)
normal_data2 = spData::boston.c[,c("LON","LAT")]; names(normal_data2) <- c("x","y")
clustered_data2 = spData::nydata[,c("X","Y")]; names(clustered_data2) <- c("x","y")
random_data2 = data.frame(x = runif(500), y = runif(500))
smallclusters_data2 = data.frame(x = c(sapply(runif(100), function(x) rnorm(5,x,0.01))) + rnorm(100,0.5,0.01), 
                                y = c(sapply(runif(100), function(x) rnorm(5,x,0.01))) + rnorm(100,0.5,0.01))
consistent_data2 = data.frame(x = rep(seq(0,1,0.1), 11) + rnorm(121,0,0.01), y = sort(rep(seq(0,1,0.1),11)) + rnorm(121,0,0.01))

##### now to make the maps apply function
normal_maps2 = p.g.h.maps2(normal_data2)
clustered_maps2 = p.g.h.maps2(clustered_data)
random_maps2 = p.g.h.maps2(random_data)
smallclusters_maps2 = p.g.h.maps2(smallclusters_data)
consistent_maps2 = p.g.h.maps2(consistent_data)




















