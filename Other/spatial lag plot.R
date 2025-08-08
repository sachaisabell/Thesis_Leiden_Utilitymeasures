



ngo_rast = raster(ncol = 35, nrow = 35); values(ngo_rast) = normal_maps$raster_data$original$count
ngo_geary = Geary_grid_func(ngo_rast, version = "local", type = "rook")
plot(local_grid_map(ngo_geary, ngo_rast))

neighs = spdep::cell2nb(nrow(ngo_rast), ncol(ngo_rast), type = "rook")  # Neighbors list
weight.matrix = spdep::nb2listw(neighs, style = "B", zero.policy = TRUE) 

lag_estimate <- spdep::lag.listw(weight.matrix, values(ngo_rast))

datadf = cbind(x = values(ngo_rast),
               y = lag_estimate)

ggplot(datadf,aes(x, y )) + 
  geom_point(size = 3) + 
  geom_abline(color = "red") + 
  theme_minimal()




spatial_lag_plot_func = function(input, type = "rook", style = "B"){
  vals = values(input)
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  lag_estimate <- spdep::lag.listw(weight.matrix, vals)
  
  datadf = cbind(x = vals,
                 y = lag_estimate)
  
  ggplot(datadf,aes(x, y )) + 
    geom_point(size = 3) + 
    geom_abline(color = "red") + 
    theme_minimal()
}

spatial_lag_plot_func(ngr_rast)


ngr_rast = raster(ncol = 35, nrow = 35); values(ngr_rast) = normal_maps$raster_data$random$count
######################
### on areas
area_lag_plot_func = function(input, type = "rook", style = "B"){
  vals = input$values
  
  neighs = spdep::poly2nb(input$geometry) 
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  lag_estimate <- spdep::lag.listw(weight.matrix, vals)
  
  datadf = cbind(x = vals,
                 y = lag_estimate)
  
  ggplot(datadf,aes(x, y )) + 
    geom_point(size = 3) + 
    geom_abline(color = "red") + 
    theme_minimal()
}

UT_count$values = UT_count$count
area_lag_plot_func(UT_count)





