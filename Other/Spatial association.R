##################
###### GLOBAL measures
##################

### VMR (variance mean ratio)
VMR_function = function(original, safe){
  VMR.og = var(c(original))/mean(c(original))
  VMR.safe = var(c(safe))/mean(c(safe))
  
  VMR.og - VMR.safe
}


### MMR varieties
MMR_function = function(original, safe, option = "MMR", pure = TRUE){
  # calculate median beforehand
  Med = c(median(c(original)),median(c(safe)))
  
  # use corrected median if asked for, or if either median is 0
  if (pure == FALSE) Med = Med + c(1/length(c(original)), 1/length(safe)) * c(min(original[original != 0]), min(safe[safe != 0]))
  
  if(0 %in% Med == TRUE){
    warning("one or both medians are 0, corrected medians are used (pure = FALSE)")
    return(MMR_function(original, safe, option, pure = FALSE))
  }
  
  ## MMR
  if (option == "MMR"){
    MedAD = c(c(median(abs(c(original) - Med[1])), 
                median(abs(c(safe) - Med[2]))))
    
    return(diff(MedAD / Med))
  }
  
  ## MMR*
  if (option == "MMR*"){
    MedAD = c(c(median(abs(c(original) - mean(original))), 
                median(abs(c(safe) - mean(safe)))))
    
    return(diff(MedAD / Med))
  }
  
  ## MMR**
  if (option == "MMR**"){
    MedAD = c(c(mean(abs(c(original) - Med[1])), 
                mean(abs(c(safe) - Med[2]))))
    
    return(diff(MedAD / Med))
  }
}



##### Moran's I: global/local and for grid/area
moransI_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"), style = "B") {
  version <- match.arg(version)
  type <- match.arg(type)
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix
  
  if (version == "global") {
    return(spdep::moran.test(values(input), weight.matrix)$estimate)
  } else {
    return(spdep::localmoran(values(input), weight.matrix)[,1])
  }
}


moransI_area_func = function(input, version = c("local", "global"), style = "B") {
  
  neighs = spdep::poly2nb(input$geometry)   # Neighbors list
  weight.matrix = spdep:: nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix
  
  if (version == "global") {
    return(spdep::moran.test(input$values, weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localmoran(input$values, weight.matrix)[,1])
  }
}

################# Gi and Gi* (local)
Gi_function = function(data, d, star = FALSE) {
  n = length(data)
  GIs = rep(NA,n)
  
  for (i in 1:n) {
    num = den = 0
    
    for (j in 1:n) {
      if (star == FALSE & i == j) next
      
      else {
        num = num + d[i,j] * data[j]
        den = den + data[j]
      }
      
    }
    
    GIs[i] = num / den
  }
  
  return(GIs)
}

Gi_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"),  style = "B", star = FALSE) {
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  diag(weight.matrix) = 1 # function automatically sets to 0 which is not good
  
  if (version == "global") {
    return(spdep::globalG.test(values(input), spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(Gi_function(values(input), weight.matrix, star = star))
  }
  
  
}

Gi_area_func = function(input, version = c("local", "global"),  style = "B", star = FALSE) {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  diag(weight.matrix) = 1
  
  if (version == "global") {
    return(spdep::globalG.test(input$values, spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(Gi_function(input$values, weight.matrix, star = star))
  }
  
}

###### geary index
Geary_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"),  style = "B") {
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  if (version == "global") {
    return(spdep::geary.test(values(input), weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(values(input), weight.matrix, star = star))
  }
  
  
}

Geary_area_func = function(input, version = c("local", "global"),  style = "B") {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  if (version == "global") {
    return(spdep::geary.test(input$values, weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(input$values, weight.matrix, star = star))
  }
  
}


##### for making local maps
local_grid_map = function(output, original){
  
  empty.raster= raster(ncol = ncol(original), nrow = nrow(original))
  extent(empty.raster) = (extent(original)[1:4])
  values(empty.raster) = output
  
  return(empty.raster)
}

local_area_map = function(output, original){
  
  output.map = original
  output.map$values = output
  
  return(output.map)
  
}


###### spatial lag plot
grid_lag_plot_func = function(input, type = "rook", style = "B"){
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


###### examples
ngo_rast = raster(ncol = 35, nrow = 35); values(ngo_rast) = normal_maps$raster_data$original$count
ngo_geary = Geary_grid_func(ngo_rast, version = "local", type = "rook")
plot(local_grid_map(ngo_geary, ngo_rast))

UT_count$values = UT_count$count
area_lag_plot_func(UT_count)

loc = moransI_grid_func(ngo_rast, version = "local", type = "rook", style = "B")
glob = moransI_grid_func(ngo_rast, version = "global", type = "rook", style = "B")


neighs = spdep::cell2nb(35, 35, type = "rook")  # Neighbors list
weight.matrix = spdep::nb2mat(neighs, style = "B", zero.policy = TRUE)  # Weight matrix
scal = 1/sum(weight.matrix)

scal*sum(loc)



