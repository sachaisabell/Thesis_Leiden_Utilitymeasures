
##################
###### GLOBAL measures
##################

### VMR (variance mean ratio)
VMR_function = function(original, safe){
  VMR.og = var(c(original))/mean(c(original))
  VMR.safe = var(c(safe))/mean(c(safe))
  
  abs(VMR.og - VMR.safe)
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
moransI_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"), style = "W") {
  input_rast = raster(ncol = 35, nrow = 35)
  values(input_rast) = input
  
  neighs = spdep::cell2nb(nrow(input_rast), ncol(input_rast), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix
  
  if (version == "global") {
    return(spdep::moran.test(values(input_rast), weight.matrix)$estimate)
  } else {
    return(spdep::localmoran(values(input_rast), weight.matrix)[,1])
  }
}


moransI_area_func = function(input, version = c("local", "global"), style = "W") {
  
  neighs = spdep::poly2nb(input$geometry)   # Neighbors list
  weight.matrix = spdep:: nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix
  
  if (version == "global") {
    return(spdep::moran.test(input$count, weight.matrix)$estimate) # count is not inherent, name yourself if necessary
  } else {
    return(spdep::localmoran(input$count, weight.matrix)[,1])
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

Gi_grid_func = function(input_vals, version = c("local", "global"), type = "queen",  style = "W", star = FALSE) {
  input = raster(ncol = 35, nrow = 35)
  values(input) = input_vals
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  
  if(version == "local" & star == TRUE){
    diag(weight.matrix) = 1 # function automatically sets to 0 which is not good
    
    if(style == "W"){
      neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
      weight.matrix = spdep::nb2mat(neighs, style = "B", zero.policy = TRUE) 
      diag(weight.matrix) = 1
      
      weight.matrix = t(rbind(apply(weight.matrix, 1, function(x) x/sum(x))))
    }
  }
  
  
  if (version == "global") {
    return(spdep::globalG.test(values(input), spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(Gi_function(values(input), weight.matrix, star = star))
  }
  
  
}

Gi_area_func = function(input, version = c("local", "global"),  style = "W", star = FALSE) {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  
  if(version == "local" & star == TRUE){
    diag(weight.matrix) = 1 # function automatically sets to 0 which is not good
    
    if(style == "W"){
      neighs = spdep::poly2nb(input$geometry) # Neighbors list
      weight.matrix = spdep::nb2mat(neighs, style = "B", zero.policy = TRUE) 
      diag(weight.matrix) = 1
      
      weight.matrix = t(rbind(apply(weight.matrix, 1, function(x) x/sum(x))))
    }
  }
  
  if (version == "global") {
    return(spdep::globalG.test(input$count, spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # count is not inherent, name yourself if necessary
  } else {
    return(Gi_function(input$count, weight.matrix, star = star))
  }
  
}

###### geary index
Geary_grid_func = function(input_vals, version = c("local", "global"), type = "queen",  style = "W") {
  input = raster(ncol = 35, nrow = 35)
  values(input) = input_vals
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  if (version == "global") {
    return(spdep::geary.test(values(input), weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(values(input), weight.matrix, star = star))
  }
  
  
}

Geary_area_func = function(input, version = c("local", "global"),  style = "W") {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 
  
  if (version == "global") {
    return(spdep::geary.test(input$count, weight.matrix)$estimate) # count is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(input$count, weight.matrix, star = star))
  }
  
}


##### for making local maps
local_grid_func = function(vals, all, title){
  data = cbind(x = rep(1:35, 35),
               y = rev(c(sapply(1:35, function(x) rep(x, 35)))),
               count = vals)
  
  p = ggplot() + geom_tile(data = data, aes(x = x, y = y, fill = count)) + 
    scale_fill_gradient(name = "Value", low = "white", high = "black", limits = c(min(all), max(all)))+ theme_classic()+
    theme(aspect.ratio = 1, legend.position = "right",legend.text =  element_text(size = 20), legend.title = element_text(size = 20), legend.key.size = unit(1, "cm"),
          axis.line.y.right = element_line(), axis.line.x.top = element_line(), plot.title = element_text(size = 30), panel.border = element_rect(colour = "black", fill = NA),panel.background = element_rect(fill = "white", color = NA)) + 
    ggtitle(title)
  
  return(p)
}

local_area_func = function(dataset, all, vals, title){
  ggplot() +
    geom_sf(data = dataset, aes(fill = vals)) +
    scale_fill_gradient(name = "Value", low = "white", high = "black", limits = c(min(all), max(all))) +
    theme_minimal() +
    theme(legend.position = "right", axis.line.y.right = element_line(), axis.line.x.top = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white", color = NA), plot.title = element_text(size = 30))+
    labs(fill = "Value") + 
    ggtitle(title)
  
}