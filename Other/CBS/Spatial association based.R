



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


### Moran's I for GRIDS
# so input here is e.g. map_safe$value$mean (with the nan = 0)
MI_func = function(original, safe, style = c("W","C","B")){
  
  original = original$value$mean
  safe = safe$value$mean
  
  original = raster::as.matrix(original); original[is.na(original)] = 0
  safe = raster::as.matrix(safe); safe[is.na(safe)] = 0
  
  original.weights = spdep::cell2nb(nrow(original),ncol(original)) %>% nb2listw(style = style, zero.policy = TRUE)
  safe.weights = spdep::cell2nb(nrow(safe),ncol(safe)) %>% nb2listw(style = style, zero.policy = TRUE)
  
  MI.original = spdep::moran.test(c(original), original.weights)
  MI.safe = spdep::moran.test(c(safe), safe.weights)
  
  return(c(MI.original, MI.safe))  
}




##################
###### LISA measures
##################

weight.matrix = function(input, style = c("B", "C", "W"), output = c("matrix","list"), map = c("grid", "area")){
  
  # first get the neighbors
  if(map == "grid"){
    input = input$value$mean
    input = raster::as.matrix(input); input[is.na(input)] = 0
    input.weights = spdep::cell2nb(nrow(input),ncol(input)) 
    
  } 
  else if(map == "area"){
    input = input$geometry
    input.weights = spdep::poly2nb(input) 
  } 
  
  # then actually calculate the correct weights matrix
  if(output == "matrix"){
    weight.matrix = nb2mat(input.weights, style = style, zero.policy = TRUE)
  }
  else if(output == "list"){
    weight.matrix = nb2listw(input.weights, style = style, zero.policy = TRUE)
  }
  
  
  return(weight.matrix)
}


### local Moran's I 
local_MI_func = function(original, safe, style = c("W","C","B"), type = c("grid", "area")){
  
  if(type == "grid"){
    og = original$value$mean %>% raster::as.matrix(); og[is.na(og)] = 0
    sf = safe$value$mean %>% raster::as.matrix(); sf[is.na(sf)] = 0
    
    original.weights = weight.matrix(original, style = style, map = type, output = "list")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "list")
    
    MI.original = spdep::localmoran(c(og), original.weights)
    MI.safe = spdep::localmoran(c(sf), safe.weights)
  }
  
  if(type == "area"){
    
    original.weights = weight.matrix(original, style = style, map = type, output = "list")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "list")
    
    MI.original = spdep::localmoran(original$values, original.weights)
    MI.safe = spdep::localmoran(safe$values, safe.weights)
  }
  
  
  return(c(MI.original, MI.safe))  
}




#### Gi and Gi*
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


Gi_func = function(original, safe, style = c("W","C","B"), type = c("grid", "area"), star = FALSE){
  if(type == "grid"){
    og = original$value$mean %>% raster::as.matrix(); og[is.na(og)] = 0
    sf = safe$value$mean %>% raster::as.matrix(); sf[is.na(sf)] = 0
    
    original.weights = weight.matrix(original, style = style, map = type, output = "matrix")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "matrix")
    
    print(c(original$value$mean))
    print(original.weights)
    
    
    Gi.original = Gi_function(c(og), original.weights, star = star)
    Gi.safe = Gi_function(c(sf), safe.weights, star = star)
  }
  
  if(type == "area"){
    
    original.weights = weight.matrix(original, style = style, map = type, output = "matrix")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "matrix")
    
    Gi.original = Gi_function(original$values, original.weights, star = star)
    Gi.safe = Gi_function(safe$values, safe.weights, star = star)
  }
  
  
  return(list("original" = Gi.original, 
              "safe" = Gi.safe))  
}



#### local Geary index















