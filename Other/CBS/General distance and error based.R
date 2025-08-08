


###################### 
########### GLOBAL measures
###################### 

### MSE (mean squared error)
MSE_func = function(original, safe){
  mean((original - safe)^2)
}


### MAE (mean absolute error)
MAE_func = function(original, safe){
  mean(abs(original - safe))
}


### SME (symmetric mean error)
SME_func = function(original, safe){
  delta = abs(original - safe)/(original + safe)
  delta[is.nan(delta)] = 0
  
  1/(length(original)*2) * sum(delta)
}


### Hellinger distance
HD_func = function(original, safe){
  in.sqrt = sum((sqrt(safe) - sqrt(original))^2)
  1/sqrt(2) * sqrt(in.sqrt)
}


### MRAD (Mean relative absolute difference)
MRAD_func = function(original, safe){
  RAD.int = abs(original - safe)/original
  RAD.int[is.nan(RAD.int)] = 0
  mean(RAD.int)
}



###################### 
########### LOCAL measures
###################### 


### SE (squared error)
SE_func = function(original, safe){
  ((original - safe)^2)
}


### AE (absolute error)
AE_func = function(original, safe){
  (abs(original - safe))
}


### Symmetric error
SymmE_func = function(original, safe){
  delta = abs(original - safe)/(original + safe)
  delta[is.nan(delta)] = 0
  
  delta
}


### SDSR (local Hellinger distance)
SDSR_func = function(original, safe){
  ((sqrt(safe) - sqrt(original))^2)
}


### RAD (relative absolute difference)
RAD_func = function(original, safe){
  RAD.int = abs(original - safe)/original
  RAD.int[is.nan(RAD.int)] = 0
  (RAD.int)
}


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











































