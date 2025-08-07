

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


### RAD (Mean relative absolute difference)
RAD_func = function(original, safe){
  RAD.int = abs(original - safe)/original
  RAD.int[is.nan(RAD.int)] = 0
  mean(RAD.int)
}

