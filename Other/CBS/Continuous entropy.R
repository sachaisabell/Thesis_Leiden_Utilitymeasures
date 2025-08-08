







### LDDP (related to kullbeck leibner)
LDDP_func = function(values, n = 10000){
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  
  - sum(p_i * log((p_i * diff(range(values)))/ length(values) ))
  
  
}


### Shannon's continuos entropy
shannon_cont_entropy_func = function(values, n = 10000){
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  
  -sum(p_i * log(p_i / step_size))
}



### general function to run
# note that safe and original are just vectors of values here
continuous_entropy_func = function(safe, original, type = c("LDDP", "shannon"), n = 10000){
  
  if(type == "LDDP"){
    safe.entropy = LDDP_func(safe, n)
    original.entropy = LDDP_func(original, n)
  }
  
  if(type == "shannon"){
    safe.entropy = shannon_cont_entropy_func(safe, n)
    original.entropy = shannon_cont_entropy_func(original, n)
  }
  
  return(abs(safe.entropy - original.entropy) / (safe.entropy + original.entropy))
  
}





















