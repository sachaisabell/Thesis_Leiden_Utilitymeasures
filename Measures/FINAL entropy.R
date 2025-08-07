
get_probs = function(values){
  dens = density(values, n = 1000, from = 0, to = max(values))
  dx = diff(dens$x)[1]
  f_x = dens$y
  p_i = f_x * dx
  p_i <- p_i / sum(p_i) 
  
  return(p_i)
}


LDDP_func = function(values, n = 1000){
  dens = density(values, n = 1000, from = 0, to = max(values))
  dx = diff(dens$x)[1]
  f_x = dens$y
  p_i = f_x * dx
  p_i <- p_i / sum(p_i) 
  
  # Uniform density over the same range, same length
  r = max(values) - min(values)
  g_x = rep(1 / r, length(p_i))
  q_i = g_x * dx # matches the same format as p_i
  q_i <- q_i / sum(q_i) 
  
  #philentropy::KL(rbind(p_i, q_i), unit = "log")
  -sum(p_i * log((p_i + 1e-100) / (q_i + 1e-100)))  # D_KL(p || q)
}


shannon_cont_entropy_func = function(values, n = 10000){ # also called differential entropy
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  p_i <- p_i / sum(p_i)
  
  -sum(p_i * log(p_i + 10^-100)) + log(step_size)
}



shannon_entropy_func = function(values){
  probs = c(table(values))/length(values)
  
  -sum(probs * log(probs))
}


KL_func = function(original, safe){
  og.probs = get_probs(original)
  sf.probs = get_probs(safe)
  
  philentropy::KL(rbind(og.probs, sf.probs), unit = "log")
}


entropy_func = function(original, safe, method = c("KL","LDDP","Shannon","Diff")){ # input is just values
  
  if(method == "KL") return(KL_func(original, safe))
  if(method == "LDDP"){
    og.LDDP = LDDP_func(original)
    safe.LDDP = LDDP_func(safe)
    return(abs(og.LDDP - safe.LDDP) / (og.LDDP + safe.LDDP))
  } 
  if(method == "Shannon"){
    og.shan = shannon_entropy_func(original)
    safe.shan = shannon_entropy_func(safe)
    return(abs(og.shan - safe.shan) / (abs(og.shan) + abs(safe.shan)))
  }
  if(method == "Diff"){
    og.diff = shannon_cont_entropy_func(original);print(og.diff)
    safe.diff = shannon_cont_entropy_func(safe);print(safe.diff)
    return(abs(og.diff - safe.diff) / (2*max(abs(og.diff), abs(safe.diff))))
  }
  
}

shannon_entropy_func(nvdata)
























