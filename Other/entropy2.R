





LDDP_func = function(values, n = 1000){
  dens = density(values, n = n, from = min(values), to = max(values))
  dx = diff(dens$x)[1]
  f_x = dens$y
  p_i = f_x * dx
  p_i <- p_i / sum(p_i) 
  
  # Uniform density over the same range, same length
  r = max(values) - min(values)
  g_x = rep(1 / r, length(f_x))
  q_i = g_x * dx  # matches the same format as p_i
  q_i <- q_i / sum(q_i) 
  
  -sum(p_i * log((p_i + 1e-100) / (q_i + 1e-100)))  # D_KL(p || q)
}

shannon_cont_entropy_func = function(values, n = 10000){
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  p_i <- p_i / sum(p_i)
  
  -sum(p_i * log(density_est$y + 10^-100)) - log(step_size)
}


nvdata = random_maps$raster_data$random$count

LDDP_func(nvdata)

set.seed(1)
dens = density(nvdata, n = 1000, from = min(nvdata), to = max(nvdata))
dx = diff(dens$x)[1]
f_x = dens$y
p_i = f_x * dx
p_i <- p_i / sum(p_i) 
plot(dens)

# Uniform density over the same range, same length
r = max(nvdata) - min(nvdata)
g_x = rep(1 / r, length(f_x))
q_i = g_x * dx  # matches the same format as p_i
q_i <- q_i / sum(q_i) 

KL(rbind(p_i,q_i), unit = "log")


KL(rbind(nvdata,g_x), unit = "log", est.prob = TRUE)


sum(p_i * log((p_i  + 1e-100) / (q_i + 1e-100)))  # D_KL(p || q)

LDDP_func()

shannon_cont_entropy_func(nvdata) - log(max(nvdata) - min(nvdata))


####
get_probs = function(values, maxdata){
  dens = density(values, n = 1000, from = 0, to = maxdata)
  dx = diff(dens$x)[1]
  f_x = dens$y
  p_i = f_x * dx
  p_i <- p_i / sum(p_i) 
}
























