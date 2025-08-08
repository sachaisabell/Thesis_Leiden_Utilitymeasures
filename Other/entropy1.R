
# fomr the example
nn <- 100
p <- seq(0, 1, len = 512)
p_mid <- (p[-1] + p[-nn])/2
delta2 <- diff(p)[1]
f_mid2 <- dbeta(p_mid, shape1=a, shape2=b)
p_i2 <- f_mid2 * delta2

plot(p_mid, p_i2, type = "l", col = "blue")
lines(dens.vals$x, p_i)

LDDP_func(f_mid2)
LDDP_func(f_mid)


### our own version
set.seed(1)
vals = rbeta(1000000, 40, 50)
plot(density(vals))

dens.vals = density(vals) 
delta = diff(dens.vals$x)[1] # step size
f_mid = dens.vals$y #; f_mid = f_mid[seq(1,500,5)]
p_i = delta * (f_mid) # probabilities

sum(p_i)
-sum(p_i * log(f_mid)) #+ log(delta)

-sum(p_i * log(p_i))  + log(delta)


###




########
LDDP_func = function(values, n = 10000){
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  
  #- sum(p_i * log((p_i * diff(range(values)))/ length(values) ))
  #- sum(p_i * log((p_i *  length(values))/ diff(range(values)) ))
  #sum(p_i * log((p_i + 10^-100) )) - log(step_size) + log(max(values) - min(values)) #pretty close
  #sum(p_i * log(p_i / step_size *)) 
  #log(length(p_i)) - sum(p_i * (log(p_i + 10^-100) - log(1/length(p_i))))
  # log(max(values) - min(values)) - sum(p_i * log(p_i * (1 / step_size) + 1e-100)) should be right?
  
  ### should be the same
  #log(n) - sum(p_i  * (log(p_i*(max(values) - min(values)) + 10^-100)))
  #log(n) - sum(p_i  * (log(p_i + 10^-100) + log(max(values) - min(values)) ))
  
  # now for the KL
  log(n) - sum(p_i  * (log(p_i + 10^-100) + log(max(values) - min(values)) ))
}


### Shannon's continuos entropy
shannon_cont_entropy_func = function(values, n = 10000){
  
  density_est = density(values, n = n, from = min(values), to = max(values))
  step_size = diff(density_est$x)[1] # all the same
  p_i = step_size * density_est$y
  
   -sum(p_i * log(density_est$y + 10^-100)) - log(step_size)
}

LDDP_func(vals)
shannon_cont_entropy_func(vals)


#############
nodata = normal_maps$raster_data$original$count
nrdata = normal_maps$raster_data$random$count
nvdata = normal_maps$raster_data$vonoroi$count

plot(density(nodata))
lines(density(nrdata), col = "blue")
lines(density(nvdata), col = "red")

shannon_cont_entropy_func(nodata)
shannon_cont_entropy_func(nrdata)
shannon_cont_entropy_func(nvdata)

LDDP_func(nodata)
LDDP_func(nrdata)
LDDP_func(nvdata)


unique(nvdata)


density_est = density(nvdata, n = 1000, from = min(nvdata), to = max(nvdata))

plot(density_est)
step_size = diff(density_est$x)[1] # all the same
p_i = step_size * density_est$y
p_i <- p_i / sum(p_i) # normalize p_i
sum(p_i)

plot(density_est$x, p_i, type = "l") # probability plot of nvdata
points(density_est$x, rep(1/length(p_i), length(p_i)), type = "l") # a comparable uniform distribution

-sum(p_i * log(density_est$y)) - log(step_size)


#################
# now for the KL
library("philentropy")
uniform = runif(100000, min(nvdata), max(nvdata))
unidensity_est = density(uniform, n = 10000, from= min(nvdata), to= max(nvdata))
unistep_size = diff(unidensity_est$x)[1] # all the same
p_iu = unistep_size * unidensity_est$y
sum(p_iu)

plot(density(p_i))

# kullbeck leibner for the uniform and the other one
KL(rbind(p_i, p_iu), unit = "log")

# now see if it similar to the LDDP
LDDP_func(nvdata)


density_est = density(nvdata, n = n, from = min(nvdata), to = max(nvdata))
step_size = diff(density_est$x)[1] # all the same
p_i = step_size * density_est$y
log(length(p_i)) - sum(p_i * (log(p_i + 10^-100) - log(1/length(p_i))))

#### try this on the beta distribution
set.seed(1)
vals = rgamma(1000000, 40, 50)
plot(density(vals))

dens.vals = density(vals) 
delta = diff(dens.vals$x)[1] # step size
f_mid = dens.vals$y #; f_mid = f_mid[seq(1,500,5)]
p_i = delta * (f_mid) # probabilities

KL(rbind(p_i, rep(1/length(p_i), length(p_i))), unit = "log")

LDDP_func(vals)










