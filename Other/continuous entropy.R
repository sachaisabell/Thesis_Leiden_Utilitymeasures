
a = 40;b = 50
nn <- 100
p <- seq(0, 1, len = nn)
p_mid <- (p[-1] + p[-nn])/2
delta <- diff(p)[1]

# for beta
f_mid <- dbeta(p_mid, shape1=a, shape2=b)
p_i <- f_mid * delta
sum(p_i)

sum(p_i * log(p_i))

(H_delta <- -sum(p_i * log(p_i)))
H_delta + log(delta)

# for norm
f_mid <- dnorm(p_mid)

norm1 = function(x){
  dnorm(x) * dnorm(x, log = TRUE)
}

integrate(norm1, min(norms), max(norms))


######################

vals = rbeta(1000000, 40, 50)
plot(density(vals))

dens.vals = density(vals)
p_i = diff(dens.vals$x)[1] * dens.vals$y

-sum(p_i * log(p_i)) + log(diff(dens.vals$x)[1])


sum(p_i)



https://rpubs.com/SteadyStoffel/entropy1
https://math.stackexchange.com/questions/3536042/differential-entropy-and-limiting-density-of-discrete-points


## for norm
set.seed(1)
norms = rnorm(10000)


dens.vals = density(norms)
p_i = diff(dens.vals$x)[1] * dens.vals$y

-sum(p_i * log(p_i)) + log(diff(dens.vals$x)[1])


sum(p_i)












