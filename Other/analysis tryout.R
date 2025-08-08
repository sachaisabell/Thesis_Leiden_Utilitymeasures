


#### fake data from dataset 1
survey_norm = c(0.5,0.33,0.67,0.8)
measure1norm = c(0.1,1.34,5.4,2)
measure2norm = c(1.11,2,5.4,0)

normal_output = cbind(survey_norm, measure1norm,measure2norm)
normal_output = normal_output[order(-survey_norm),]
normal_output = apply(normal_output, 2, function(col) col/sum(col))

#### fake dataset 2
set.seed(1)
survey_rand = runif(4)
measure1rand = runif(4)
measure2rand = runif(4)

rand_output = cbind(survey_rand, measure1rand,measure2rand)
rand_output = rand_output[order(-survey_norm),]
rand_output = apply(rand_output, 2, function(col) col/sum(col))


######### some different plots
### original v measure 1 v measure 2
plot(normal_output[,1], ylim = c(0,1), type = "b")
lines(normal_output[,2], col = "red")
lines(normal_output[,3], col = "green")

# now try with calculating the error to the original line
plot((normal_output[,1]-normal_output[,1]), ylim = c(-1,1), type = "b")
lines((normal_output[,2]-normal_output[,1]), col = "red")
lines((normal_output[,3]-normal_output[,1]), col = "green")

# now with taking the order into account(1 most important)
cumsum_norm =apply(normal_output, 2,function(col) cumsum(col))
difference2survey_norm = apply(cumsum_norm, 2, function(col) (col - cumsum_norm[,1]))
absdifference2survey_norm = apply(cumsum_norm, 2, function(col) abs(col - cumsum_norm[,1]))

# simple difference
plot(difference2survey_norm[,1], type = "b", ylim = c(-1,1))
lines(difference2survey_norm[,2], type = "b", col = "red")
lines(difference2survey_norm[,3], type = "b", col = "green")

# absolute difference
plot(absdifference2survey_norm[,1], type = "b", ylim = c(0,1))
lines(absdifference2survey_norm[,2], type = "b", col = "red")
lines(absdifference2survey_norm[,3], type = "b", col = "green")

#### maybe just plot two scores against each other
plot(normal_output[,1], normal_output[,2], col = "red")
points(normal_output[,1], normal_output[,3], col = "")





####### DOES NOT WORK FOR US
##### normalized discounted gain
# the vector of all the DCGs
DCG_func = function(input, k){
  log2i = log((1:k + 1), base = 2)
  combined = input[1:k] / log2i
  
  return(sum(combined))
}

r_DCG_func = function(input, k){
  log2i = log((1:k + 1), base = 2)
  combined = input[1:k] / log2i
  
  return((combined))
}


est = sapply(1:6, function(x) DCG_func(c(3,2,3,0,1,2), x))
act = sapply(1:6, function(x) DCG_func(c(3,3,3,2,2,2), x))  

est1 = sapply(1:4, function(x) DCG_func(normal_output[,2], x))
est2 = sapply(1:4, function(x) DCG_func(normal_output[,3], x))
act = sapply(1:4, function(x) DCG_func(normal_output[,1], x))

##### Without standardization does seem to show nicely which ones are better
plot(1:4, est1, type = "l", ylim = c(0,1), col = "red")
lines(1:4, act, col = "black")
lines(1:4, est2, col = "green")


### now some examples with only changing one thing
est1 = sapply(1:4, function(x) DCG_func(normal_output[c(1,2,4,3),1], x))
est2 = sapply(1:4, function(x) DCG_func(normal_output[c(2,1,3,4),1], x))
act = sapply(1:4, function(x) DCG_func(normal_output[,1], x))

plot(1:4, est1, type = "l", ylim = c(0,1), col = "red")
lines(1:4, act, col = "black")
lines(1:4, est2, col = "green")

### now see if we can catch the differences in magnitude
est1 = sapply(1:4, function(x) DCG_func(normal_output[,2], x))
est2 = sapply(1:4, function(x) DCG_func(c(normal_output[1,2], 0.8, normal_output[3:4,2]), x))
act = sapply(1:4, function(x) DCG_func(normal_output[,1], x))

plot(1:4, est1/act, type = "l", col = "red", ylim = c(0.5,1.5))
lines(1:4, act/act, col = "black")
lines(1:4, est2/act, col = "green")


#### how about the absolute log of the ratio
est1 = sapply(1:4, function(x) DCG_func(normal_output[,2], x))
est2 = sapply(1:4, function(x) DCG_func(c(normal_output[1,2], 0.8, normal_output[3:4,2]), x))
act = sapply(1:4, function(x) DCG_func(normal_output[,1], x))

plot(1:4, abs(log(est1/act)), type = "l", col = "red")
lines(1:4, abs(log(act/act)), col = "black")
lines(1:4, abs(log(est2/act)), col = "green")

####### now without aggregation
est1 = r_DCG_func(normal_output[,2], 4)
est2 = r_DCG_func(normal_output[,3], 4)
act = r_DCG_func(normal_output[,1], 4)

################## PROMISING (NOTTT okay this is just the same as using the bare rankings)
######## maybe compare the different to their own ideal case?
ideal1 = sort(normal_output[,2], decreasing = TRUE)
ideal2 = sort(normal_output[,3], decreasing = TRUE)

est1 = sapply(1:4, function(x) DCG_func(normal_output[,2], x))
est2 = sapply(1:4, function(x) DCG_func(normal_output[,3], x))
act = sapply(1:4, function(x) DCG_func(normal_output[,1], x))
act1 = sapply(1:4, function(x) DCG_func(ideal1, x))
act2 = sapply(1:4, function(x) DCG_func(ideal2, x))

plot(1:4, ((est1/act1)), type = "l", col = "red", ylim = c(0,1))
lines(1:4, ((act/act)), col = "black")
lines(1:4, ((est2/act2)), col = "green")


############ examples to test this
### example 1: gives a nice approach
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.39,0.31,0.2,0.1)

### example 2: also as expected
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.31,0.39,0.2,0.1)

### example 3: pretty much as expected still pretty good
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = rev(c(0.39,0.31,0.2,0.1))

### example 3: this does not work out because the rankings are the same
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = rep(0.25, 4)


###### run this for the examples
ideal1 = sort(ex1, decreasing = TRUE)
ideal2 = sort(ex2, decreasing = TRUE)

est1 = sapply(1:4, function(x) DCG_func(ex1, x))
est2 = sapply(1:4, function(x) DCG_func(ex2, x))
act = sapply(1:4, function(x) DCG_func(true, x))
act1 = sapply(1:4, function(x) DCG_func(ideal1, x))
act2 = sapply(1:4, function(x) DCG_func(ideal2, x))

plot(1:4, ((est1/act1)), type = "l", col = "red", ylim = c(0,1))
lines(1:4, ((act/act)), col = "black")
lines(1:4, ((est2/act2)), col = "green")



##################### 
### New plan:
# we are going to calculate the simple DCG 
# and also for every curve and then take the absolute difference to the ideal one
### example 4: 
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.4,0.6,0,0)

### example 5: 
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.4,0.2,0.1,0.3)

### example 6: 
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.6,0.3,0.1,0.0)
ex2 = c(0.4,0.2,0.1,0.3)

### example 7: 
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.4,0.4,0.2,0.0)
ex2 = c(0.4,0.2,0.1,0.3)

### example 8:
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.2,0.4,0.2,0.2)
ex2 = rep(0.25, 4)


###### run this for the examples
est1 = sapply(1:4, function(x) DCG_func(ex1, x))
est2 = sapply(1:4, function(x) DCG_func(ex2, x))
act = sapply(1:4, function(x) DCG_func(true, x))

# simple DCG curves
plot(1:4, ((est1)), type = "l", col = "red", ylim = c(0,1))
lines(1:4, ((act)), col = "black")
lines(1:4, ((est2)), col = "green")

# simple DCG curves - ideal one
# plot(1:4, ((est1 - act)), type = "l", col = "red", ylim = c(-1,1))
# lines(1:4, ((act - act)), col = "black")
# lines(1:4, ((est2 - act)), col = "green")

######## above is simple but works rather nicely
# one more thing to try out is just using the difference in value as the value

###### run this for the examples
est1 = sapply(1:4, function(x) DCG_func(abs(ex1-true), x))
est2 = sapply(1:4, function(x) DCG_func(abs(ex2-true), x))
act = sapply(1:4, function(x) DCG_func(abs(true-true), x))

# simple DCG curves
plot(1:4, ((est1)), type = "l", col = "red", ylim = c(0,1))
lines(1:4, ((act)), col = "black")
lines(1:4, ((est2)), col = "green")



par(mfrow = c(2,1))


###########################
### lets make a function for the RBO
# the normalization comes from this: file:///C:/Users/1glas/Downloads/On_Rank-Biased_Overlap_with_Finite_and_Conjoint_Domains.pdf
# original paper: http://w.codalism.com/research/papers/wmz10_tois.pdf


RBO_func = function(list1, list2, p){
  n = length(list1)
  RBOi = rep(NA, n)
  
  for (i in 1:n) { # assuming list 1 and 2 have the same length
    nuniq = length(unique(c(list1[1:i], list2[1:i])))
    overlap = abs(nuniq - (i*2))/i
    
    RBOi[i] = (1-p)*p^(i-1)*overlap
  }
  
  RBO = cumsum(RBOi)/(1-p^(n))
  
  return(RBO)
  
}

l1 = c("a","b","c","d","e")
l2 = c("b", "a","c","d","e")
l3 = c("a","b","c","e","d")

# maybe plot the l1,l1 as the reference line
# NO: to do the cumulative we have to adjust the n argument
RBO_func(l1,l3, 0.50)
RBO_func(l1,l2, 0.50)
RBO_func(l1,l1, 0.50)


# if we adust the n argument to 1:n we are essentially assuming that these are the complete list, so basically:
# if we only look at the ith argument then this would be the score.
# If we do not adjust the n argument then we are comparing the first i argument knowing that there are more to come
# in this sense it may make sense to plot the line compare list1 to list 1 as well to compare the ideal version. Knowing that it can never be higher than this only lower
# and all will be cumulative functions



c(0.5,0.25,0.125,0.0625)/(1-0.5^5) 

1-0.5^4

