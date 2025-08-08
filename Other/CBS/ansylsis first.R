



# the vector of all the DCGs
DCG_func = function(input, k){
  log2i = log((1:k + 1), base = 2)
  combined = input[1:k] / log2i
  
  return(cumsum(combined))
}



### example 1
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.3,0.3,0.2,0.2)

### example 2
true = c(0.4,0.3,0.2,0.1)
ex1 = c(0.1,0.6,0.2,0.1)
ex2 = c(0.3,0.2,0.1,0.4)


### plotting the plain DCG
est1 = sapply(1:4, function(x) DCG_func(ex1, x))
est2 = sapply(1:4, function(x) DCG_func(ex2, x))
act = sapply(1:4, function(x) DCG_func(true, x))

plot(1:4, act, type = "l", col = "black", ylim = c(0,1), main = "DCG based on rank and value")
lines(est1, col = "red"); lines(est2, col = "green")

### plotting the DCG of the differences, maybe not particulairly usefull since it does not place enough on the ranking
est1 = sapply(1:4, function(x) DCG_func(abs(ex1-true), x))
est2 = sapply(1:4, function(x) DCG_func(abs(ex2-true), x))
act = sapply(1:4, function(x) DCG_func(abs(true-true), x))

plot(1:4, act, type = "l", col = "black", ylim = c(0,1), main = "DCG based on difference value between observed and actual")
lines(est1, col = "red"); lines(est2, col = "green")



#### now solely based on the ranking
### plotting the plain DCG
est1 = sapply(1:4, function(x) DCG_func(rank(ex1), x))
est2 = sapply(1:4, function(x) DCG_func(rank(ex2), x))
act = sapply(1:4, function(x) DCG_func(4:1, x))

plot(1:4, act/act, type = "l", col = "black", ylim = c(0,1), main = "only ranking")
lines(est1/act, col = "red"); lines(est2/act, col = "green")

plot(1:4, act, type = "l", col = "black", main = "only ranking")
lines(est1, col = "red"); lines(est2, col = "green")



##### we can also try a barplot
data_bar = rbind(act/act,est1/act, est2/act)

barplot(data_bar, beside = TRUE, main = "DCG barplot based on ideal rank", col = c("black","red","green"))


##### and just plotting the value can be helpful too
data_bar = rbind(true,ex1,ex2)

barplot(data_bar, beside = TRUE, main = "DCG barplot based on ideal rank", col = c("black","red","green"), ylim = c(0,1))




######################
#### okay so plan is now this
### barplot based on values
### DCG based on rank
### DCG based on rank and values

### firs generate some random data sets
set.seed(1)
dataset1 = abs(cbind(rnorm(4),rnorm(4),runif(4)), c("r","g","v","w")) # e.g. normal data point
dataset1 = apply(dataset1, 2, function(col) col/sum(col))[order(-dataset1[,1]),]

dataset2 = abs(cbind(rnorm(4),rnorm(4),runif(4))) # e.g. normal data point
dataset2 = apply(dataset2, 2, function(col) col/sum(col))[order(-dataset2[,1]),]

dataset3 = abs(cbind(rnorm(4),rnorm(4),runif(4))) # e.g. normal data point
dataset3 = apply(dataset3, 2, function(col) col/sum(col))[order(-dataset3[,1]),]

### Calculate the DCG based on rank and value
out1 = apply(dataset1, 2, function(x) DCG_func((x) , 4))
out2 = apply(dataset2, 2, function(x) DCG_func((x) , 4))
out3 = apply(dataset3, 2, function(x) DCG_func((x) , 4))

### Calculate DCG based on rank only
out11 = apply(dataset1, 2, function(x) DCG_func(rank(x) , 4))
out22 = apply(dataset2, 2, function(x) DCG_func(rank(x) , 4))
out33 = apply(dataset3, 2, function(x) DCG_func(rank(x) , 4))

########## now make the plots
par(mfrow = c(3,3))

# barplots with only counts
barplot(t(dataset1), beside = TRUE, main = " barplot based on values", col = c("black","red","green"), ylim = c(0,1))
barplot(t(dataset2), beside = TRUE, main = " barplot based on values", col = c("black","red","green"), ylim = c(0,1))
barplot(t(dataset3), beside = TRUE, main = " barplot based on values", col = c("black","red","green"), ylim = c(0,1))
# lets try with barplots also for DCG rank
barplot(t(out11), beside = TRUE, main = " DCG based on rank", col = c("black","red","green"), ylim = c(0,8))
barplot(t(out22), beside = TRUE, main = " DCG based on rank", col = c("black","red","green"), ylim = c(0,8))
barplot(t(out33), beside = TRUE, main = " DCG based on rank", col = c("black","red","green"), ylim = c(0,8))
# also as a barplot DCG rank and value
barplot(t(out1), beside = TRUE, main = " DCG based on rank/values", col = c("black","red","green"), ylim = c(0,1))
barplot(t(out2), beside = TRUE, main = " DCG based on rank/values", col = c("black","red","green"), ylim = c(0,1))
barplot(t(out3), beside = TRUE, main = " DCG based on rank/values", col = c("black","red","green"), ylim = c(0,1))

# DCG with rank only
plot(1:4, out11[,1], type = "l", col = "black", ylim = c(0,8), main = "DCG only ranking");lines(out11[,2], col = "red"); lines(out11[,3], col = "green")
plot(1:4, out22[,1], type = "l", col = "black", ylim = c(0,8), main = "DCG only ranking");lines(out22[,2], col = "red"); lines(out22[,3], col = "green")
plot(1:4, out33[,1], type = "l", col = "black", ylim = c(0,8), main = "DCG only ranking");lines(out33[,2], col = "red"); lines(out33[,3], col = "green")
# DCG with rank and value
plot(1:4, out1[,1], type = "l", col = "black", ylim = c(0,1), main = "DCG values and ranking");lines(out1[,2], col = "red"); lines(out1[,3], col = "green")
plot(1:4, out2[,1], type = "l", col = "black", ylim = c(0,1), main = "DCG values and ranking");lines(out2[,2], col = "red"); lines(out2[,3], col = "green")
plot(1:4, out3[,1], type = "l", col = "black", ylim = c(0,1), main = "DCG values and ranking");lines(out3[,2], col = "red"); lines(out3[,3], col = "green")



#### instead of the DCG for the rank only we can also use the RBO
rankvals = 1:4
rank1 = apply(dataset1, 2, rank)

NBO_func = function(ideal, examp){
  val = rep(NA, length(ideal))
  
  for (i in 1:4) {
    uniq = unique(c(ideal[1:i], examp[1:i])); print(uniq)
    val[i] = (2*i - length(uniq))/i
  }
  
  return(val)
}


one = c("a","b","c","d","e")
two = c("b","a","d")

NBO_func(one, two)






















