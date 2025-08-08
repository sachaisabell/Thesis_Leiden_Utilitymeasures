


### Average nearest neighbors
ANN_func = function(original, safe){
  
  ANN.safe = mean(nndist(safe[,1], safe[,2], k = 1))
  ANN.og = mean(nndist(original[,1], original[,2], k = 1))
  
  return(c(ANN.og,ANN.safe))
}



### distance to k nearest neighbors
dist_knn_func = function(original, safe){
  safe.row = nrow(safe)
  og.row = nrow(original)
  
  kn.safe = colMeans(spatstat.geom::nndist(safe[,1], safe[,2], k = 1:(safe.row - 1)))
  kn.og = colMeans(spatstat.geom::nndist(original[,1], original[,2], k = 1:(og.row - 1)))
 
  plot(kn.og ~ eval(1:(og.row - 1)), type = "b", col = "red", ylim = c(min(kn.og, kn.safe),max(kn.og, kn.safe))); lines(kn.safe, type = "b", col = "blue") 
}





### examples
ANN_func(dwellings[1:1000,1:2], sdcSpatial:: mask_random(dwellings[1:1000,1:2], 500))

dist_knn_func(dwellings[1:1000,1:2], sdcSpatial:: mask_random(dwellings[1:1000,1:2], 500))











































