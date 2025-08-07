

### Average nearest neighbors
ANN_func = function(data){
  
  mean(nndist(data[,1], data[,2], k = 1))
  
}


### distance to k nearest neighbors
dist_knn_func = function(dataname){
  current = get(paste0(dataname,"_maps"))$point_data
  
  knnresult= sapply(current, function(x) colMeans(spatstat.geom::nndist(x[,1], x[,2], k = 1:(nrow(x) - 1))))

  plot(knnresult[,5] ~ eval(1:(nrow(knnresult))), type = "l", col = "purple",lwd = 2, ylim = c(min(knnresult),max(knnresult)), xlab = "k", ylab = "value", main = dataname)
  lines(knnresult[,2], type = "l", col = "pink", lwd = 2) 
  lines(knnresult[,3], type = "l", col = "red", lwd = 2) 
  lines(knnresult[,4], type = "l", col = "orange", lwd = 2) 
  lines(knnresult[,1], type = "l", col = "blue", lwd = 1) 
  legend("topleft",legend = c("Original","Random","Grid","Voronoi","Weighted"),pch = 20, col = c("blue","pink","red","orange","purple"), cex = 6, pt.cex = 6)
}


##### application
data_names = c("normal","clustered","random","smallclusters","consistent")

### ANN
ANN_results = sapply(data_names, function(x) {
  current = get(paste0(x,"_maps"))$point_data
  sapply(current, ANN_func)
})



#### KNN distances
jpeg("knnresults.jpg", width = 4000, height = 1000)
par(mfrow = c(1,5))
lapply(data_names, dist_knn_func)
dev.off()

