



datanames = c("normal", "clustered", "random", "smallclusters", "consistent")
sdcnames = c("Random", "Grid", "Vonoroi", "Weighted")

#####
#### Ratio differencing
for(j in 1:length(datanames)){
  current_map = get(paste0(datanames[j], "_maps2"))$heatmap
  
 
  jpeg(paste0(datanames[j],"_ratioheatmap2.jpg"), width = 600, height = 350)
  
  par(mfrow = c(1,4))
  heatmap_func(current_map[[2]], current_map[[1]],"ratio", title = paste0("ratio ",datanames[j] ,": ", sdcnames[2-1]))
  heatmap_func(current_map[[3]], current_map[[1]],"ratio", title = paste0("ratio ",datanames[j] ,": ", sdcnames[3-1]))
  heatmap_func(current_map[[4]], current_map[[1]],"ratio", title = paste0("ratio ",datanames[j] ,": ", sdcnames[4-1]))
  heatmap_func(current_map[[5]], current_map[[1]],"ratio", title = paste0("ratio ",datanames[j] ,": ", sdcnames[5-1]))
  
  dev.off()
}

### Simple differencing
for(j in 1:length(datanames)){
  current_map = get(paste0(datanames[j], "_maps2"))$heatmap
  

  jpeg(paste0(datanames[j],"_simpleheatmap.jpg"), width = 600, height = 350)

  par(mfrow = c(1,4))
  heatmap_func(current_map[[2]], current_map[[1]],"simple", title = paste0("simple ",datanames[j] ,": ", sdcnames[2-1]))
  heatmap_func(current_map[[3]], current_map[[1]],"simple", title = paste0("simple ",datanames[j] ,": ", sdcnames[3-1]))
  heatmap_func(current_map[[4]], current_map[[1]],"simple", title = paste0("simple ",datanames[j] ,": ", sdcnames[4-1]))
  heatmap_func(current_map[[5]], current_map[[1]],"simple", title = paste0("simple ",datanames[j] ,": ", sdcnames[5-1]))
    
  dev.off()
}


### Hedi
hedidf = data.frame(matrix(rep(NA,20), nrow = 5))

for(j in 1:5){
  current_map = get(paste0(datanames[j], "_maps2"))$heatmap
  
  hedidf[j,1] = heatmap_func(current_map[[2]], current_map[[1]],"hedi", title = paste0("simple ",datanames[j] ,": ", sdcnames[2-1]))
  hedidf[j,2] = heatmap_func(current_map[[3]], current_map[[1]],"hedi", title = paste0("simple ",datanames[j] ,": ", sdcnames[3-1]))
  hedidf[j,3] = heatmap_func(current_map[[4]], current_map[[1]],"hedi", title = paste0("simple ",datanames[j] ,": ", sdcnames[4-1]))
  hedidf[j,4] = heatmap_func(current_map[[5]], current_map[[1]],"hedi", title = paste0("simple ",datanames[j] ,": ", sdcnames[5-1]))
  
}




















































