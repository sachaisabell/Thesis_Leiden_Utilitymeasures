
### displacement distance
normal_displacement = sapply(normal_maps$point_data[2:5], function(x) displacement_distance(x, normal_maps$point_data[[1]]))
clusterd_displacement = sapply(clustered_maps$point_data[2:5], function(x) displacement_distance(x, clustered_maps$point_data[[1]]))
random_displacement = sapply(random_maps$point_data[2:5], function(x) displacement_distance(x, random_maps$point_data[[1]]))
consistent_displacement = sapply(consistent_maps$point_data[2:5], function(x) displacement_distance(x, consistent_maps$point_data[[1]]))
smallclusters_displacement = sapply(smallclusters_maps$point_data[2:5], function(x) displacement_distance(x, smallclusters_maps$point_data[[1]]))


data_names = c("normal","clustered","random","smallclusters","consistent")


### MDi
t(rbind(sapply(2:5, function(x) MDI_func(normal_maps$point$original,normal_maps$point[[x]]))))
t(rbind(sapply(2:5, function(x) MDI_func(clustered_maps$point$original,clustered_maps$point[[x]]))))
t(rbind(sapply(2:5, function(x) MDI_func(random_maps$point$original,random_maps$point[[x]]))))
t(rbind(sapply(2:5, function(x) MDI_func(smallclusters_maps$point$original,smallclusters_maps$point[[x]]))))
t(rbind(sapply(2:5, function(x) MDI_func(consistent_maps$point$original,consistent_maps$point[[x]]))))



### KWD
sapply(data_names, function(x) {
  current = get(paste0(x,"_maps"))$raster_data
  original = get(paste0(x,"_maps"))$raster_data$original # the original data
  
  sapply(current, function(y){
    KWD_func(y$count, original$count)
  })
})



















