
#source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Final R files/Survey files/final final survey maps.R")
source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Application measures/convert data.R")
source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Final R files/Geographic distance based.R")

### displacement distance
normal_displacement = sapply(normal_points[2:5], function(x) displacement_distance(x, normal_points[[1]]))
clusterd_displacement = sapply(clustered_points[2:5], function(x) displacement_distance(x, clustered_points[[1]]))
random_displacement = sapply(random_points[2:5], function(x) displacement_distance(x, random_points[[1]]))
consistent_displacement = sapply(consistent_points[2:5], function(x) displacement_distance(x, consistent_points[[1]]))
smallclusters_displacement = sapply(smallclusters_points[2:5], function(x) displacement_distance(x, smallclusters_points[[1]]))


data_names = c("normal","clustered","random","smallclusters","consistent")


### MDi
sapply(data_names, function(x) {
  current = get(paste0(x,"_maps"))$point
  sapply(current, MDI_func)
})



### KWD
sapply(data_names, function(x) {
  current = get(paste0(x,"_maps"))$raster_data
  original = get(paste0(x,"_maps"))$raster_data$original # the original data
  
  sapply(current, function(y){
    KWD_func(y$count, original$count)
  })
})

