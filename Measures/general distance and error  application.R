
#### for areal
### MSE
MSEA = c(sapply(UT_list[1:3], function(x) MSE_func(UT_list[[4]], x)),
sapply(OV_list[1:3], function(x) MSE_func(OV_list[[4]], x)),
sapply(NB_list[1:3], function(x) MSE_func(NB_list[[4]], x)))

### MAE
MAEA = c(sapply(UT_list[1:3], function(x) MAE_func(UT_list[[4]], x)),
sapply(OV_list[1:3], function(x) MAE_func(OV_list[[4]], x)),
sapply(NB_list[1:3], function(x) MAE_func(NB_list[[4]], x)))

### SME
SMEA = c(sapply(UT_list[1:3], function(x) SME_func(UT_list[[4]], x)),
sapply(OV_list[1:3], function(x) SME_func(OV_list[[4]], x)),
sapply(NB_list[1:3], function(x) SME_func(NB_list[[4]], x)))

### Hellinger
HDA = c(sapply(UT_list[1:3], function(x) HD_func(UT_list[[4]], x)),
sapply(OV_list[1:3], function(x) HD_func(OV_list[[4]], x)),
sapply(NB_list[1:3], function(x) HD_func(NB_list[[4]], x)))

View(cbind(MSEA,SMEA,MAEA,HDA))


#### for grid
### MSE
sapply(normal_maps$raster_data[2:5], function(x) MSE_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MSE_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MSE_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MSE_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MSE_func(consistent_maps$raster_data$original$count, x$count))

### MAE
sapply(normal_maps$raster_data[2:5], function(x) MAE_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MAE_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MAE_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MAE_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MAE_func(consistent_maps$raster_data$original$count, x$count))

### SME
sapply(normal_maps$raster_data[2:5], function(x) SME_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) SME_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) SME_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) SME_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) SME_func(consistent_maps$raster_data$original$count, x$count))


### Hellinger
sapply(normal_maps$raster_data[2:5], function(x) HD_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) HD_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) HD_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) HD_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) HD_func(consistent_maps$raster_data$original$count, x$count))

























