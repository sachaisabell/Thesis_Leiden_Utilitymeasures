




### MSE
sapply(normal_maps$raster_data[2:5], function(x) MSE_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MSE_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MSE_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MSE_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MSE_func(consistent_maps$raster_data$original$count, x$count))

sapply(UT_list[2:4], function(x) MSE_func(UT_list[[1]], x))
sapply(OV_list[2:4], function(x) MSE_func(OV_list[[1]], x))
sapply(NB_list[2:4], function(x) MSE_func(NB_list[[1]], x))


### MAE
sapply(normal_maps$raster_data[2:5], function(x) MAE_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MAE_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MAE_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MAE_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MAE_func(consistent_maps$raster_data$original$count, x$count))

sapply(UT_list[2:4], function(x) MAE_func(UT_list[[1]], x))
sapply(OV_list[2:4], function(x) MAE_func(OV_list[[1]], x))
sapply(NB_list[2:4], function(x) MAE_func(NB_list[[1]], x))


### SME
sapply(normal_maps$raster_data[2:5], function(x) SME_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) SME_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) SME_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) SME_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) SME_func(consistent_maps$raster_data$original$count, x$count))

sapply(UT_list[2:4], function(x) SME_func(UT_list[[1]], x))
sapply(OV_list[2:4], function(x) SME_func(OV_list[[1]], x))
sapply(NB_list[2:4], function(x) SME_func(NB_list[[1]], x))


### Hellinger
sapply(normal_maps$raster_data[2:5], function(x) HD_func(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) HD_func(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) HD_func(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) HD_func(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) HD_func(consistent_maps$raster_data$original$count, x$count))

sapply(UT_list[2:4], function(x) HD_func(UT_list[[1]], x))
sapply(OV_list[2:4], function(x) HD_func(OV_list[[1]], x))
sapply(NB_list[2:4], function(x) HD_func(NB_list[[1]], x))






normal_maps$raster_data


















