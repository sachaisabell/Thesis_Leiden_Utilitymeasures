



### KL
sapply(normal_maps$raster_data[2:5], function(x) entropy_func(normal_maps$raster_data$original$count, x$count, method = "KL"))
sapply(clustered_maps$raster_data[2:5], function(x) entropy_func(clustered_maps$raster_data$original$count, x$count, method = "KL"))
sapply(random_maps$raster_data[2:5], function(x) entropy_func(random_maps$raster_data$original$count, x$count, method = "KL"))
sapply(smallclusters_maps$raster_data[2:5], function(x) entropy_func(smallclusters_maps$raster_data$original$count, x$count, method = "KL"))
sapply(consistent_maps$raster_data[2:5], function(x) entropy_func(consistent_maps$raster_data$original$count, x$count, method = "KL"))

sapply(UT_list[1:3], function(x) entropy_func(UT_list[[4]], x, method = "KL"))
sapply(OV_list[1:3], function(x) entropy_func(OV_list[[4]], x, method = "KL"))
sapply(NB_list[1:3], function(x) entropy_func(NB_list[[4]], x, method = "KL"))



### LDDP
set.seed(1)
View(t(rbind(
sapply(normal_maps$raster_data[2:5], function(x) entropy_func(normal_maps$raster_data$original$count, x$count, method = "LDDP")),
sapply(clustered_maps$raster_data[2:5], function(x) entropy_func(clustered_maps$raster_data$original$count, x$count, method = "LDDP")),
sapply(random_maps$raster_data[2:5], function(x) entropy_func(random_maps$raster_data$original$count, x$count, method = "LDDP")),
sapply(smallclusters_maps$raster_data[2:5], function(x) entropy_func(smallclusters_maps$raster_data$original$count, x$count, method = "LDDP")),
sapply(consistent_maps$raster_data[2:5], function(x) entropy_func(consistent_maps$raster_data$original$count, x$count, method = "LDDP"))
)))

set.seed(1)
LDDPA = ((c(
sapply(UT_list[1:3], function(x) entropy_func(UT_list[[4]], x, method = "LDDP")),
sapply(OV_list[1:3], function(x) entropy_func(OV_list[[4]], x, method = "LDDP")),
sapply(NB_list[1:3], function(x) entropy_func(NB_list[[4]], x, method = "LDDP"))
)))

### shannon discrete
set.seed(1)

View(t(rbind(
sapply(normal_maps$raster_data[2:5], function(x) entropy_func(normal_maps$raster_data$original$count, x$count, method = "Shannon")),
sapply(clustered_maps$raster_data[2:5], function(x) entropy_func(clustered_maps$raster_data$original$count, x$count, method = "Shannon")),
sapply(random_maps$raster_data[2:5], function(x) entropy_func(random_maps$raster_data$original$count, x$count, method = "Shannon")),
sapply(smallclusters_maps$raster_data[2:5], function(x) entropy_func(smallclusters_maps$raster_data$original$count, x$count, method = "Shannon")),
sapply(consistent_maps$raster_data[2:5], function(x) entropy_func(consistent_maps$raster_data$original$count, x$count, method = "Shannon"))
)))

SEA = ((c(
sapply(UT_list[1:3], function(x) entropy_func(UT_list[[4]], x, method = "Shannon")),
sapply(OV_list[1:3], function(x) entropy_func(OV_list[[4]], x, method = "Shannon")),
sapply(NB_list[1:3], function(x) entropy_func(NB_list[[4]], x, method = "Shannon"))
)))


### shannon continuous
set.seed(1)
View((rbind(
sapply(normal_maps$raster_data[2:5], function(x) entropy_func(normal_maps$raster_data$original$count, x$count, method = "Diff")),
sapply(clustered_maps$raster_data[2:5], function(x) entropy_func(clustered_maps$raster_data$original$count, x$count, method = "Diff")),
sapply(random_maps$raster_data[2:5], function(x) entropy_func(random_maps$raster_data$original$count, x$count, method = "Diff")),
sapply(smallclusters_maps$raster_data[2:5], function(x) entropy_func(smallclusters_maps$raster_data$original$count, x$count, method = "Diff")),
sapply(consistent_maps$raster_data[2:5], function(x) entropy_func(consistent_maps$raster_data$original$count, x$count, method = "Diff"))
)))
set.seed(1)

DEA = ((c(
sapply(UT_list[1:3], function(x) entropy_func(UT_list[[4]], x, method = "Diff")),
sapply(OV_list[1:3], function(x) entropy_func(OV_list[[4]], x, method = "Diff")),
sapply(NB_list[1:3], function(x) entropy_func(NB_list[[4]], x, method = "Diff"))
)))


View(cbind(LDDPA, SEA, DEA))

#######
entropy_func(clustered_maps$raster_data$original$count, clustered_maps$raster_data$random$count, method = "Diff")
shannon_cont_entropy_func(clustered_maps$raster_data$random$count)








abs((og.diff) - (safe.diff)) / (abs(og.diff) + abs(safe.diff))









