

########## GLOBAL
### VMR
sapply(normal_maps$raster_data[2:5], function(x) VMR_function(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) VMR_function(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) VMR_function(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) VMR_function(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) VMR_function(consistent_maps$raster_data$original$count, x$count))

### MMR
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count))

### MMR*
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count, option = "MMR*"))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count, option = "MMR*"))

### MMR*
sapply(normal_maps$raster_data[2:5], function(x) MMR_function(normal_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(clustered_maps$raster_data[2:5], function(x) MMR_function(clustered_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(random_maps$raster_data[2:5], function(x) MMR_function(random_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(smallclusters_maps$raster_data[2:5], function(x) MMR_function(smallclusters_maps$raster_data$original$count, x$count, option = "MMR**"))
sapply(consistent_maps$raster_data[2:5], function(x) MMR_function(consistent_maps$raster_data$original$count, x$count, option = "MMR**"))

### Morans I global
sapply(normal_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) moransI_grid_func(x$count, version = "global", type = "queen"))

### Getis ord global
sapply(normal_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) Gi_grid_func(x$count, version = "global", type = "queen"))

### Geary global
sapply(normal_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(clustered_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(random_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(smallclusters_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))
sapply(consistent_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "global", type = "queen"))

####### LOCAL
### Geary local
gearynorm = sapply(normal_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gearyclus = sapply(clustered_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gearyrand = sapply(random_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gearysmall = sapply(smallclusters_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "local", type = "queen"))
gearyconsis = sapply(consistent_maps$raster_data[1:5], function(x) Geary_grid_func(x$count, version = "local", type = "queen"))

layout(matrix(c(1,2,3,4,rep(5,4)), 2, 4, byrow = TRUE))
apply(gearynorm, 2, local_grid_map)

plot_func(cbind(normal_maps$raster_data[,1:2], gearynorm[,1]), title = "grid", type = "grid", xlim = 


a <- sample(1:100)
rbPal <- colorRampPalette(c('red','blue'))
b <- rbPal(10)[as.numeric(cut(a,breaks = 10))]
plot(a,col=b,pch=16)












