



## ODI function
View(t(rbind(sapply(normal_maps$point_data[2:5], function(x) ODI_func(x, normal_maps$point_data$original)),
sapply(clustered_maps$point_data[2:5], function(x) ODI_func(x, clustered_maps$point_data$original)),
sapply(random_maps$point_data[2:5], function(x) ODI_func(x, random_maps$point_data$original)),
sapply(smallclusters_maps$point_data[2:5], function(x) ODI_func(x, smallclusters_maps$point_data$original)),
sapply(consistent_maps$point_data[2:5], function(x) ODI_func(x, consistent_maps$point_data$original))
)))




### convex polygon
sapply(normal_maps$point_data[2:5], function(x) convex_poly_func(x, normal_maps$point_data$original))
sapply(clustered_maps$point_data[2:5], function(x) convex_poly_func(x, clustered_maps$point_data$original))
sapply(random_maps$point_data[2:5], function(x) convex_poly_func(x, random_maps$point_data$original))
sapply(smallclusters_maps$point_data[2:5], function(x) convex_poly_func(x, smallclusters_maps$point_data$original))
sapply(consistent_maps$point_data[2:5], function(x) convex_poly_func(x, consistent_maps$point_data$original))



### ellipse overlap
sapply(normal_maps$point_data[2:5], function(x) ellipse_ratio_func(x, normal_maps$point_data$original))
sapply(clustered_maps$point_data[2:5], function(x) ellipse_ratio_func(x, clustered_maps$point_data$original))
sapply(random_maps$point_data[2:5], function(x) ellipse_ratio_func(x, random_maps$point_data$original))
sapply(smallclusters_maps$point_data[2:5], function(x) ellipse_ratio_func(x, smallclusters_maps$point_data$original))
sapply(consistent_maps$point_data[2:5], function(x) ellipse_ratio_func(x, consistent_maps$point_data$original))





























