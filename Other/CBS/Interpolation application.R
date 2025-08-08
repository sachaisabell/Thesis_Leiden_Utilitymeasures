








#### Nearest neighbours
set.seed(1)
sapply(normal_maps$point[2:5], function(x) avg_interpolation_func(normal_maps$point$original,x, "nearest"))
sapply(clustered_maps$point[2:5], function(x) avg_interpolation_func(clustered_maps$point$original,x, "nearest"))
sapply(random_maps$point[2:5], function(x) avg_interpolation_func(random_maps$point$original,x, "nearest"))
sapply(smallclusters_maps$point[2:5], function(x) avg_interpolation_func(smallclusters_maps$point$original,x, "nearest"))
sapply(consistent_maps$point[2:5], function(x) avg_interpolation_func(consistent_maps$point$original,x, "nearest"))


#### kernel density twice
set.seed(1)
sapply(normal_maps$point[2:5], function(x) avg_interpolation_func(normal_maps$point$original,x, "interp"))
sapply(clustered_maps$point[2:5], function(x) avg_interpolation_func(clustered_maps$point$original,x, "interp"))
sapply(random_maps$point[2:5], function(x) avg_interpolation_func(random_maps$point$original,x, "interp"))
sapply(smallclusters_maps$point[2:5], function(x) avg_interpolation_func(smallclusters_maps$point$original,x, "interp"))
sapply(consistent_maps$point[2:5], function(x) avg_interpolation_func(consistent_maps$point$original,x, "interp"))
























