




library(patchwork)

library(ggplot2)
library(gridExtra)

?grid.arrange
top = normal_maps$grid[1:3]
bottom = normal_maps$grid[4:5]


# heat  maps
# interesting: consistent (random vs vonoroi),  consistent (random vs weighted),  consistent (vonoroi vs weighted), clustered (random vs weighted), normal (random vs vonoroi)
grid.arrange(normal_maps$heat$original, normal_maps$heat$random,normal_maps$heat$grid,normal_maps$heat$vonoroi,normal_maps$heat$weighted, ncol = 3, nrow = 2)
grid.arrange(clustered_maps$heat$original, clustered_maps$heat$random,clustered_maps$heat$grid,clustered_maps$heat$vonoroi,clustered_maps$heat$weighted, ncol = 3, nrow = 2)
grid.arrange(random_maps$heat$original, random_maps$heat$random,random_maps$heat$grid,random_maps$heat$vonoroi,random_maps$heat$weighted, ncol = 3, nrow = 2)
grid.arrange(smallclusters_maps$heat$original, smallclusters_maps$heat$random,smallclusters_maps$heat$grid,smallclusters_maps$heat$vonoroi,smallclusters_maps$heat$weighted, ncol = 3, nrow = 2)
grid.arrange(consistent_maps$heat$original, consistent_maps$heat$random,consistent_maps$heat$grid,consistent_maps$heat$vonoroi,consistent_maps$heat$weighted, ncol = 3, nrow = 2)


# point maps
# interesting: normal (grid vs vonoroi), random (grid vs. vonoroi), clustered (grid vs vonoroi)
grid.arrange(normal_maps$point$original, normal_maps$point$random,normal_maps$point$grid,normal_maps$point$vonoroi,normal_maps$point$weighted, ncol = 3, nrow = 2)
grid.arrange(clustered_maps$point$original, clustered_maps$point$random,clustered_maps$point$grid,clustered_maps$point$vonoroi,clustered_maps$point$weighted, ncol = 3, nrow = 2)
grid.arrange(random_maps$point$original, random_maps$point$random,random_maps$point$grid,random_maps$point$vonoroi,random_maps$point$weighted, ncol = 3, nrow = 2)
grid.arrange(smallclusters_maps$point$original, smallclusters_maps$point$random,smallclusters_maps$point$grid,smallclusters_maps$point$vonoroi,smallclusters_maps$point$weighted, ncol = 3, nrow = 2)
grid.arrange(consistent_maps$point$original, consistent_maps$point$random,consistent_maps$point$grid,consistent_maps$point$vonoroi,consistent_maps$point$weighted, ncol = 3, nrow = 2)


# grids maps
# interesting: consistent (random vs vonoroi), smallclusters (grid vs vonoroi), random (grid vs vonoroi), normal (vonoroi vs weighted)
grid.arrange(normal_maps$grid$original, normal_maps$grid$random,normal_maps$grid$grid,normal_maps$grid$vonoroi,normal_maps$grid$weighted, ncol = 3, nrow = 2)
grid.arrange(clustered_maps$grid$original, clustered_maps$grid$random,clustered_maps$grid$grid,clustered_maps$grid$vonoroi,clustered_maps$grid$weighted, ncol = 3, nrow = 2)
grid.arrange(random_maps$grid$original, random_maps$grid$random,random_maps$grid$grid,random_maps$grid$vonoroi,random_maps$grid$weighted, ncol = 3, nrow = 2)
grid.arrange(smallclusters_maps$grid$original, smallclusters_maps$grid$random,smallclusters_maps$grid$grid,smallclusters_maps$grid$vonoroi,smallclusters_maps$grid$weighted, ncol = 3, nrow = 2)
grid.arrange(consistent_maps$grid$original, consistent_maps$grid$random,consistent_maps$grid$grid,consistent_maps$grid$vonoroi,consistent_maps$grid$weighted, ncol = 3, nrow = 2)


### Interesting ones
# HEAT: consistent (random vs vonoroi),  consistent (random vs weighted),  consistent (vonoroi vs weighted), clustered (random vs weighted), normal (random vs vonoroi)
grid.arrange(consistent_maps$heat$random, consistent_maps$heat$original,consistent_maps$heat$vonoroi, ncol = 3, nrow = 1)
grid.arrange(consistent_maps$heat$random, consistent_maps$heat$original,consistent_maps$heat$weighted, ncol = 3, nrow = 1)
grid.arrange(consistent_maps$heat$vonoroi, consistent_maps$heat$original,consistent_maps$heat$weighted, ncol = 3, nrow = 1)
grid.arrange(clustered_maps$heat$random, clustered_maps$heat$original,clustered_maps$heat$weighted, ncol = 3, nrow = 1)
grid.arrange(normal_maps$heat$random, normal_maps$heat$original,normal_maps$heat$vonoroi, ncol = 3, nrow = 1)

# POINT: normal (grid vs vonoroi), random (grid vs. vonoroi), clustered (grid vs vonoroi)
grid.arrange(normal_maps$point$grid, normal_maps$point$original,normal_maps$point$vonoroi, ncol = 3, nrow = 1)
grid.arrange(random_maps$point$grid, random_maps$point$original,random_maps$point$vonoroi, ncol = 3, nrow = 1)
grid.arrange(clustered_maps$point$grid, clustered_maps$point$original,clustered_maps$point$vonoroi, ncol = 3, nrow = 1)

# GRID: consistent (random vs vonoroi), smallclusters (grid vs vonoroi), random (grid vs vonoroi), normal (vonoroi vs weighted)
grid.arrange(consistent_maps$grid$random, consistent_maps$grid$original,consistent_maps$grid$vonoroi, ncol = 3, nrow = 1)
grid.arrange(smallclusters_maps$grid$grid, smallclusters_maps$grid$original,smallclusters_maps$grid$vonoroi, ncol = 3, nrow = 1)
grid.arrange(random_maps$grid$grid, random_maps$grid$original,random_maps$grid$vonoroi, ncol = 3, nrow = 1)
grid.arrange(normal_maps$grid$weighted, normal_maps$grid$original,normal_maps$grid$vonoroi, ncol = 3, nrow = 1)



######################## uiteindelijke maps voor de quali survey
## HEATMAPS
# consistent: random, vonoroi, weighted
grid.arrange(consistent_maps$heat$random, consistent_maps$heat$vonoroi,consistent_maps$heat$weighted, consistent_maps$heatmap$grid, ncol = 4, nrow = 1)
consistent_maps$heatmap$original
# clustered: random weighted
grid.arrange(clustered_maps$heat$random, clustered_maps$heat$original,clustered_maps$heat$weighted, ncol = 3, nrow = 1)
# normal: random vonoroi
grid.arrange(normal_maps$heat$random, normal_maps$heat$original,normal_maps$heat$vonoroi, ncol = 3, nrow = 1)

# POINT
# random (grid vs. vonoroi), 
grid.arrange(random_maps$point$grid, random_maps$point$random,random_maps$point$vonoroi, random_maps$point$weighted, ncol = 4, nrow = 1)
grid.arrange(random_maps$point$original, ncol = 4, nrow = 1)
# clustered (grid vs vonoroi)
grid.arrange(clustered_maps$point$grid, clustered_maps$point$original,clustered_maps$point$vonoroi, ncol = 3, nrow = 1)

# GRID: consistent (random vs vonoroi), smallclusters (grid vs vonoroi), random (grid vs vonoroi), normal (vonoroi vs weighted)
grid.arrange(consistent_maps$grid$random, consistent_maps$grid$weighted,consistent_maps$grid$vonoroi, consistent_maps$grid$grid, ncol = 4, nrow = 1)
grid.arrange(consistent_maps$grid$original, ncol = 4, nrow = 1)


grid.arrange(smallclusters_maps$grid$grid, smallclusters_maps$grid$original,smallclusters_maps$grid$vonoroi, ncol = 3, nrow = 1)
grid.arrange(random_maps$grid$grid, random_maps$grid$original,random_maps$grid$vonoroi, ncol = 3, nrow = 1)
grid.arrange(normal_maps$grid$weighted, normal_maps$grid$original,normal_maps$grid$vonoroi, ncol = 3, nrow = 1)


jpeg()
grid.arrange(normal_maps$point$original, 
             clustered_maps$point$original,
             random_maps$point$original, 
             smallclusters_maps$point$original,
             consistent_maps$point$original, ncol = 5, nrow = 1)


for (i in vector) {
  
}

grid.arrange(normal_maps$heat[[1]],
             normal_maps$heat[[2]], 
             normal_maps$heat[[3]],
             normal_maps$heat[[4]],
             normal_maps$heat[[5]],
             clustered_maps$heat[[1]],
             clustered_maps$heat[[2]],
             clustered_maps$heat[[3]],
             clustered_maps$heat[[4]],
             clustered_maps$heat[[5]],
             random_maps$heat[[1]],
             random_maps$heat[[2]],
             random_maps$heat[[3]],
             random_maps$heat[[4]],
             random_maps$heat[[5]],
             smallclusters_maps$heat[[1]],
             smallclusters_maps$heat[[2]],
             smallclusters_maps$heat[[3]],
             smallclusters_maps$heat[[4]],
             smallclusters_maps$heat[[5]],
             consistent_maps$heat[[1]],
             consistent_maps$heat[[2]],
             consistent_maps$heat[[3]],
             consistent_maps$heat[[4]],
             consistent_maps$heat[[5]], ncol = 5, nrow = 5)

grid.arrange(normal_maps$heat$original, 
             clustered_maps$heat$original,
             random_maps$heat$original, 
             smallclusters_maps$heat$original,
             consistent_maps$heat$original, ncol = 5, nrow = 1)





