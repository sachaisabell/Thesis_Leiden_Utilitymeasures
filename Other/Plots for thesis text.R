######################## 
### Data used for all the plots
########################
set.seed(1)
dwellings_maps = p.g.h.maps2(dwellings[sample(1:nrow(dwellings), 200),1:2])
set.seed(1)
dwellings_maps2 = p.g.h.maps(dwellings[sample(1:nrow(dwellings), 200),1:2])


### heatmap specific


##### chapter 3: VMR
raster1 = raster2 = raster::raster(nrow = 6, ncol = 6)
extent(raster1) = extent(raster2) = c(rep(c(0,1),2))
values(raster1) = c(rep(c(0,1,0,1,0,1,
                          1,0,1,0,1,0),3))
values(raster2) = abs(1 - values(raster1))

par(mfrow = c(1,2)) 
plot(raster1);plot(raster2)


raster3 = raster2
values(raster3) = c(rep(1,18), rep(0,18))
plot(raster3)

raster4 = raster2
values(raster4) = sample(c(rep(1,18), rep(0,18)), 36)
plot(raster4)

### rook or queen explanation
raster.rook = raster1
raster.rook = raster::raster(nrow = 5, ncol = 5)
extent(raster.rook) = c(rep(c(0,1), 2))
values(raster.rook) = c(0,1,0,1,2,1,0,1,0)

raster.queen = raster.rook
values(raster.queen) = c(rep(0,5), 0,1,1,1,0,
                         0,1,2,1,0,
                         0,1,1,1,0, rep(0,5))


plot(raster.rook, axes = FALSE, legend = FALSE, , col = c("white","grey","black"))
plot(raster.queen, axes = FALSE, legend = FALSE, col = c("white", "grey","black"))

neibs = spdep::cell2nb(5, 5)
mat1 = spdep::nb2mat(neibs)

######################################## Ripleys K 

df = dwellings_maps2$point_data$original
dfsafe = dwellings_maps2$point_data$random

dwell_limits = dwellings_maps2$point$original$coordinates$limits
win = as.ppp(df, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
win2 = as.ppp(dfsafe, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))

# original
RipL = Lest(win ,correction="isotropic")
RipK = Kest(win ,correction="isotropic")

# random
RipL2 = Lest(win2 ,correction="isotropic", r = RipL$r)
RipK2 = Kest(win2 ,correction="isotropic", r = RipK$r)

################ Ripleys K
par(mfrow = c(1,1))
plot(RipK$r, RipK$iso, col = "blue", type = "l", xlab = "r")
lines(RipK2$r, RipK2$iso, col = "red")
lines(RipK2$r, RipK2$theo, col = "black")

################ L and other transformations
par(mfrow = c(1,4))
plot(RipK$r, RipK$iso, col = "blue", type = "l", xlab = "r")
lines(RipK2$r, RipK2$iso, col = "red")
lines(RipK2$r, RipK2$theo, col = "black")

plot(RipL$r, RipL$iso, col = "blue", type = "l", xlab = "r")
lines(RipL2$r, RipL2$iso, col = "red")
lines(RipL2$r, RipL2$theo, col = "black")

plot(RipL$r, RipL$iso - RipL$r, col = "blue", type = "l", xlab = "r")
lines(RipL2$r, RipL2$iso - RipL2$r, col = "red")
lines(RipL2$r, RipL2$theo - RipL2$r, col = "black")

plot(RipK$r, (RipK$iso / (pi*RipK$r^2)), col = "blue", type = "l", xlab = "r")
lines(RipK2$r, RipK2$iso / (pi*RipK2$r^2), col = "red")
lines(RipK2$r, RipK2$theo / (pi*RipK2$r^2), col = "black")

#################### cross functions and d function
cross = rbind(df, dfsafe)
win.cross = as.ppp(cross, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
win.cross$origin = c(rep("a",nrow(df)), rep("b", nrow(dfsafe)))
win.cross$origin = as.factor(win.cross$origin)
marks(win.cross) <- win.cross$origin

### k-cross, mcross, d function
kcross1 = Kcross(win.cross, "a", "b", correction = "isotropic")
win.cross2 = as.wmppp(win.cross)
RipD = Dhat(win.cross2, Cases = "a", Controls = "b")
RipM = Mhat(win.cross2, ReferenceType = "a", NeighborType = "b")

par(mfrow = c(1,3))
plot(RipD$r, RipD$iso, col = "purple", type = "l", xlab = "r")
lines(RipD$r, RipD$theo, col = "black")

plot(kcross1$r, kcross1$iso, col = "purple", type = "l", xlab = "r")
lines(kcross1$r, kcross1$theo, col = "black")

plot(RipM$r, RipM$M, col = "purple", type = "l", xlab = "r")
lines(RipM$r, RipM$theo, col = "black")



#####################
#### hotspot measures

hotspot_dwelling = bounded_hotspots_hdr(dwellings_maps$heatmap$original, 40)
hotspot_dwelling_safe = bounded_hotspots_hdr(dwellings_maps$heatmap$random, 40)

library(sf)
library(ggplot2)
library(gridExtra)

p1 <- ggplot() +
  geom_sf(data = hotspot_dwelling, fill = "blue") +
  theme_void()

p2 <- ggplot() +
  geom_sf(data = hotspot_dwelling_safe, fill = "red") +
  theme_void()
  


grid.arrange(dwellings_maps2$point$original + theme_void() + theme(panel.border = element_rect(colour = "black", fill = NA)), p1, 
             dwellings_maps2$point$random + theme_void() + theme(panel.border = element_rect(colour = "black", fill = NA)), p2, ncol = 4)



##### boxcount
par(mfrow = c(2,5))
boxcount_dwel = output_measure(box_counting_dim_func, hotspot_dwelling_safe, hotspot_dwelling, "", eps = 5) 
epsis = 2*2^(1:5-1) # 5 from the eps argument
y.safe = (boxcount1[[1]]$safe + boxcount1[[2]]$safe)/max(hotspot_og$clust)
y.og = (boxcount1[[1]]$original + boxcount1[[2]]$original)/max(hotspot_og$clust)
plot(epsis, y.safe, col = "blue", type = "l"); lines(epsis, y.og, col = "red")

par(mfrow = c(1,3))
symmdiff_dwel = output_measure(symmetric_diff_function, hotspot_dwelling_safe, hotspot_dwelling, "") 



########################### heatmap specific
par(mfrow = c(1,2))
dwell_ratio = heatmap_func(dwellings_maps$heatmap$original, dwellings_maps$heatmap$random, "ratio")
dwell_diff = heatmap_func(dwellings_maps$heatmap$original, dwellings_maps$heatmap$random, "simple")
dwell_ratio
dwell_diff



#################################
### the MDI illustration
# Set up plotting area
plot(NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1,
     xlab = "", ylab = "", axes = FALSE)


# Define the main point
p <- c(0.1, 0.7)
points(p[1], p[2], col = "blue", pch = 19)
text(p[1], p[2], labels = "P", pos = 3, col = "blue")

# Draw lines from P to boundaries
# Top and bottom edges
segments(p[1], p[2], p[1], 1, col = "gray")
segments(p[1], p[2], p[1], 0, col = "gray")

# Left and right edges
segments(p[1], p[2], 0, p[2], col = "gray")
segments(p[1], p[2], 1, p[2], col = "gray")

segments(0, 0, 0, 1, col = "black")
segments(0, 0, 1, 0, col = "black")
segments(1,1, 0, 1, col = "black")
segments(1,1, 1, 0, col = "black")

# Define the main point
p <- c(0.1, 0.7)
points(p[1], p[2], col = "blue", pch = 19)
text(p[1], p[2], labels = "P", pos = 3, col = "blue")


# Add a secondary point near bottom-right
q <- c(1, 0)
points(q[1], q[2], col = "red", pch = 19)
text(q[1], q[2], labels = "Q", pos = 1, col = "red")

q = rep(1, 8)
r = c(0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)
points(q[1:8], r[1:8], col = "red", pch = 19)


# Optional: diagonal to actual farthest point (corner)
segments(q[1:8], r[1:8], p[1], p[2], col = "darkred", lwd = 2, lty = 3)


#####################
#### local morans I example
par(mfrow  = c(1,3))
dwel_rast = raster(ncol = 35, nrow = 35); values(dwel_rast) = dwellings_maps$raster_data$original$count;extent(dwel_rast) = c(0,1,0,1)
plot(dwel_rast, asp = 1)


#########
### differentm map types
grid.arrange(dwellings_maps$point$original, dwellings_maps$grid$original + theme(legend.position = "none"), dwellings_maps$heatmap$original + theme(panel.border = element_rect(colour = "black", fill = NA)),
             OV_og_plot + theme(legend.position = "none"), ncol = 4)


dwellings_maps$point$original

OV_og_plot + theme(legend.position = "none")

### hausdorff
poly1 <- st_polygon(list(rbind(
  c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)
)))

# Create polygon 2 (slightly shifted)
poly2 <- st_polygon(list(rbind(
  c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1)
)))

# Convert to sf objects
sf1 <- st_sfc(poly1)
sf2 <- st_sfc(poly2)


plot(random_hotspots$grid$geometry)
plot(random_hotspots$original$geometry)

grid.arrange(UT_og_plot,
             UT_small_plot,
             UT_medium_plot,
             UT_large_plot,
             OV_og_plot,
             OV_small_plot,
             OV_medium_plot,
             OV_large_plot,
             NB_og_plot,
             NB_small_plot,
             NB_medium_plot,
             NB_large_plot,ncol = 4)

plot(coordinates_matrix(smallclusters_hotspots$original$geometry))

### all maps
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

########## all hotspots
par(mfrow = c(5,5))
datanames = c("normal", "clustered", "random", "smallclusters", "consistent")

for (i in 1:length(datanames)) {
  hsp = get(paste0(datanames[i],"_hotspots"))
  
  plot(hsp$original$geometry)
  plot(hsp$random$geometry)
  plot(hsp$grid$geometry)
  plot(hsp$vonoroi$geometry)
  plot(hsp$weighted$geometry)
}

par(mfrow = c(5,5))
datanames = c("normal", "clustered", "random", "smallclusters", "consistent")

for (i in 1:length(datanames)) {
  hsp = get(paste0(datanames[i],"_hotspots_grid"))
  
  plot(hsp$original$geometry)
  plot(hsp$random$geometry)
  plot(hsp$grid$geometry)
  plot(hsp$vonoroi$geometry)
  plot(hsp$weighted$geometry)
}


