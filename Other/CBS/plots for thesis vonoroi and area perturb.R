


x <- cbind(
  x = c(2.5, 3.5, 7.2, 1.5, 2*c(2.5, 3.5, 7.2, 1.5), 3*c(2.5, 3.5, 7.2, 1.5)),
  y = c(6.2, 3.8, 4.4, 2.1, 2*c(6.2, 3.8, 4.4, 2.1), 3*c(6.2, 3.8, 4.4, 2.1))
)
# plotting is only useful from small datasets!
# grid masking
x_g <- mask_grid(x, plot=TRUE, r = 1)




xx = mask_voronoi(normal_data, plot = TRUE, ylim = c(42, 42.4))
xx = mask_random(normal_data[1:100,], r = 10, plot = TRUE)


plot(xx)
library(sp)

p = st_as_sf(data.frame(normal_data),coords=1:2)
v = st_voronoi_point(p)


st_voronoi_point <- function(points){
  ## points must be POINT geometry
  # check for point geometry and execute if true
  if(!all(st_geometry_type(points) == "POINT")){
    stop("Input not  POINT geometries")
  }
  g = st_combine(st_geometry(points)) # make multipoint
  v = st_voronoi(g)
  v = st_collection_extract(v)
  return(v[unlist(st_intersects(points, v))])
}

plot(v, axes = TRUE, ylim = c(42,42.4),xlim = c(-71.4,-70.7), asp  =1)
plot(p, add = TRUE, col = "blue", pch = 20)



library(cbsodataR)




ogp = cities_sf$geometry[100]

buffers = st_buffer(ogp, 100000)
inters =  st_intersection(buffers, nl_outline)

sfp = st_sample(inters, 1)

plot(nl_outline)
plot(inters, col = "lightblue", add = TRUE)
plot(ogp, add = TRUE, lwd = 5, pch = 20, col = "blue")
plot(sfp, add = TRUE, lwd = 5, pch = 20, col = "red")

###############
### for frechet points

par(mfrow = c(1,2))
x = c(0,1,2,4,4,3,2,1,0)
y = c(2,1,1,2,3,4,4,3,2)

plot(x,y, axes = FALSE, type = "b", xlab = "", ylab = "")
text(x,y,c(1:8), pos = 2, cex = 2)


plot(rev(x), rev(y), axes = FALSE, type = "b", xlab = "", ylab = "")
text(rev(x), rev(y),c(1:8), pos = 2, cex = 2)

###########
## boxcount average over
b1 = c(1:5)
b2 = c(0.8*1:5)
r1 = c(0.5*1:5)
r2 = c(0.4*1:5)
y = 1:5

par(mfrow = c(1,2))
plot(y, b1, type = "b", col = "blue", ylim = c(0,5), ylab = "log N(r)", xlab = "log (1/r)")
lines(y,b2, col = "blue", type = "b")
lines( y,r1, col = "red", type = "b")
lines( y,r2, col = "red", type = "b")


plot(y, c((b1  +b2)/2), type = "b", col = "blue", ylab = "log N(r)", xlab = "log (1/r)", ylim = c(0,5))
lines( y,c((r1  +r2)/2), col = "red", type = "b")

#####
## grid hotspot
par(mfrow = c(1,1))
x = c(1,2,2,3,3,4,4,5,5,5,4,3,2,2,1,1,1)
y = c(1,1,0,0,1,1,2,2,3,4,4,4,4,3,3,2,1)
plot(y,x, type = "b", xlab = "", ylab = "", asp = 1, axes = FALSE, lwd = 2)
segments(1,2,1,3, lty = 2)
segments(3,2,3,5, lty = 2)
segments(2,1,2,4, lty = 2)
segments(1,2,3,2, lty = 2)
segments(1,3,4,3, lty = 2)
segments(2,4,4,4, lty = 2)


library(dplyr)

###########
## hotspot examples
fullheat = clustered_maps2$heatmap$original + theme_void()+ theme(legend.position = "none")
hotspots = ggplot(data = clustered_data2, aes(x,y)) + geom_hdr(probs = c(0.2,0.4,0.6, 0.8, 0.99)) + theme_void() + theme(legend.position = "none")
hotspot = bounded_hotspots_hdr(fullheat, minimum = 60)
hotspotplot = ggplot(data = clustered_data2, aes(x,y)) + geom_hdr(probs = c(0.001, 0.6)) + theme_void() + theme(legend.position = "none")#ggplot() + geom_sf(aes(geometry= hotspot$geometry))  + theme_void() + theme(legend.position = "none")

library(gridExtra)
grid.arrange(fullheat, hotspots, hotspotplot,  ncol = 3)

hotspot1 = bounded_hotspots_hdr(fullheat, minimum = 60)
hotspot2 = bounded_hotspots_hdr(hotspots, minimum = 60)

plot(hotspot1$geometry)



