
source("hotspot_functions.R")

## libraries
library(dplyr)
library(ggplot2)
library(lidaRtRee)
library(sf)
library(purrr)
library(sdcSpatial)
library(tigers)
library(raster)


### general functions
bounded_hotspots_hdr = function(plot, minimum){
  
  # extracting information from the plot
  ld.df = layer_data(plot) 
  ld.df = ld.df[ld.df$probs >= paste0(minimum, "%"),]
  
  if(nrow(ld.df) == 0) stop("No hotspots due to minimum value")
  
  # separating the different polygons based on subgroup
  ld.df$pol <- paste0(ld.df$subgroup, "_", ld.df$group)
  ids <- unique(ld.df$pol)
  
  # Split contours based on the id
  pols <- lapply(ids, function(x){
    topol <- ld.df[ld.df$pol == x, ]
    closepol <- rbind(topol, topol[1, ])
    pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
    df <- unique(topol[, grepl("group", names(topol))])
    tofeatures <- st_as_sf(df, geometry=st_sfc(pol))
    return(tofeatures)
  })
  
  final_pols <- do.call(rbind, pols)
  
  # gets no overlapping, separate polygons
  parts <- st_cast(st_union(final_pols),"POLYGON")
  clust <- unlist(st_intersects(final_pols, parts))
  
  diss <- cbind(final_pols, clust) %>%
    group_by(clust) %>%
    summarize()
  
  return(diss)
}

output_measure = function(func, safe, og, result = "avg", ...){
  s2o = st_nearest_feature(safe$geometry, og$geometry)
  matches = data.frame("Safe ID" = 1:length(s2o),
                       "Closest Og ID" = s2o)
  
  # 'func' is the utility measure function, we apply to the matches dataset by taking per row the correspoding polygons/hotspots
  results = apply(matches, 1, function(x) func(safe[x[1],], og[x[2],], ...))
  
  if(result == "avg") return(mean(results))
  else if(result == "min") return(min(results))
  else if(result == "max") return(max(results))
  else return(results)
  
}


grid_hotspot = function(input, value, adjacent = 8) {
  rast = raster(nrow = 35, ncol = 35); extent(rast) = c(rep(c(0,1),2))
  values(rast) = input
  
  matrix = raster::as.matrix(rast)
  binary_grid <- matrix > value # always bigger than in case of a hotspot to decide the contour
  
  
  # Convert binary grid to raster object
  raster_grid <- raster(binary_grid)
  
  # Identify clusters of 1's (above value) using connected component labeling
  cluster_labels <- clump(raster_grid, directions = adjacent) # 4 is rooks case, 8 is queens
  
  # Convert clusters to polygons
  polygons <- rasterToPolygons(cluster_labels, fun=function(x){x > 0}, dissolve=TRUE)
  
  return(st_as_sf(polygons))
}


coordinates_matrix = function(hotspot){
  st_cast(hotspot, "MULTIPOINT") %>% as("Spatial") %>% coordinates() %>% as.matrix()
}


####### Box counting dimension
box_counting = function(eps, poly) {
  
  poly = scale.center.poly(poly)
  bb33 = st_bbox(poly)
  bbsize = max(abs(bb33 - 0))
  r <- raster(nrows=1*eps, ncol=1*eps, xmn=-bbsize, ymn=-bbsize, xmx = bbsize, ymx = bbsize)
  vals <- seq(1:length(r))
  
  r2 = st_as_sf(rasterToPolygons(r))
  
  inter2 = st_overlaps(poly, sf::st_set_crs(r2, st_crs(poly)), byid = TRUE)
  ind = inter2[[1]]
  
  
  values(r) = NA
  values(r)[ind] = vals[ind]
  
  plot(r)
  plot(poly, add = TRUE)
  
  return(list("boxes" = length(ind),
              "size" = 2*bbsize / eps,
              "eps" = eps))
  
}

box_counting_dim_func = function(safe, original, eps = 10){
  boxcount_safe = sapply(2*2^(1:eps-1), function (x) box_counting(x, safe$geometry))
  boxcount_og = sapply(2*2^(1:eps-1), function (x) box_counting(x, original$geometry))
  
  bcdim_safe = log(unlist(boxcount_safe[1,])) / log(1/unlist(boxcount_safe[3,]))
  bcdim_og = log(unlist(boxcount_og[1,])) / log(1/unlist(boxcount_og[3,]))
  
  plot(unlist(boxcount_safe[3,]), bcdim_safe, type = "l", col = "blue")
  lines(unlist(boxcount_og[3,]), bcdim_og, col = "red")
  
  return(list("safe" = bcdim_safe,
              "original" = bcdim_og))
}


##### Symmetric difference
symmetric_diff_function = function(sf.hs1, og.hs1){
  sc.og.hs = scale.center.poly(og.hs1$geometry)
  sc.sf.hs = scale.center.poly(sf.hs1$geometry)
  symdiff1 = st_sym_difference(sc.og.hs, sc.sf.hs)
  
  plot(sc.sf.hs, axes = FALSE, col = "red") # the safe hotspots
  plot(sc.og.hs, axes = FALSE, col = "blue")              # the original hotspots
  plot(symdiff1, col = "green")
  
  # the to calculate the area of the symmetric difference
  return(st_area(symdiff1))
}


##### Hausdorff distance
HausdorffDistance_func <- function(A, B, directed = FALSE)
{
  A = coordinates_matrix(A)
  B = coordinates_matrix(B)
  
  s <- 1:nrow(A)
  D <- as.matrix(dist(rbind(A, B)))
  D <- D[s, -s]
  h <- max(apply(D, 1, min))
  if (directed) return(h)
  max(h, apply(D, 2, min))
}


##### Frechet distance
## first we calculate the starting points
# Helper: Compute Euclidean distance
euclidean <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}


# Find first starting point: closest to origin, then to other group
first_starting_point <- function(C, D, first = c(0, 0)) {
  all_points <- data.frame(
    x = c(C[,1], D[,1]),
    y = c(C[,2], D[,2]),
    group = factor(c(rep("C", nrow(C)), rep("D", nrow(D))))
  )
  all_points <- all_points[!duplicated(all_points[, 1:2]), ]
  
  all_points$dist2origin <- sqrt((all_points$x - first[1])^2 + (all_points$y - first[2])^2)
  closest2origin <- all_points[all_points$dist2origin == min(all_points$dist2origin), ]
  
  if (nrow(closest2origin) == 1) return(closest2origin)
  
  closest2origin$dist2other <- apply(closest2origin, 1, function(x) {
    other_group <- all_points[all_points$group != x["group"], ]
    min(sqrt((as.numeric(x["x"]) - other_group$x)^2 + (as.numeric(x["y"]) - other_group$y)^2))
  })
  
  closest2other <- closest2origin[closest2origin$dist2other == min(closest2origin$dist2other), ]
  if (nrow(closest2other) == 1) return(closest2other)
  
  smallest_x <- closest2other[which(closest2other$x == min(closest2other$x)), ]
  if (nrow(smallest_x) == 1) return(smallest_x)
  
  return(smallest_x[which(smallest_x$y == min(smallest_x$y)), ])
}

# Find second starting point based on first
second_starting_point <- function(C, D, start) {
  all_points <- data.frame(
    x = c(C[,1], D[,1]),
    y = c(C[,2], D[,2]),
    group = factor(c(rep("C", nrow(C)), rep("D", nrow(D))))
  )
  all_points <- all_points[!duplicated(all_points[, 1:2]), ]
  relevant_points <- all_points[all_points$group == "D", ]
  
  if (nrow(merge(start[, 1:2], relevant_points[, 1:2])) > 0) return(start)
  
  relevant_points$dist2start <- sqrt((relevant_points$x - start$x)^2 + (relevant_points$y - start$y)^2)
  closest2start <- relevant_points[relevant_points$dist2start == min(relevant_points$dist2start), ]
  
  if (nrow(closest2start) == 1) return(closest2start)
  
  closest2start$dist2other <- apply(closest2start, 1, function(x) {
    other_group <- all_points[all_points$group != x["group"], ]
    min(sqrt((as.numeric(x["x"]) - other_group$x)^2 + (as.numeric(x["y"]) - other_group$y)^2))
  })
  
  closest2other <- closest2start[closest2start$dist2other == min(closest2start$dist2other), ]
  if (nrow(closest2other) == 1) return(closest2other)
  
  smallest_x <- closest2other[which(closest2other$x == min(closest2other$x)), ]
  if (nrow(smallest_x) == 1) return(smallest_x)
  
  return(smallest_x[which(smallest_x$y == min(smallest_x$y)), ])
}

# Check and make polygon clockwise
is_clockwise <- function(x, y) {
  x_next <- c(x[-1], x[1])
  y_next <- c(y[-1], y[1])
  area <- sum(x * y_next - x_next * y)
  return(area < 0)
}

make_clockwise <- function(coordinates) {
  if (!is_clockwise(coordinates[,1], coordinates[,2])) {
    return(coordinates[nrow(coordinates):1, ])
  }
  return(coordinates)
}

# Compute discrete Frechet distance matrix
compute_frechet_matrix <- function(P, Q) {
  m <- nrow(P)
  n <- nrow(Q)
  D <- matrix(0, nrow = m, ncol = n)
  
  for (i in 1:m) {
    for (j in 1:n) {
      d <- euclidean(P[i, ], Q[j, ])
      if (i == 1 && j == 1) {
        D[i, j] <- d
      } else if (i == 1) {
        D[i, j] <- max(D[i, j - 1], d)
      } else if (j == 1) {
        D[i, j] <- max(D[i - 1, j], d)
      } else {
        D[i, j] <- max(min(D[i - 1, j], D[i - 1, j - 1], D[i, j - 1]), d)
      }
    }
  }
  return(D)
}

# Main function to compute Fréchet distance
frechet_distance_func <- function(C, D) {
  C <- coordinates_matrix(C)
  D <- coordinates_matrix(D)

  C <- C[!duplicated(C), ]
  D <- D[!duplicated(D), ]
  
  firsths <- first_starting_point(C, D)

  if(firsths$group == "D"){
    print("changed C to D")
    C.original = C
    C = D
    D = C.original
  }
  secondhs <- second_starting_point(C, D, firsths)
  
  # Match coordinates using a tolerance
  Cstart <- which(abs(C[,1] - firsths$x) < 1e-8 & abs(C[,2] - firsths$y) < 1e-8)
  Dstart <- which(abs(D[,1] - secondhs$x) < 1e-8 & abs(D[,2] - secondhs$y) < 1e-8)
  
  Corder <- rbind(C[Cstart:nrow(C), ], C[1:(Cstart - 1), ])
  Dorder <- rbind(D[Dstart:nrow(D), ], D[1:(Dstart - 1), ])
  
  Crotate <- make_clockwise(Corder)
  Drotate <- make_clockwise(Dorder)
  
  fm <- compute_frechet_matrix(Crotate, Drotate)
  return(fm[nrow(Crotate), nrow(Drotate)])
}

##### Correlation dimension
corr_integral = function(x, eps){
  N = nrow(x)
  heavyside = sum((eps - dist(x)) > 0)
  
  (1/(N*(N-1))) * heavyside
  
}

corr_dimension = function(polygon, step, n){
  
  sampled = st_sample(polygon, n) %>% coordinates_matrix()
  print(max(dist(sampled)))
  
  epsis = seq(0,max(dist(sampled)), step)
  Ce = sapply(epsis, function(eps) corr_integral(sampled, eps))
  
  return(list(
    "eps" = epsis,
    "C(e)" = Ce
  ))
}

correlation_dim_func = function(safe, original, step = 0.1, n = 10){
  
  ogcorr = corr_dimension(safe[1,], step, n)
  sfcorr = corr_dimension(safe[1,], step, n)
  print(ogcorr)
  
  
  plot(log(ogcorr$eps), log(ogcorr$`C(e)`), type = "l", col = "blue")
  lines(log(sfcorr$eps), log(sfcorr$`C(e)`), type = "l", col = "red")
  
  
}

#### volume function
volume_func = function(sf.hs1, og.hs1){
  
  return(abs(st_area(sf.hs1) - st_area(og.hs1)))
}

### circumference function
circ_func = function(sf.hs1, og.hs1){
  
  return(abs(st_perimeter_lwgeom(sf.hs1) - st_perimeter_lwgeom(og.hs1)))
}



### Examples
# first creating the data and the hotspots
hotspot_og = bounded_hotspots_hdr2(normal_maps$heatmap$original, 10)
hotspot_safe = bounded_hotspots_hdr2(normal_maps$heatmap$weighted, 10)

# symmetric difference: WORKS
output_measure(symmetric_diff_function, hotspot_safe,hotspot_og, "")

# hausdorff distance: WORKS
output_measure(HausdorffDistance_func, hotspot_safe, hotspot_og, "")

# frechet distance: WORKS
output_measure(frechet_distance_func, hotspot_safe, hotspot_og, "")

# correlation dimension: WORKS
output_measure(correlation_dim_func, hotspot_safe, hotspot_og, "", step = 0.001)

# boxcount: WORKS
boxcount1 = output_measure(box_counting_dim_func, hotspot_safe, hotspot_og, "", eps = 5) 
epsis = 2*2^(1:5-1) # 5 from the eps argument
y.safe = (boxcount1[[1]]$safe + boxcount1[[2]]$safe)/max(hotspot_og$clust)
y.og = (boxcount1[[1]]$original + boxcount1[[2]]$original)/max(hotspot_og$clust)
plot(epsis, y.safe, col = "blue", type = "l"); lines(epsis, y.og, col = "red")



######## specifically for GRIDS: angle measure
number_angels = function(hotspot){
  ogco = st_coordinates(hotspot)
  changedf = data.frame(
    x = c(diff(ogco[,1]),diff(ogco[,1])[1]),
    y = c(diff(ogco[,2]),diff(ogco[,2])[1]))
  
  sum(diff(changedf$x) != 0 & diff(changedf$y) != 0)
}

angle_measure_func = function(safe, original){
  area_safe = st_area(safe)
  area_og = st_area(original)
  
  angles_safe = number_angels(safe)
  angles_og = number_angels(original)
  
  (angles_safe * area_og) / (angles_og * area_safe)
}

### examples
grid_original = grid_hotspot(normal_maps$raster_data$original$count, 4)
grid_safe = grid_hotspot(normal_maps$raster_data$random$count,4)

output_measure(angle_measure_func, grid_original, grid_safe)


































