



## libraries
library(dplyr)
library(ggplot2)
library(lidaRtRee)
library(sf)
library(purrr)
library(sdcSpatial)
library(tigers)
library(lwgeom)
library(raster)


### general functions
scale.center.poly = function(polygon){
  # calculate centroid and center on origin
  centroid = st_centroid(polygon)
  c.polygon = polygon - centroid
  
  # calculate area and rescale
  area = st_area(polygon)
  sc.polygon = c.polygon * sqrt(1/area)
  
  return(sc.polygon)
}

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
  s2o = st_nearest_feature(st_centroid(safe)$geometry, st_centroid(og)$geometry)
  matches = data.frame("Safe ID" = 1:length(s2o),
                       "Closest Og ID" = s2o)
  
  # 'func' is the utility measure function, we apply to the matches dataset by taking per row the corresponding polygons/hotspots
  results = apply(matches, 1, function(x) func(safe[x[1],], og[x[2],], ...))
  
  if(result == "avg") return(mean(results))
  else if(result == "min") return(min(results))
  else if(result == "max") return(max(results))
  else return(results)
  
}


coordinates_matrix = function(hotspot){
  st_cast(hotspot, "MULTIPOINT") %>% as("Spatial") %>% coordinates() %>% as.matrix()
}


######################### actual measures
###### Symmetric difference
symmetric_diff_function = function(sf.hs1, og.hs1){
  sc.og.hs = scale.center.poly(og.hs1$geometry)
  sc.sf.hs = scale.center.poly(sf.hs1$geometry)
  symdiff1 = st_sym_difference(sc.og.hs, sc.sf.hs)
  
  #plot(sc.sf.hs, axes = FALSE, col = "red") # the safe hotspots
  #plot(sc.og.hs, axes = FALSE, col = "blue")              # the original hotspots
  plot(symdiff1, col = "green")
  
  # the to calculate the area of the symmetric difference
  return(st_area(symdiff1))
}


###### circumference function
circ_func = function(sf.hs1, og.hs1){
  
  return(abs(st_perimeter_lwgeom(sf.hs1) - st_perimeter_lwgeom(og.hs1)))
}

###### volume function
volume_func = function(sf.hs1, og.hs1){
  
  return(abs(st_area(sf.hs1) - st_area(og.hs1)))
}

##### Frechet distance
euclidean <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}

## first we calculate the starting points
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
  x_next <- c(x[-1], x[1]);print(x_next)
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
  C = scale.center.poly(C$geometry)
  D = scale.center.poly(D$geometry)
  
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
  
  Cstart <- which(abs(C[,1] - firsths$x) < 1e-8 & abs(C[,2] - firsths$y) < 1e-8)
  Dstart <- which(abs(D[,1] - secondhs$x) < 1e-8 & abs(D[,2] - secondhs$y) < 1e-8)
  
  Corder <- rbind(C[Cstart:nrow(C), ], C[1:(Cstart - 1), ])
  Dorder <- rbind(D[Dstart:nrow(D), ], D[1:(Dstart - 1), ])
  
  Crotate <- make_clockwise(Corder)
  Drotate <- make_clockwise(Dorder)
  
  fm <- compute_frechet_matrix(Crotate, Drotate)
  return(fm[nrow(Crotate), nrow(Drotate)])
}



##### Hausdorff distance
HausdorffDistance_func <- function(A, B, directed = FALSE){
  A = scale.center.poly(A$geometry)
  B = scale.center.poly(B$geometry)
  
  A = coordinates_matrix(A)
  B = coordinates_matrix(B)
  
  s <- 1:nrow(A)
  D <- as.matrix(dist(rbind(A, B)))
  D <- D[s, -s]
  h <- max(apply(D, 1, min))
  if (directed) return(h)
  max(h, apply(D, 2, min))
}



##### Correlation dimension
corr_integral = function(x, eps){
  N = nrow(x)
  heavyside = sum((eps - dist(x)) > 0)
  
  (2/(N*(N-1))) * heavyside
  
}

corr_dimension = function(polygon, step, n){
  set.seed(1)
  sampled = st_sample(polygon, n) %>% coordinates_matrix()

  epsis = seq(2*(min(dist(sampled)) + 0.0001), median(dist(sampled)), length.out = 1/step) 
  Ce = sapply(epsis, function(eps) corr_integral(sampled, eps))
  
  return(list(
    "eps" = epsis,
    "C(e)" = Ce
  ))
}

# this is per pair of hotspots, for the average over all hotspots see the application hotspot file
correlation_dim_func = function(safe, original, step = 0.01, n = 500){
  safe = scale.center.poly(safe$geometry)
  original = scale.center.poly(original$geometry)

  ogcorr = corr_dimension(original[1,], step, n)
  sfcorr = corr_dimension(safe[1,], step, n)
  
  plot(log(ogcorr$eps), log(ogcorr$`C(e)`), type = "l", col = "blue")
  lines(log(sfcorr$eps), log(sfcorr$`C(e)`), type = "l", col = "red")
  
  ogslope = lm(log(ogcorr$`C(e)`) ~ log(ogcorr$eps))$coefficients[2]
  sfslope = lm(log(sfcorr$`C(e)`) ~ log(sfcorr$eps))$coefficients[2]
  
  return(abs(ogslope - sfslope))
  
}

##### boxcounting dimension
box_counting = function(eps, poly) {
  # Get tight bounding box and compute square
  bb = st_bbox(poly)
  bb_width = bb["xmax"] - bb["xmin"]
  bb_height = bb["ymax"] - bb["ymin"]
  bbsize = max(bb_width, bb_height) / 2
  
  xmid = (bb["xmax"] + bb["xmin"]) / 2
  ymid = (bb["ymax"] + bb["ymin"]) / 2
  
  # Square bounding box covering the polygon
  r <- raster(nrows = eps, ncol = eps, 
              xmn = xmid - bbsize, xmx = xmid + bbsize, 
              ymn = ymid - bbsize, ymx = ymid + bbsize)
  
  vals <- seq(1:length(r))
  
  r2 <- st_as_sf(rasterToPolygons(r))
  st_crs(r2) <- st_crs(poly)  # Ensure same CRS
  
  inter2 <- st_overlaps(poly, r2, byid = TRUE)
  ind <- inter2[[1]]  
  
  values(r) <- NA
  values(r)[ind] <- vals[ind]
  
  #plot(r, axes = FALSE, legend = FALSE)
  #plot(poly, add = TRUE)
  
  return(list("boxes" = length(ind),
              "size" = 2 * bbsize / eps,
              "eps" = eps))
}

box_counting_dim_func = function(safe, original, eps = 5){
  boxcount_safe = sapply(2*2^(1:eps-1), function (x) box_counting(x, safe$geometry))
  boxcount_og = sapply(2*2^(1:eps-1), function (x) box_counting(x, original$geometry))
  
  # usually log(1/size_of_box), but since size of box is seen as 1/eps (number of rows/cols), reduces to log(eps)
  #plot(log(unlist(boxcount_og[1,])), log(unlist(boxcount_og[3,])), type = "l", col = "blue")
  #lines(log(unlist(boxcount_safe[1,])), log(unlist(boxcount_safe[3,])), col = "red")
  
  
  return(list(
    "Nsafe" = log(unlist(boxcount_safe[1,])),
    "Nog" = log(unlist(boxcount_og[1,])),
    "size" = log(2*2^(1:eps-1)) # is actually the epsilon, but used here as boxsize since only shape is of interest
  ))
  
}



############### for GRID
grid_hotspot = function(input, value = 0.4, adjacent = 8) {
  rast = raster(nrow = 35, ncol = 35); extent(rast) = c(rep(c(0,1),2))
  values(rast) = input
  
  matrix = raster::as.matrix(rast)
  binary_grid <- matrix >= value*max(input) # always bigger than in case of a hotspot to decide the contour
  
  # Convert binary grid to raster object
  raster_grid <- raster(binary_grid)
  
  # Identify clusters of 1's (above value) using connected component labeling
  cluster_labels <- clump(raster_grid, directions = adjacent) # 4 is rooks case, 8 is queens
  
  # Convert clusters to polygons
  polygons <- rasterToPolygons(cluster_labels, fun=function(x){x > 0}, dissolve=TRUE)
  
  return(st_as_sf(polygons))
}

angle_measure_func = function(safe, original){
  area_safe = st_area(safe)
  area_og = st_area(original)
  
  angles_safe = number_angels(safe)
  angles_og = number_angels(original)
  
  (angles_safe * area_og) / (angles_og * area_safe)
}

number_angels = function(hotspot){
  ogco = st_coordinates(hotspot)
  changedf = data.frame(
    x = c(diff(ogco[,1]),diff(ogco[,1])[1]),
    y = c(diff(ogco[,2]),diff(ogco[,2])[1]))
  
  sum(diff(changedf$x) != 0 & diff(changedf$y) != 0)
}









