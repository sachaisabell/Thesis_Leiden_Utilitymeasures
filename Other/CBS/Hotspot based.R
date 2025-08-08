
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
bounded_hotspots_hdr = function(df, minimum){
  # create a plot using filled argument
  point.plot = ggplot(df, aes(x = x, y = y)) +
    ggdensity:: geom_hdr(probs = minimum)
  
  # extracting information from the plot
  ld.df = layer_data(point.plot)
  
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
  
  plot(sc.og.hs, axes = TRUE, col = "blue")              # the original hotspots
  plot(sc.sf.hs, axes = TRUE, add = TRUE, col = "green") # the safe hotspots
  plot(symdiff1, add = TRUE, col = "red")
  
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
first_starting_point = function(C, D, first = c(0,0)){
  
  all_points = as.data.frame(cbind(rbind(C,D), group = as.factor(c(rep("C", nrow(C)), rep("D", nrow(D)))))) # include and indicator from which polygon they are
  all_points = all_points[!duplicated(all_points[,1:2]),]
  
  # find distance to origin 
  all_points$dist2origin = sqrt((all_points[,1] - first[1])^2 + (all_points[,2] - first[2])^2)
  
  # find the points that are closest to the origin (could be multiple from the same group and from different groups)
  closest2origin = all_points %>% filter(dist2origin == min(dist2origin)) 
  
  # if we have more than one row we calculate the minimal distance of those points to the other group
  if (nrow(closest2origin) == 1) start1 = closest2origin 
  else {
    closest2origin$dist2other = apply(closest2origin, 1, function(x) {
      other_group = all_points[all_points$group != as.numeric(x[3]),] # define other group
      min(sqrt((x[1] - other_group[,1])^2 + (x[2]- other_group[,2])^2))}) # minimal distance to other group
    
    closest2other = closest2origin %>% filter(dist2other == min(dist2other)) 
    
    if (nrow(closest2other) == 1) start1 = closest2other 
    else {
      smallest.x = closest2other[which(closest2other$V1 == min(closest2other$V1)), ]
      
      if(nrow(smallest.x) == 1) start1 = smallest.x 
      else start1 = closest2other[which(smallest.x$V2 == min(smallest.x$V2)), ]
    }
  }
  
  return(start1)
  
}

second_starting_point = function(C,D,start) {
  
  all_points = as.data.frame(cbind(rbind(C,D), group = as.factor(c(rep("C", nrow(C)), rep("D", nrow(D)))))) # include and indicator from which polygon they are
  all_points = all_points[!duplicated(all_points[,1:2]),]
  relevant_points = all_points %>% filter(group != start$group)
  
  # if the first starting point also occurs in the other group this is automatically the second starting point
  if(nrow(merge(data.frame(start[,1:2]), relevant_points[,1:2]))>0) start2 = start
  else {
    relevant_points$dist2start = sqrt((relevant_points[,1] - start[1,1])^2 + (relevant_points[,2] - start[1,2])^2)
    closest2start = relevant_points %>% filter(dist2start == min(dist2start)) 
    
    if (nrow(closest2start) == 1) start2 = closest2start 
    else {
      closest2start$dist2other = apply(closest2start, 1, function(x) {
        other_group = all_points[all_points$group != as.numeric(x[3]),] # define other group
        min(sqrt((x[1] - other_group[,1])^2 + (x[2]- other_group[,2])^2))}) # minimal distance to other group
      
      closest2other = closest2start %>% filter(dist2other == min(dist2other)) 
      
      if (nrow(closest2other) == 1) start2 = closest2other 
      else {
        smallest.x = closest2other[which(closest2other$V1 == min(closest2other$V1)), ]
        
        if(nrow(smallest.x) == 1) start2 = smallest.x 
        else start2 = closest2other[which(smallest.x$V2 == min(smallest.x$V2)), ]
      }
    }    
    
  }
  
  return(start2)
}

## rotate to clockwise if necessary
is_clockwise <- function(x, y) {
  n <- length(x)
  x_next <- c(x[-1], x[1])
  y_next <- c(y[-1], y[1])
  area <- sum(x * y_next - x_next * y)
  return(area < 0)
}

make_clockwise = function(coordinates){
  # check direction and change if needed
  direction = is_clockwise(coordinates[,1], coordinates[,2])
  
  if(!direction) return(coordinates[nrow(coordinates):1, ])
  else return(coordinates)
  
}

## actual calculating of the frechet distance
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

frechet_distance_func = function(C,D){
  C = coordinates_matrix(C)
  D = coordinates_matrix(D)
  
  # remove duplicates
  C = C[!duplicated(C),]
  D = D[!duplicated(D),]
  
  # calculate starting points and reorder coordinates
  firsths = first_starting_point(C,D) #;  print(paste("here", firsths))
  secondhs = second_starting_point(C,D,firsths)
  
  Cstart = which(C[,1] == firsths[,1] & C[,2] == firsths[,2])
  Dstart = which(D[,1] == secondhs[,1] & D[,2] == secondhs[,2])
  
  Corder = rbind(C[Cstart:nrow(C),], C[1:Cstart,])
  Dorder = rbind(D[Dstart:nrow(D),], D[1:Dstart,])
  
  # rotate to clockwise
  Crotate = make_clockwise(Corder)
  Drotate = make_clockwise(Dorder)
  
  fm = compute_frechet_matrix(Crotate, Drotate)
  frechet_distance <- fm[nrow(Crotate), nrow(Drotate)]
  
  return(frechet_distance)
}


##### Correlation dimension
corr_integral = function(x, eps){
  N = nrow(x)
  heavyside = sum((eps - dist(x)) > 0)
  
  (1/(N*(N-1))) * heavyside
  
}

corr_dimension = function(polygon, step, n){
  
  sampled = st_sample(polygon, n) %>% coordinates_matrix()
  
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
  
  
  plot(log(ogcorr$eps), log(ogcorr$`C(e)`), type = "l", col = "blue")
  lines(log(sfcorr$eps), log(sfcorr$`C(e)`), type = "l", col = "red")
  
  
}



### Examples
# first creating the data and the hotspots
data_og = dwellings[,1:2]
data_safe = mask_random(data_og, r = 100)

hotspot_og = bounded_hotspots_hdr(data_og, minimum = 0.35)
hotspot_safe = bounded_hotspots_hdr(data_safe, minimum = 0.35)

plot(hotspot_safe$geometry, axes = TRUE, col = "blue")              # the original hotspots
plot(hotspot_og$geometry, axes = TRUE, add = TRUE, col = "green") # the safe hotspots

# boxcount
boxcount1 = output_measure(box_counting_dim_func, hotspot_safe, hotspot_og, "", eps = 5) 
epsis = 2*2^(1:5-1) # 5 from the eps argument
y.safe = (boxcount1[[1]]$safe + boxcount1[[2]]$safe)/max(hotspot_og$clust)
y.og = (boxcount1[[1]]$original + boxcount1[[2]]$original)/max(hotspot_og$clust)
plot(epsis, y.safe, col = "blue", type = "l"); lines(epsis, y.og, col = "red")

# symmetric difference
output_measure(symmetric_diff_function, hotspot_safe,hotspot_og, "")

# hausdorff distance
output_measure(HausdorffDistance_func, hotspot_safe, hotspot_og, "")

# frechet distance
output_measure(frechet_distance_func, hotspot_og, hotspot_safe, "")

# correlation dimension
output_measure(correlation_dim_func, hotspot_safe, hotspot_og, "")



######## specifically for GRIDS: angle measure
map_original = sdc_raster(dwellings[,1:2], dwellings$consumption)
map_safe = protect_smooth(map_original)

dat_original = raster::as.matrix(map_original$value$mean); dat_original[is.na(dat_original)] = 0
dat_safe = raster::as.matrix(map_safe$value$mean); dat_safe[is.na(dat_safe)] = 0

gridsf = grid_hotspot(dat_safe, 5000) %>% st_as_sf()
gridog = grid_hotspot(dat_original, 5000) %>% st_as_sf()

plot(gridsf$geometry, axes = TRUE, col = "blue")              # the safe hotspots
plot(gridog$geometry, axes = TRUE, add = TRUE, col = "green") # the og hotspots

s2o = st_nearest_feature(gridsf$geometry, gridog$geometry)

matches = data.frame("Safe ID" = 1:length(s2o),
                     "Closest Og ID" = s2o)


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


output_measure(angle_measure_func, gridsf, gridog, "")
































