



par(mfrow = c(1,1))


DCG_func = function(input, k = length(input)){
  log2i = log((1:k + 1), base = 2)
  combined = input[1:k] / log2i
  
  return(cumsum(combined))
}

RBO_func2 = function(list1, list2, p){
  n = length(list1)
  RBOi = rep(NA, n)
  
  for (i in 1:n) { # assuming list 1 and 2 have the same length
    nuniq = length(unique(c(list1[1:i], list2[1:i])))
    overlap = abs(nuniq - (i*2))/i
    
    RBOi[i] = p^(i-1)*overlap
  }
  
  RBO = cumsum(RBOi)*(1-p)
  return(RBO)
}

D1 = DCG_func(c(0.6,0.1,0.2,0.1))
D2 = DCG_func(c(0.4,0.3,0.2,0.1))
D3 = DCG_func(rep(0.25,4))

plot(1:4, D1/D2, axes = TRUE, ylim = c(-2,2))
lines(1:4, D2)


RBO_func2(c(1,2,3,4), c(1,2,3,4), 0.25)




d = 1:4
sum(log(d+1, base = 2))



0.6^(1:4-1)


s1/1.58


plot(scale.center.poly(normal_hotspots[[1]]$geometry), axes = TRUE)

A = normal_hotspots[[1]]
B = normal_hotspots[[2]]
A = scale.center.poly(A$geometry)
B = scale.center.poly(B$geometry)

A = coordinates_matrix(A)
B = coordinates_matrix(B)

s <- 1:nrow(B)
D <- as.matrix(dist(rbind(A, B)))
D <- D[s, -s]
h <- max(apply(D, 1, min))
if (directed) return(h)
max(h, apply(D, 2, min))



dim(D)
# Load libraries
library(sf)
library(geosphere)

# ---- Generate irregular shape function ----
generate_blob <- function(center = c(0, 0), 
                          base_radius = 1, 
                          irregularity = 0.3, 
                          n_points = 100, 
                          seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  angles <- seq(0, 2 * pi, length.out = n_points)
  perturb <- runif(n_points, 1 - irregularity, 1 + irregularity)
  radius <- base_radius * perturb
  coords <- cbind(center[1] + radius * cos(angles),
                  center[2] + radius * sin(angles))
  coords <- rbind(coords, coords[1, ])  # close the polygon
  st_polygon(list(coords))
}

# ---- Create two irregular polygons (inner and outer) ----
inner_poly <- generate_blob(center = c(0, 0), base_radius = 1, irregularity = 0.2, seed = 42)
outer_poly <- generate_blob(center = c(0, 0), base_radius = 1.3, irregularity = 0.25, seed = 99)

sf_inner <- st_sfc(inner_poly)
sf_outer <- st_sfc(outer_poly)

sf_inner = normal_hotspots$original[1]$geometry
sf_outer = normal_hotspots$random[1]$geometry

# ---- Sample boundary points ----
pts_inner <- st_line_sample(st_cast(sf_inner, "LINESTRING"), density = 0.25)
pts_outer <- st_line_sample(st_cast(sf_outer, "LINESTRING"), density = 0.25)

coords_inner <- coordinates_matrix(sf_inner)
coords_outer <- coordinates_matrix(sf_outer)

# ---- Compute pairwise distance matrix ----
dmat <- geosphere::distm(coords_inner[,1:2], coords_outer[,1:2])

# hd(X,Y): max of min distances from inner to outer
d1 <- apply(dmat, 1, min)
i1 <- which.max(d1)
j1 <- which.min(dmat[i1, ])
pt1_from <- coords_inner[i1, ]
pt1_to   <- coords_outer[j1, ]

# hd(Y,X): max of min distances from outer to inner
d2 <- apply(dmat, 2, min)
j2 <- which.max(d2)
i2 <- which.min(dmat[, j2])
pt2_from <- coords_outer[j2, ]
pt2_to   <- coords_inner[i2, ]




allcoord = rbind(coords_inner, coords_outer)
plot(sf_inner)
# ---- Plot ----
plot(sf_inner, lwd = 2, border = "red", xlim = c(min(allcoord[,1]), max(allcoord[,1])), main = "Directed Hausdorff Distances")
plot(st_geometry(sf_outer), col = NA, lty = "dashed", lwd = 2, border = "blue", add = TRUE)

# Draw sample points
points(coords_inner, pch = 19, col = "red", cex = 0.4)
points(coords_outer, pch = 19, col = "blue", cex = 0.4)

# Arrows and annotations
arrows(pt1_from[1], pt1_from[2], pt1_to[1], pt1_to[2], 
       col = "black", lwd = 2, length = 0.1)
text(mean(c(pt1_from[1], pt1_to[1])), mean(c(pt1_from[2], pt1_to[2])) + 0.1,
     labels = "hd(X,Y)", col = "black", cex = 0.9)

arrows(pt2_from[1], pt2_from[2], pt2_to[1], pt2_to[2], 
       col = "black", lwd = 2, length = 0.1)
text(mean(c(pt2_from[1], pt2_to[1])), mean(c(pt2_from[2], pt2_to[2])) + 0.1,
     labels = "hd(Y,X)", col = "black", cex = 0.9)







