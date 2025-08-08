


install.packages("sdcSpatial")


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

polies3 = bounded_hotspots_hdr(df.original, 0.5)
plot(polies3, axes = TRUE)









###################
pplot = ggplot(df.original, aes(x = x, y = y)) +
  geom_density_2d_filled(contour = TRUE, breaks = c(0.025, 1))

pplot = ggplot(df.original, aes(x = x, y = y)) +
  ggdensity::geom_hdr(probs = .80)



ld = layer_data(pplot)

unique(ld$subgroup)


ld$pol <- paste0(ld$subgroup, "_", ld$group)
idss <- unique(ld$pol)

x = idss[1]

pols <- lapply(idss, function(x){
  topol <- ld[ld$pol == x, ]
  closepol <- rbind(topol, topol[1, ])
  pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
  df <- unique(topol[, grepl("group", names(topol))])
  tofeatures <- st_as_sf(st_sfc(pol))
  return(tofeatures)
})

final_pols <- do.call(rbind, pols)

# gets no overlapping, separate polygons
parts <- st_cast(st_union(final_pols),"POLYGON")
clust <- unlist(st_intersects(final_pols, parts))

diss <- cbind(final_pols, clust) %>%
  group_by(clust) %>%
  summarize()


plot(diss, axes = TRUE)








bounded_hotspots(df.safe, 0.05)

class(pol)

tofeatures$x

plot(1:5, 1:5)
plot(pols[1], add = TRUE)

