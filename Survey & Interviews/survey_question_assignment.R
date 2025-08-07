


set.seed(3)

# heat grid point specifics
maptypes <- c("grid", "heat", "point")
datatypes <- c("normal", "clustered", "random", "small", "consistent")
sdc_techniques <- c("random", "grid", "voronoi", "weighted")

# area-specific inputs
area_maptype <- "area"
area_datatypes <- c("NL1", "NL2", "NL3")
area_sdcs <- c("small", "medium", "large")

# make 16 groups
group_names <- paste0("group_", 1:16)
group_assignments <- setNames(vector("list", length(group_names)), group_names)

#### defined all possible comparisons
# for grid,point,heat
map_data_combos <- expand.grid(maptype = maptypes, datatype = datatypes)
sdc_pairs <- t(combn(sdc_techniques, 2))
comparisons1 <- do.call(rbind, lapply(1:nrow(map_data_combos), function(i) {
  combo <- map_data_combos[i, ];print(combo)
  cbind(
    maptype = combo[1],#combo$maptype,
    datatype = combo[2],#combo$datatype,
    sdc1 = sdc_pairs[, 1],
    sdc2 = sdc_pairs[, 2]
  )
}))
comparisons1 <- as.data.frame(comparisons1)

# for areal maps
area_combos <- expand.grid(datatype = area_datatypes)
area_sdc_pairs <- t(combn(area_sdcs, 2))
comparisons2 <- do.call(rbind, lapply(1:nrow(area_combos), function(i) {
  combo <- area_combos[i, ];print(combo)
  cbind(
    maptype = area_maptype,
    datatype = combo,
    sdc1 = area_sdc_pairs[, 1],
    sdc2 = area_sdc_pairs[, 2]
  )
}))
comparisons2 <- as.data.frame(comparisons2)

# combine all comparisons
comparisons <- rbind(comparisons1, comparisons2)
comparisons$id <- paste(comparisons$maptype, comparisons$datatype, comparisons$sdc1, comparisons$sdc2, sep = "_")

##### now assign comparisons to various groups
# to make sure no group gets the same comparison multiple times
can_assign <- function(group, cmp_id) {
  !(cmp_id %in% group_assignments[[group]])
}

assignment_log <- data.frame()
for (cmp in sample(comparisons$id)) {
  assigned <- c()
  possible_groups <- sample(group_names)
  for (g in possible_groups) {
    if (length(group_assignments[[g]]) < 20 && can_assign(g, cmp)) { # no more than 20 comparisons per group
      group_assignments[[g]] <- c(group_assignments[[g]], cmp)
      assignment_log <- rbind(assignment_log, data.frame(group = g, id = cmp))
      assigned <- c(assigned, g)
      if (length(assigned) == 3) break # all comparisons assigned to 3 groups
    }
  }
  if (length(assigned) < 3) {
    stop(paste("Could not assign", cmp, "to 3 unique groups")) # if not possible, different seed or +1 number of groups
  }
}

# create table of all assinged comparisons
final_table <- merge(assignment_log, comparisons, by = "id", all.x = TRUE) ; head(final_table)

check.table = table(final_table[,1:2])
sum(rowSums(check.table) < 3) # should be 0
sum(check.table > 1) # should be 0
(colSums(check.table)) # should be around 20


heat.table = final_table[final_table$maptype == "heat",c("datatype","group")]
table(heat.table); colSums(table(heat.table))
plot(density(colSums(table(heat.table))))

grid.table = final_table[final_table$maptype == "grid",c("datatype","group")]
table(grid.table); colSums(table(grid.table))
plot(density(colSums(table(grid.table))))

point.table = final_table[final_table$maptype == "point",c("datatype","group")]
table(point.table); colSums(table(point.table))
plot(density(colSums(table(point.table))))

area.table = final_table[final_table$maptype == "area",c("datatype","group")]
table(area.table); colSums(table(area.table))
plot(density(colSums(table(area.table))))

library("openxlsx")
write.xlsx(as.data.frame(final_table), "Question_groups.xlsx")













