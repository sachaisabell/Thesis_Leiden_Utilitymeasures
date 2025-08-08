set.seed(42)

# Inputs
maptypes <- c("grid", "heat", "point")
datatypes <- c("normal", "clustered", "random", "small", "consistent")
sdc_techniques <- c("random", "grid", "voronoi", "weighted")
group_names <- paste0("group_", 1:15)
n_per_group <- 20

# Generate comparisons
map_data_combos <- expand.grid(maptype = maptypes, datatype = datatypes)
sdc_pairs <- t(combn(sdc_techniques, 2))
comparisons <- do.call(rbind, lapply(1:nrow(map_data_combos), function(i) {
  combo <- map_data_combos[i, ]
  cbind(
    maptype = combo$maptype,
    datatype = combo$datatype,
    sdc1 = sdc_pairs[, 1],
    sdc2 = sdc_pairs[, 2]
  )
}))
comparisons <- as.data.frame(comparisons)
comparisons$id <- paste(comparisons$maptype, comparisons$datatype, comparisons$sdc1, comparisons$sdc2, sep = "_")

# Initialize tracking
group_assignments <- setNames(vector("list", length(group_names)), group_names)
comparison_counts <- setNames(rep(0, length(comparisons$id)), comparisons$id)

# Helper to check if a comparison already exists in group
can_assign <- function(group, cmp_id) {
  !(cmp_id %in% group_assignments[[group]])
}

# Assign each comparison to 3 different groups
all_comparisons <- comparisons$id
assignment_log <- data.frame()

for (cmp in sample(all_comparisons)) {
  assigned <- c()
  
  # Try to find 3 different groups that can take it
  possible_groups <- sample(group_names)
  for (g in possible_groups) {
    if (length(group_assignments[[g]]) < n_per_group && can_assign(g, cmp)) {
      group_assignments[[g]] <- c(group_assignments[[g]], cmp)
      assignment_log <- rbind(assignment_log, data.frame(group = g, id = cmp))
      assigned <- c(assigned, g)
      if (length(assigned) == 3) break
    }
  }
  
  # Sanity check
  if (length(assigned) < 3) {
    stop(paste("Couldn't assign comparison", cmp, "to 3 unique groups"))
  }
}

# Final merge with metadata
final_table <- merge(assignment_log, comparisons, by = "id", all.x = TRUE)
final_table$question_text <- paste0(
  "Given a ", final_table$maptype, " map with ", final_table$datatype,
  " data, which SDC technique performs better?\nA: ", final_table$sdc1,
  "\nB: ", final_table$sdc2
)

nrow(final_table)
sum((table(final_table[,1:2])) > 1) # no bigger than 1, so thats good

rowSums(table(final_table[,1:2]))


# Validation
stopifnot(all(table(final_table$group) == 20))        # each group has 20
stopifnot(all(table(final_table$id) == 3))            # each comparison used 3x
stopifnot(!any(duplicated(final_table[c("group", "id")])))  # no dup in group

# Save
write.csv(final_table, "limesurvey_unique_balanced_assignments.csv", row.names = FALSE)

###################################
#### with area maps in there as well



set.seed(42)

# Original inputs
maptypes <- c("grid", "heat", "point")
datatypes <- c("normal", "clustered", "random", "small", "consistent")
sdc_techniques <- c("random", "grid", "voronoi", "weighted")

# Area-specific inputs
area_maptype <- "area"
area_datatypes <- c("NL1", "NL2", "NL3")
area_sdcs <- c("small", "medium", "large")

# Groups
group_names <- paste0("group_", 1:16)
group_assignments <- setNames(vector("list", length(group_names)), group_names)

# === Generate all comparisons ===

# 1. Standard comparisons
map_data_combos <- expand.grid(maptype = maptypes, datatype = datatypes)
sdc_pairs <- t(combn(sdc_techniques, 2))
comparisons1 <- do.call(rbind, lapply(1:nrow(map_data_combos), function(i) {
  combo <- map_data_combos[i, ]
  cbind(
    maptype = combo$maptype,
    datatype = combo$datatype,
    sdc1 = sdc_pairs[, 1],
    sdc2 = sdc_pairs[, 2]
  )
}))
comparisons1 <- as.data.frame(comparisons1)

# 2. Area comparisons
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

# Combine and ID
comparisons <- rbind(comparisons1, comparisons2)
comparisons$id <- paste(comparisons$maptype, comparisons$datatype, comparisons$sdc1, comparisons$sdc2, sep = "_")

# === Assignments ===

# Init tracking
can_assign <- function(group, cmp_id) {
  !(cmp_id %in% group_assignments[[group]])
}

assignment_log <- data.frame()
for (cmp in sample(comparisons$id)) {
  assigned <- c()
  possible_groups <- sample(group_names)
  for (g in possible_groups) {
    if (length(group_assignments[[g]]) < 20 && can_assign(g, cmp)) {
      group_assignments[[g]] <- c(group_assignments[[g]], cmp)
      assignment_log <- rbind(assignment_log, data.frame(group = g, id = cmp))
      assigned <- c(assigned, g)
      if (length(assigned) == 3) break
    }
  }
  if (length(assigned) < 3) {
    stop(paste("Could not assign", cmp, "to 3 unique groups"))
  }
}

# Merge with metadata
final_table <- merge(assignment_log, comparisons, by = "id", all.x = TRUE)

check.table = table(final_table[,1:2])
sum(rowSums(check.table) < 3) # should be 0
(colSums(check.table)) # should be around 20

# Validation
cat("Group size distribution:\n")
print(table(final_table$group))  # ~19 or 20 per group
cat("Each comparison should appear 3 times:\n")
print(table(final_table$id))     # Should be 3
stopifnot(!any(duplicated(final_table[c("group", "id")])))  # No dup within group

# Save
write.csv(final_table, "assignments_with_area_3sdc.csv", row.names = FALSE)

library("openxlsx")


write.xlsx(as.data.frame(final_table), "assignments_with_area_3sdc.xlsx")



