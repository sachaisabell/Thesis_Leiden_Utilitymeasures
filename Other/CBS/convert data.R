
source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Final R files/Survey files/final final survey maps.R")

###### point data to points
# Get the names of the point plots
point_names <- names(normal_maps$point)
data_names = c("normal","clustered","smallclusters","random","consistent")

# Create the list with names and include the point name as a column in each data frame
normal_points <- setNames(
  lapply(point_names, function(nm) {
    layer_data(normal_maps$point[[nm]])[, 1:2]}),
  point_names
)

clustered_points <- setNames(
  lapply(point_names, function(nm) {
    layer_data(clustered_maps$point[[nm]])[, 1:2]}),
  point_names
)

smallclusters_points <- setNames(
  lapply(point_names, function(nm) {
    layer_data(smallclusters_maps$point[[nm]])[, 1:2]}),
  point_names
)

random_points <- setNames(
  lapply(point_names, function(nm) {
    layer_data(random_maps$point[[nm]])[, 1:2]}),
  point_names
)

consistent_points <- setNames(
  lapply(point_names, function(nm) {
    layer_data(consistent_maps$point[[nm]])[, 1:2]}),
  point_names
)
















