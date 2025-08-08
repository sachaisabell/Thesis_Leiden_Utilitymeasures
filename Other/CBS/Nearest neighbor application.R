
#source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Final R files/Survey files/final final survey maps.R")
source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Application measures/convert data.R")
source("C:/Users/1glas/OneDrive/Bureaublad/Master Statistics and Datascience/Thesis/Programming/Thesis-SDC/Final R files/Nearest neighbor distance based.R")


library(tidyr)

data_names = c("normal","clustered","random","smallclusters","consistent")


### ANN
sapply(data_names, function(x) {
  current = get(paste0(x,"_maps"))$point_data
  sapply(current, ANN_func)
})



#### KNN distances

knn_plots = lapply(data_names, knn_plot)
knn_plots[1:5]

















