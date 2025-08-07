

source("survey data averages.R")


library("RColorBrewer")
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl)



#### Loading in the data
pointdata <- read_excel("C:/Users/1glas/Downloads/pointdata.xlsx")
pointdata$survey = survey_point
ranked.pointdata = pointdata[, c(1:2, 4:11)] 


cont.pointdata = pointdata[, -c(4:11)] # remove the ranked measures

# now there are some that
reverse_measures = c("ANN","KDE","NNE","DD")
natural_reverse_measure = c("Odi", "CP", "Ellipse","MDI")

cont.pointdata[,-(1:2)] = apply(cont.pointdata[,-(1:2)], 2, as.numeric)
cont.pointdata = cont.pointdata %>% filter(`SDC METHOD` != "original") %>% group_by(`DATA TYPE`) %>% 
  mutate(across(all_of(reverse_measures), ~ 1/., .names = "{.col}")) %>%
  mutate(across(all_of(natural_reverse_measure), ~ abs(1 - .), .names = "{.col}")) %>%
  mutate(across(where(is.numeric), ~ . / sum(.))) %>%
  group_by(`DATA TYPE`) %>% arrange(desc(survey), .by_group = TRUE)



ranked.pointdata = pointdata[, c(1:2, 4:11, 19)] 
ranked.pointdata = ranked.pointdata %>% group_by(`DATA TYPE`) %>% arrange(desc(survey), .by_group = TRUE)
ranked.pointdata$survey = rep(1:4, 5)



RBO_plot = function(mnames, data, colors = c("red", "blue"), xlab = "Index", legendpos = "none"){
  color_vals <- setNames(c(colors, "black"), c(mnames[1], mnames[2], "survey"))
  linetype_vals <- setNames(c("solid", "dashed", "dotted"), c(mnames[1], mnames[2], "survey"))
  
  data_long <- as.data.frame(data) %>%
    mutate(Index = 1:n()) %>%
    dplyr:: select(Index, all_of(mnames), survey)  %>% melt(id.vars = "Index")
  
  if(xlab == "p") {
    data_long$Index = data_long$Index/10
    legendpos = c(0.75,0.75)
  }
  
  ggplot(data_long, aes(x = Index, y = value, color = variable, linetype = variable)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = color_vals) +
    scale_linetype_manual(values = linetype_vals) +
    labs(x = xlab, y = "") +
    ylim(0,1.01) +
    theme_minimal(base_size = 15) +
    theme(
      # Remove backgrounds
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = legendpos,
      legend.justification = c(1.25, 0.75),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      
      # Add border around plot
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      
      # Increase all text sizes
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      strip.text = element_text(size = 18),  # for facet labels
      panel.grid.major  = element_blank(),  # uncomment to remove major gridlines
      panel.grid.minor = element_blank()   # uncomment to remove minor gridlines
      
    )
}

RBO_func_adj = function(list1, list2, p){
  n = length(list1)
  RBOi = rep(NA, n)
  
  for (i in 1:n) { # assuming list 1 and 2 have the same length
    nuniq = length(unique(c(list1[1:i], list2[1:i])))
    overlap = abs(nuniq - (i*2))/i
    
    RBOi[i] = (1-p)*p^(i-1)*overlap
  }
  
  RBO = sum(RBOi)/(1-p^(n)) # counts only the last outcome
  return(RBO)
}


RBO_plot_adj = function(dataname, data){
  RBO_data = data %>% filter(`DATA TYPE` == dataname) 
  RBO_adj = t(sapply(seq(0.1,0.9,0.1), function(x){
    
    apply(RBO_data[3:11], 2, function(y){
      RBO_func_adj(rank(y),rank(RBO_data[,11]), x)
    })
    
  }) )
  
  RBO_point_rank = RBO_data %>% mutate(across(where(is.numeric), ~ RBO_func(survey, ., 0.5))) 
  
  cols = viridis(n = 10, option = "D")
  
  
  measure_names = colnames(RBO_adj)
  a = RBO_plot(measure_names[1:2], RBO_point_rank, cols[c(1,5)])
  b = RBO_plot(measure_names[3:4], RBO_point_rank, cols[c(2,6)])
  c = RBO_plot(measure_names[5:6], RBO_point_rank, cols[c(3,7)])
  d = RBO_plot(measure_names[7:8], RBO_point_rank, cols[c(4,8)])
  
  e = RBO_plot(measure_names[1:2], RBO_adj, cols[c(1,5)], xlab = "p")
  f = RBO_plot(measure_names[3:4], RBO_adj, cols[c(2,6)], xlab = "p")
  g = RBO_plot(measure_names[5:6], RBO_adj, cols[c(3,7)], xlab = "p")
  h = RBO_plot(measure_names[7:8], RBO_adj, cols[c(4,8)], xlab = "p")
  
  grid.arrange(a, b, c, d, e, f, g, h, ncol = 4)
  
}


#########################################################################
##### for contuinuous variables

RBO_func = function(list1, list2, p){
  n = length(list1)
  RBOi = rep(NA, n)
  
  for (i in 1:n) { # assuming list 1 and 2 have the same length
    nuniq = length(unique(c(list1[1:i], list2[1:i])))
    overlap = abs(nuniq - (i*2))/i
    
    RBOi[i] = (1-p)*p^(i-1)*overlap
  }
  
  RBO = cumsum(RBOi)/(1-p^(n))
  return(RBO)
}


plots_cont = function(dataname, data){
  data = data[data$`DATA TYPE` == dataname,] 
  measure_names = colnames(data)[3:(ncol(data) - 1)]
  cols = viridis(n = 10, option = "D")
  
  ## DCG (rank and value)
  DCG_data = data %>% mutate(across(where(is.numeric), ~ DCG_func(., 4)))
  i = RBO_plot(measure_names[1:2], DCG_data, cols[c(1,5)])
  j = RBO_plot(measure_names[3:4], DCG_data, cols[c(2,6)])
  k = RBO_plot(measure_names[5:6], DCG_data, cols[c(3,7)])
  l = RBO_plot(measure_names[7:8], DCG_data, cols[c(4,8)])
  
  # Bar-plot of values
  e = barplot_func(measure_names[1:2], data, cols[c(1,5)])
  f = barplot_func(measure_names[3:4], data, cols[c(2,6)])
  g = barplot_func(measure_names[5:6], data, cols[c(3,7)])
  h = barplot_func(measure_names[7:8], data, cols[c(4,8)])
  
  # RBO (rank)
  RBO_data = data %>% mutate(across(where(is.numeric), ~ RBO_func(rank(survey), rank(.), 0.5)))
  
  a = RBO_plot(measure_names[1:2], RBO_data, cols[c(1,5)])
  b = RBO_plot(measure_names[3:4], RBO_data, cols[c(2,6)])
  c = RBO_plot(measure_names[5:6], RBO_data, cols[c(3,7)])
  d = RBO_plot(measure_names[7:8], RBO_data, cols[c(4,8)])
  
  return(grid.arrange(i,j,k,l, e,f,g,h,a,b,c,d, ncol = 4))
}



######
barplot_func = function(mnames, data, colors = c("red","blue")){
  color_vals <- setNames(c("black", colors), c( "survey", mnames[1], mnames[2]))
  
  data_long <- as.data.frame(data) %>%
    dplyr::select(survey,all_of(mnames),  `SDC METHOD`) %>% melt(id.vars = "SDC METHOD")
  data_long$`SDC METHOD` <- factor(data_long$`SDC METHOD`, levels = unique(data$`SDC METHOD`))

  p = ggplot(data_long, aes(fill=variable, y=value, x=`SDC METHOD`)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values = color_vals) + 
    labs(x = "SDC Method", y = "") + 
    ylim(0, 1) +
    theme_minimal(base_size = 15) +
    theme(
      # Remove backgrounds
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      
      # Add border around plot
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      
      # Increase all text sizes
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      strip.text = element_text(size = 18),  # for facet labels
      panel.grid.major  = element_blank(),  # uncomment to remove major gridlines
      panel.grid.minor = element_blank() )  # uncomment to remove minor gridlines
  
  return(p)
  

  
}

DCG_func = function(input, k){
  log2i = log((1:k + 1), base = 2)
  combined = input[1:k] / log2i
  
  return(cumsum(combined))
}



###############

cont_norm_point = plots_cont("normal",cont.pointdata)
cont_clus_point = plots_cont("clustered",cont.pointdata)
cont_rand_point = plots_cont("random",cont.pointdata)
cont_small_point = plots_cont("small clusters",cont.pointdata)
cont_consi_point = plots_cont("consistent",cont.pointdata)


rank_norm_point = RBO_plot_adj("normal",ranked.pointdata)
rank_clus_point = RBO_plot_adj("clustered",ranked.pointdata)
rank_rand_point = RBO_plot_adj("random",ranked.pointdata)
rank_small_point = RBO_plot_adj("small clusters",ranked.pointdata)
rank_cons_point = RBO_plot_adj("consistent",ranked.pointdata)

# 
# ggsave("cont_norm_point.jpg", plot = cont_norm_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_clus_point.jpg", plot = cont_clus_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_rand_point.jpg", plot = cont_rand_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_small_point.jpg", plot = cont_small_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_consi_point.jpg", plot = cont_consi_point, width = 20 , height = 8 , units = "in", dpi = 300)
# 
# ggsave("rank_norm_point.jpg", plot = rank_norm_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_clus_point.jpg", plot = rank_clus_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_rand_point.jpg", plot = rank_rand_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_small_point.jpg", plot = rank_small_point, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_cons_point.jpg", plot = rank_cons_point, width = 20 , height = 8 , units = "in", dpi = 300)































































