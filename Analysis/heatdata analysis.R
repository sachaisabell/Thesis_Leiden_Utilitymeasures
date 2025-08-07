
source("survey data averages.R")

library("RColorBrewer")
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(gridExtra)



#### Loading in the data
heatdata <- read_excel("heatdata.xlsx")
heatdata[,-(1:2)] = apply(heatdata[,-(1:2)], 2, as.numeric)
heatdata = heatdata %>% group_by(`DATA TYPE`) %>% mutate(across(where(is.numeric), ~ abs(. - .[`SDC METHOD` == "original"]))) %>% filter(`SDC METHOD` != "original")
heatdata$survey = survey_heat

ranked.heatdata = heatdata[, c(1:2,16:19)] 
cont.heatdata = heatdata[, c(1:15, 19)] # remove the ranked measures

# now there are some that
reverse_measures = c(c("SD Max", "SD Avg", "FD Max", "FD Avg", "Vol Max", "Vol Avg", "CD Max", "CD Avg", 
                       "HDD Max", "HDD Avg", "Cir Max", "Cir Avg","Hedi"))
natural_reverse_measure = c()

cont.heatdata = cont.heatdata %>% group_by(`DATA TYPE`) %>% 
  mutate(across(all_of(reverse_measures), ~ 1/., .names = "{.col}")) %>%
  mutate(across(all_of(natural_reverse_measure), ~ abs(1 - .), .names = "{.col}")) %>%
  mutate(across(where(is.numeric), ~ . / sum(.))) %>%
  group_by(`DATA TYPE`) %>% arrange(desc(.[,"survey"]), .by_group = TRUE)



ranked.heatdata = ranked.heatdata %>% group_by(`DATA TYPE`) %>% arrange(desc(survey), .by_group = TRUE)
ranked.heatdata$survey = rep(1:4, 5) %>% as.double %>% as.numeric()



RBO_plot = function(mnames, data, colors = c("red", "blue"), xlab = "Index", legendpos = "none"){
  color_vals <- setNames(c(colors, "black"), c(mnames, "survey"))
  linetype_vals <- setNames(c( "dotted","solid","dashed"), c( "survey", mnames))
  
  data_long <- as.data.frame(data) %>%
    mutate(Index = 1:n()) %>%
    dplyr:: select(Index, all_of(c(mnames)), survey)  %>% melt(id.vars = "Index")
  
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


RBO_plot_adj_HEAT = function(dataname, data){
  RBO_data = data %>% filter(`DATA TYPE` == dataname) 
  RBO_adj = t(sapply(seq(0.1,0.9,0.1), function(x){
    
    apply(RBO_data[3:6], 2, function(y){
      RBO_func_adj(rank(y),rank(RBO_data[,"survey"]), x)
    })
    
  }) )
  
  RBO_point_rank = RBO_data %>% mutate(across(where(is.numeric), ~ RBO_func(survey, ., 0.5))) 
  print(RBO_adj)
  
  cols = viridis(n = 3, option = "D")
  
  
  measure_names = colnames(RBO_adj)
  a = RBO_plot(measure_names[1], RBO_point_rank, cols[c(1)])
  b = RBO_plot(measure_names[2], RBO_point_rank, cols[c(2)])
  c = RBO_plot(measure_names[3], RBO_point_rank, cols[c(3)])

  e = RBO_plot(measure_names[1], RBO_adj, cols[c(1)], xlab = "p")
  f = RBO_plot(measure_names[2], RBO_adj, cols[c(2)], xlab = "p")
  g = RBO_plot(measure_names[3], RBO_adj, cols[c(3)], xlab = "p")

  
  grid.arrange(a, b, c, e, f, g, ncol = 3)
  
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


plots_cont_HEAT = function(dataname, data){
  data = data[data$`DATA TYPE` == dataname,] 
  measure_names = colnames(data)[3:(ncol(data) - 1)] ; print(measure_names)
  cols = viridis(n = 14, option = "D")
  
  ## DCG (rank and value)
  DCG_data = data %>% mutate(across(where(is.numeric), ~ DCG_func(., 4)))
  i = RBO_plot(measure_names[1:2], DCG_data, cols[c(1,8)])
  j = RBO_plot(measure_names[3:4], DCG_data, cols[c(2,9)])
  k = RBO_plot(measure_names[5:6], DCG_data, cols[c(3,10)])
  l = RBO_plot(measure_names[7:8], DCG_data, cols[c(4,11)])
  m = RBO_plot(measure_names[9:10], DCG_data, cols[c(5,12)])
  n = RBO_plot(measure_names[11:12], DCG_data, cols[c(6,13)])
  o = RBO_plot(measure_names[13], DCG_data, cols[c(14)])
  
  # Bar-plot of values
  e = barplot_func(measure_names[1:2], data, cols[c(1,8)])
  f = barplot_func(measure_names[3:4], data, cols[c(2,9)])
  g = barplot_func(measure_names[5:6], data, cols[c(3,10)])
  h = barplot_func(measure_names[7:8], data, cols[c(4,11)])
  p = barplot_func(measure_names[9:10], data, cols[c(5,12)])
  q = barplot_func(measure_names[11:12], data, cols[c(6,13)])
  r = barplot_func(measure_names[13], data, cols[c(14)])
  
  # RBO (rank)
  RBO_data = data %>% mutate(across(where(is.numeric), ~ RBO_func(rank(survey), rank(.), 0.5)))
  
  a = RBO_plot(measure_names[1:2], RBO_data, cols[c(1,8)])
  b = RBO_plot(measure_names[3:4], RBO_data, cols[c(2,9)])
  c = RBO_plot(measure_names[5:6], RBO_data, cols[c(3,10)])
  d = RBO_plot(measure_names[7:8], RBO_data, cols[c(4,11)])
  s = RBO_plot(measure_names[9:10], RBO_data, cols[c(5,12)])
  t = RBO_plot(measure_names[11:12], RBO_data, cols[c(6,13)])
  u = RBO_plot(measure_names[13], RBO_data, cols[c(14)])
  
  
  return(grid.arrange(i,j,k,l,m,n,o, 
                      e,f,g,h,p,q, r,
                      a,b,c,d,s,t,u, ncol = 7))
}



######
barplot_func = function(mnames, data, colors = c("red","blue")){
  color_vals <- setNames(c("black", colors), c( "survey", mnames))
  
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


cont_norm = plots_cont_HEAT("normal",cont.heatdata)
cont_clus = plots_cont_HEAT("clustered",cont.heatdata)
cont_rand = plots_cont_HEAT("random",cont.heatdata)
cont_small = plots_cont_HEAT("small clusters",cont.heatdata)
cont_consi = plots_cont_HEAT("consistent",cont.heatdata)


rank_norm = RBO_plot_adj_HEAT("normal",ranked.heatdata)
rank_clus = RBO_plot_adj_HEAT("clustered",ranked.heatdata)
rank_rand = RBO_plot_adj_HEAT("random",ranked.heatdata)
rank_small = RBO_plot_adj_HEAT("small clusters",ranked.heatdata)
rank_cons = RBO_plot_adj_HEAT("consistent",ranked.heatdata)
# 
# 
# ggsave("cont_norm_analysis.jpg", plot = cont_norm, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_clus_analysis.jpg", plot = cont_clus, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_rand_analysis.jpg", plot = cont_rand, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_small_analysis.jpg", plot = cont_small, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("cont_consi_analysis.jpg", plot = cont_consi, width = 20 , height = 8 , units = "in", dpi = 300)
# 
# ggsave("rank_norm_analysis.jpg", plot = rank_norm, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_clus_analysis.jpg", plot = rank_clus, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_rand_analysis.jpg", plot = rank_rand, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_small_analysis.jpg", plot = rank_small, width = 20 , height = 8 , units = "in", dpi = 300)
# ggsave("rank_cons_analysis.jpg", plot = rank_cons, width = 20 , height = 8 , units = "in", dpi = 300)








