

data1 = data.frame(x = runif(500), y = runif(500))

plot1 = ggplot() +
  ggdensity::geom_hdr(data = data1, aes(x, y), probs = seq(0.00001,0.99,0.1)) + 
  labs(probs = "Value")+
  theme_classic()+
  theme(aspect.ratio = 1, legend.position = "right", legend.text =  element_text(size = 20), legend.title = element_text(size = 20),axis.text = element_text(size = 20), legend.frame = element_rect(linewidth = 20))


plot2 = ggplot() +
  ggdensity::geom_hdr(data = data1, aes(x, y), probs = seq(0.00001,0.99,0.01)) + 
  theme_classic()+
  theme(aspect.ratio = 1, legend.position = "none")


legend = get_legend(plot1)


final_plot <- plot_grid(
  plot2, 
  legend, 
  ncol = 2, 
  align = "v", 
  rel_widths = c(1, 0.15)  # Make legend column narrower
)

final_plot









ggplot() +
  ggdensity::geom_hdr(data = data1, aes(x, y), probs = seq(0.00001,0.99,0.1)) + 
  labs(probs = "Value")+
  theme_classic()+
  theme(aspect.ratio = 1, legend.position = "right", legend.text =  element_text(size = 50), legend.title = element_text(size = 20),axis.text = element_text(size = 20))








data2 = rbind(data1, c(0,0))
data2[499:501,]





