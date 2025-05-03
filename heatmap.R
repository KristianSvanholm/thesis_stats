library(ggplot2)
library(dplyr)
library(forcats)

tau <- read.csv("data/kendalls_tau.csv", header = TRUE, sep = ",")
#tau <- tau %>% 
  #arrange(desc(Value)) %>% 
  #mutate(A = fct_inorder(A))

print(tau)

heatmap <- ggplot(tau, aes(x=A, y=B, fill=Value)) + geom_tile() +theme_bw()+ coord_equal() +
    geom_text(data=tau, aes(x=A, y=B, label=Value, angle=45, color=ifelse(Value < 0.94, 1, 0))) +
    scale_x_discrete( guide=guide_axis(angle = 45), position = "top") +
    scale_y_discrete( guide=guide_axis(angle = 45)) +
    labs(x="", y="") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(legend.position = "none")

ggsave(file= "heatmap.svg",plot=heatmap)



