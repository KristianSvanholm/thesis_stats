library(ggplot2)
library(dplyr)
library(forcats)

tau <- read.csv("data/kendalls_tau.csv", header = TRUE, sep = ",")
#tau <- tau %>% 
  #arrange(desc(Value)) %>% 
  #mutate(A = fct_inorder(A))

print(tau)

heatmap <- ggplot(tau, aes(x=A, y=B, fill=Value)) + geom_tile() +theme_bw()+
    geom_text(data=tau, aes(x=A, y=B, label=Value, color=ifelse(Value < 0.94, 1, 0))) +
    labs(x="", y="") +
    theme(legend.position = "none")

ggsave(file= "heatmap.svg",plot=heatmap)



