library(ggplot2)

tau <- read.csv("data/kendalls_tau.csv", header = TRUE, sep = ",")

heatmap <- ggplot(tau, aes(x=A, y=B, fill=Value)) + geom_tile() +theme_bw()+ coord_equal() +
    scale_fill_gradient(low = "#0F1926", high = "#038C33") +
    geom_text(data=tau, aes(x=A, y=B, label=Value, angle=45, color="white")) + # This does not end up as white, edit in svg file
    scale_x_discrete( guide=guide_axis(angle = 45), position = "top") +
    scale_y_discrete( guide=guide_axis(angle = 45)) +
    labs(x="", y="") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(legend.position = "none")

ggsave(file= "heatmap.svg",plot=heatmap)



