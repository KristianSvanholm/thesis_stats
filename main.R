library(ggplot2)

getwd()

eng <- read.csv("energy.csv", header = TRUE, sep = ",", dec = ".")
eng

summary(eng)

ggplot(eng, aes(y=energy, color=task)) + geom_bar()
