library(ggplot2)
library(dplyr)

getwd()

concat <- function(...) {
    paste(..., sep="")
}

chiplist <- c("m1max", "m3max")
energy_scale <- 1000000
duration_scale <- 1000

collector <- frame()

for (chip in chiplist) {
    
    collect <- frame()

    high <- read.csv(concat("data/", chip, "/energy.csv"))
    low  <- read.csv(concat("data/", chip, "_low/energy.csv"))
    high["cpu"] <- chip
    high["mode"] <- "high"
    low["cpu"] <- chip
    low["mode"] <- "low"

    collect <- rbind(collect, high)
    collect <- rbind(collect, low)
    collector <- rbind(collector, collect)
    
    high_e <- mean(high$energy) / energy_scale
    high_d <- mean(high$duration) / duration_scale

    low_e <- mean(low$energy) / energy_scale
    low_d <- mean(low$duration) / duration_scale

    print(chip)
    print(high_e)
    print(high_d)
    print("----")
    print(low_e)
    print(low_d)
    print("----")
    print(low_e/high_e)
    print(low_d/high_d)
}

collector["power"] = collector$energy/collector$duration
collector <- aggregate(energy~duration, collector, by=list(collector$cpu,collector$mode), FUN= mean)
#names(collector) = c("cpu", "mode", "power")
print(collector)

#power <- collector
#power["power"] = power$energy/power$duration
#print(power)

bars <- ggplot(collector, aes(x = cpu, y = power, fill = mode)) + geom_bar(position = position_dodge(width=0.5), stat='identity')
ggsave(file="power.svg", bars)

lo_hi_energy <- ggplot(collector, aes(y= energy, x=mode)) + geom_boxplot() + theme_bw()
ggsave(file="low-highboxplot.svg", plot =lo_hi_energy)


bars <- ggplot(collector, aes(x = cpu, y = energy, fill = factor(mode))) + geom_bar(position = position_dodge(width = 0.5), stat='identity')
ggsave(file="barcharts_energy.svg", plot=bars)

bars <- ggplot(collector, aes(x = cpu, y = duration, fill = factor(mode))) + geom_bar(position = position_dodge(width = 0.5), stat='identity')
ggsave(file="barcharts_duration.svg", plot=bars)

cluster <- ggplot(collector, aes(x=energy, y=duration, color=factor(mode), fill=factor(mode))) + geom_point()
ggsave(file="cluster.svg", plot=cluster)


print(collector)
