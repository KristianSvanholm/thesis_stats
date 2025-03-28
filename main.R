library(ggplot2)
library(dplyr)

getwd()

concat <- function(...) {
    paste(..., sep="")
}

chiplist <- c("i78700", "n150", "epyc1", "epyc2", "m1max", "m1max_low", "m2pro", "m3max", "m3max_low")

# Type group
comp <- c("C", "Go", "Rust","Fortran", "Pascal", "Lisp")
virt <- c("Java", "JRuby", "CSharp", "Erlang", "FSharp", "Racket") 
interp <- c("Python", "Perl", "PHP", "Lua", "JavaScript", "TypeScript")

for (chip in chiplist) {

    path <- concat("data/" , chip ,"/")

    eng <- read.csv(concat(path,"energy.csv"), header = TRUE, sep = ",", dec = ".")

    # Aggregate minimum for both energy and duration individually
    min <- aggregate(list(eng$energy,eng$duration), by= list(eng$language, eng$task), FUN = min)
    names(min) = c("language", "task", "energy", "duration")

    # Assign type groups   
    min$type <- "not covered"
    min$type[min$language %in% comp ] <- "Compiled"
    min$type[min$language %in% virt ] <- "Virtualized"
    min$type[min$language %in% interp ] <- "Interpreted"

    # scatter points
    scatter <- ggplot(min, aes(y=energy, x=duration, color=type)) + geom_point()
    ggsave(file=concat(path, "scatter.svg"), plot=scatter)

    #data <- min %>% select(-type, -language, -task) %>% scale()
    #cls <- kmeans(data, centers = 10, iter.max = 100, nstart = 100)
    #clust <- ggplot(data, aes(x=energy, y = duration, color= as.factor(cls$cluster), fill =as.factor(cls$cluster))) + geom_point() + stat_ellipse(type="t", geom = "polygon", alpha = 0.4)
    #ggsave(file=concat(path, "cluster.svg"), plot=clust)

    # Boxplot specific task
    eng2 <- filter(min, min$task == "fannkuch-redux")
    spec_task <- ggplot(eng2, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()
    #ggsave(file=concat(path, "task_avg.svg"), plot=spec_task)

    # Boxplot specific language type
    eng4 <- filter(min, min$type == "Virtualized")
    ggplot(eng4, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

    # Boxplot specific language
    eng3 <- filter(min, min$language == "CSharp")
    ggplot(eng3, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

    # Boxplot energy on task
    eng_on_task <- ggplot(min, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()
    #ggsave(file=concat(path, "energy_on_task.svg"), plot= eng_on_task)

    # Boxplot energy on language
    eng_on_lang <- ggplot(min, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()
    #ggsave(file=concat(path, "energy_on_language.svg"), plot=eng_on_lang)

    # Boxplot energy on language type
    eng_on_cat <- ggplot(min, aes(y=energy, x=type)) + geom_boxplot() +theme_bw()
    ggsave(file=concat(path, "energy_on_language_category.svg"), plot=eng_on_cat)

    # Normalized minimums AVG list by language
    lang_min_avg <- aggregate(x=min$energy, by=list(min$language), mean)

    m <- min(lang_min_avg$x)

    norm <- lang_min_avg$x / m
    new <- data.frame(language=lang_min_avg$Group.1, energy_avg=norm)
    sorted_lang <- new[order(new$energy_avg),]
    rownames(sorted_lang) <- NULL
    write.csv(sorted_lang, file=concat(path, "ranks.csv"))

    # Correlations between duration and energy
    cor(min$duration, min$energy, use = "everything", method ="spearman")
    cor(min$duration, min$energy, use = "everything", method ="kendall")
    cor(min$duration, min$energy, use = "everything", method ="pearson")

    # P test
    cor.test(min$duration, min$energy)

    # Energy & Duration correlate to language ( Very different results with different methods )
    correlation <- min %>%
      group_by(language) %>%
      summarise(correlation = cor(duration, energy, use = "everything", method ="kendall"))

    write.csv(correlation, file=concat(path, "energy_time_cor.csv"))

}
