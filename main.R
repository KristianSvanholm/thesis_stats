library(ggplot2)
library(dplyr)
library(forcats)

getwd()

concat <- function(...) {
    paste(..., sep="")
}

chiplist <- c("i78700", "n150", "epyc1", "epyc2", "m1max", "m2pro", "m3max")

# Type group
comp <- c("C", "Go", "Rust","Fortran", "Pascal", "Lisp")
virt <- c("Java", "JRuby", "CSharp", "Erlang", "FSharp", "Racket") 
interp <- c("Python", "Perl", "PHP", "Lua", "JavaScript", "TypeScript")

# Jit languages
jit <- c("Java", "CSharp", "FSharp", "JavaScript", "TypesScript", "Racket", "JRuby")

collector <- frame()

for (chip in chiplist) {

    path <- concat("data/" , chip ,"/")

    energy <- read.csv(concat(path,"energy.csv"), header = TRUE, sep = ",", dec = ".")

    # minimum for energy for each lang and task
    min <- energy %>%
        group_by(language, task) %>%
        slice_min(energy, n=3, with_ties=FALSE) %>%
        as.data.frame()

    # Assign type groups   
    min$type <- "not covered"
    min$type[min$language %in% comp ] <- "Compiled"
    min$type[min$language %in% virt ] <- "Virtualized"
    min$type[min$language %in% interp ] <- "Interpreted"

    # Assign JIT groups
    min$jit <- "No"
    min$jit[min$language %in% comp] <- "-"
    min$jit[min$language %in% jit] <- "Yes"

    # scatter points
    #scatter <- ggplot(min, aes(y=energy, x=duration, color=type)) + geom_point()
    #ggsave(file=concat(path, "scatter.svg"), plot=scatter)

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

    # Compute normalized data
    summary <- min %>%
        group_by(language,jit, type) %>%
        summarise(across(where(is.numeric),mean), .groups="drop") %>%
        as.data.frame()
    m <- min(summary$energy)
    summary$energy <- summary$energy / m

    # Boxplot energy on language type
    eng_on_cat <- ggplot(summary, aes(y=energy, x=type, color=jit)) + geom_boxplot() +theme_bw()+ 
        labs(x = "", y  ="Energy (normalized)") +ggtitle(chip);
    ggsave(file=concat(path, "energy_on_language_category_",chip,".svg"), plot=eng_on_cat)

    # Collect into larger dataset
    norm_collect <- summary
    norm_collect["cpu"] <- chip
    collector <- rbind(collector, norm_collect)

    sorted_lang <- summary[order(summary$energy),]
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

collector <- collector %>% mutate(cpu = fct_inorder(cpu)) %>% as.data.frame()

variance <- ggplot(collector, aes(y=energy, x=cpu, group=language, color=language)) +
    geom_line() + geom_point() + 
    geom_text(data=subset(collector, energy > 5 & cpu=="i78700" & language!="Python"),
            aes(y=energy,x = cpu,label=language),
            vjust=-0.5) +
    geom_text(data=subset(collector, cpu=="n150" & language=="Python"),
            aes(y=energy,x = cpu,label=language),
            vjust=+1.25) +
    theme_bw() +
    labs(
        x = "",
        y = "Energy (normalized)",
    ) +
    theme(legend.position = "none")
ggsave(file="variance.svg", plot=variance)

data_ranked <- collector %>%
    group_by(cpu) %>%
    mutate(rank = rank(energy, ties.method = 'min')) %>%
    as.data.frame()

rank_diffs <- ggplot(data_ranked, aes(y=rank, x=cpu, group=language, color=language)) +
    geom_line(linewidth = 1, alpha = 0.7) + 
    geom_point(size = 3) +
    scale_y_reverse(breaks = 1:max(data_ranked$energy)) +
    geom_text(data=subset(data_ranked, cpu=="i78700"),
            aes(y=rank,x = cpu,label=language),
            vjust=-1) +
    labs(
        x = "",
        y = "Rank (Lower is better)",
    ) +
    theme_bw() + 
    theme(legend.position= "none")

ggsave(file="rank_diffs.svg", plot=rank_diffs)

group_ordered <- with(collector, reorder(language, energy, median))

# Boxplot energy on language
eng_on_lang <- ggplot(collector, aes(y=energy, x=group_ordered)) + geom_boxplot() +theme_bw() +
        labs(
            x = "",
            y = "Energy (normalized)",
        ) +
        scale_x_discrete(guide = guide_axis(angle = 45)) 
ggsave(file="energy_on_language.svg", plot=eng_on_lang)

# Print sorted median languages
dta <- collector %>%
        group_by(language) %>%
        summarize(median=median(energy, na.rm = TRUE)) %>%
        arrange(median) %>%
        as.data.frame()

print(dta)


