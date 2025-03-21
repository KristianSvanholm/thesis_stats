library(ggplot2)
library(dplyr)

getwd()

concat <- function(...) {
    paste(..., sep="")
}

chiplist <- c("i78700", "epyc1", "m2pro", "epyc2", "n150", "m3max")

# Assign Type group
comp = c("C", "Go", "Rust","Fortran", "Pascal", "Lisp")
virt = c("Java", "JRuby", "CSharp", "Erlang", "FSharp", "Racket")
interp = c("Python", "Perl", "PHP", "Lua", "JavaScript", "TypeScript")

for (chip in chiplist) {

    path <- concat("data/" , chip ,"/")

    eng <- read.csv(concat(path,"energy.csv"), header = TRUE, sep = ",", dec = ".")
    eng

    # Assign Type group
    comp = c("C", "Go", "Rust","Fortran", "Pascal")
    virt = c("Java", "JRuby", "CSharp", "Erlang", "FSharp", "Lisp", "Racket")
    interp = c("Python", "Perl", "PHP", "Lua", "JavaScript", "TypeScript")

    eng$type <- "not covered"
    eng$type[eng$language %in% comp ] <- "Compiled"
    eng$type[eng$language %in% virt ] <- "Virtualized"
    eng$type[eng$language %in% interp ] <- "Interpreted"

    summary(eng)
    eng
    min(eng$energy)

    # Boxplot specific task
    eng2 <- filter(eng, eng$task == "fannkuch-redux")
    eng2
    spec_task <- ggplot(eng2, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()
    ggsave(file=concat(path, "task_avg.svg"), plot=spec_task)

    # Boxplot specific language type
    eng4 <- filter(eng, eng$type == "Virtualized")
    eng4
    ggplot(eng4, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

    # Boxplot specific language
    eng3 <- filter(eng, eng$language == "CSharp")
    eng3
    ggplot(eng3, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

    # Boxplot energy on task
    eng_on_task <- ggplot(eng, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()
    ggsave(file=concat(path, "energy_on_task.svg"), plot= eng_on_task)

    # Boxplot energy on language
    eng_on_lang <- ggplot(eng, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()
    ggsave(file=concat(path, "energ_on_language.svg"), plot=eng_on_lang)

    # Boxplot energy on language type
    eng_on_cat <- ggplot(eng, aes(y=energy, x=type)) + geom_boxplot() +theme_bw()
    ggsave(file=concat(path, "energy_on_language_category.svg"), plot=eng_on_cat)

    # Normalized AVG list by language
    lang_avg <- aggregate(x=eng$energy, by= list(eng$language), FUN = mean)
    m <- min(lang_avg$x)

    norm <- lang_avg$x / m
    print(norm)
    new <- data.frame(language=lang_avg$Group.1, energy_avg=norm)
    print(new[order(new$energy_avg),])

    # Normalized AVG list by task // Kinda irrelevant i think.
    task_avg <- aggregate(x=eng$energy, by= list(eng$task), FUN = mean)
    m <- min(task_avg$x)

    norm <- task_avg$x / m
    print(norm)
    new <- data.frame(language=task_avg$Group.1, energy_avg=norm)
    print(new[order(new$energy_avg),])

    # Correlations between duration and energy
    cor(eng$duration, eng$energy, use = "everything", method ="spearman")
    cor(eng$duration, eng$energy, use = "everything", method ="kendall")
    cor(eng$duration, eng$energy, use = "everything", method ="pearson")

    # P test
    cor.test(eng$duration, eng$energy)

    # Energy & Duration correlate to language ( Very different results with different methods )
    correlation <- eng %>%
      group_by(language) %>%
      summarise(correlation = cor(duration, energy,use = "everything", method ="kendall"))

    print(correlation)

}
