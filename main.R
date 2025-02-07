library(ggplot2)
library(dplyr)

getwd()

eng <- read.csv("energy.csv", header = TRUE, sep = ",", dec = ".")
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
View(eng)
eng
min(eng$energy)

# Boxplot specific task
eng2 <- filter(eng, eng$task == "n-body")
eng2
ggplot(eng2, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()

# Boxplot specific language type
eng4 <- filter(eng, eng$type == "Virtualized")
eng4
ggplot(eng4, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

# Boxplot specific language
eng3 <- filter(eng, eng$language == "CSharp")
eng3
ggplot(eng3, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

# Boxplot energy on task
ggplot(eng, aes(y=energy, x=task)) + geom_boxplot() +theme_bw()

# Boxplot energy on language
ggplot(eng, aes(y=energy, x=language)) + geom_boxplot() +theme_bw()

# Boxplot energy on language type
ggplot(eng, aes(y=energy, x=type)) + geom_boxplot() +theme_bw()

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

