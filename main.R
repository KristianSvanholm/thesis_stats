library(ggplot2)
library(dplyr)
library(forcats)
library(tibble)
library(tidyr)

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
cor_collector <- frame()

for (chip in chiplist) {

    path <- concat("data/" , chip ,"/")

    energy <- read.csv(concat(path,"energy.csv"), header = TRUE, sep = ",", dec = ".")

    # minimum for energy for each lang and task
    min <- energy %>%
        group_by(language, task) %>%
        slice_min(energy, n=3, with_ties=FALSE) %>%
        as.data.frame()

    # Compute normalized data
    summary <- min %>%
        group_by(language) %>%
        summarise(across(where(is.numeric),mean), .groups="drop") %>%
        as.data.frame()
    m <- min(summary$energy)
    summary$energy <- summary$energy / m

    # Collect into larger dataset
    norm_collect <- summary
    norm_collect["cpu"] <- chip
    collector <- rbind(collector, norm_collect)

    sorted_lang <- summary[order(summary$energy),]
    rownames(sorted_lang) <- NULL
    write.csv(sorted_lang, file=concat(path, "ranks.csv"))

    # Correlations between duration and energy

    time_cor_results <- energy %>%
        group_by(language) %>%
        summarise(
            cor_result = list(cor.test(energy, duration, method = "pearson", exact=TRUE))
        ) %>%
        rowwise() %>%
        mutate(
            estimate = cor_result$estimate,
            p_value = cor_result$p.value
        ) %>%
        ungroup() %>%
        select(language, estimate, p_value)

    time_cor_results["cpu"] <- chip
    cor_collector <- rbind(cor_collector, time_cor_results)

    # Energy & Duration correlate to language ( Very different results with different methods )
}

## TIME / ENERGY CORRELATION
cor_collector <- cor_collector %>% mutate(cpu = fct_inorder(cpu)) %>% as.data.frame()
write.csv(cor_collector, file="energy_time_cor.csv")

correlations <- ggplot(cor_collector, aes(y=estimate, x=cpu, group=language, color=language)) +
    geom_line() + geom_point() + 
    #geom_text(data=subset(collector, energy > 5 & cpu=="i78700" & language!="Python"),
    #        aes(y=energy,x = cpu,label=language),
    #           vjust=-0.5) +
    #geom_text(data=subset(collector, cpu=="n150" & language=="Python"),
    #        aes(y=energy,x = cpu,label=language),
    #           vjust=+1.25) +
    theme_bw() +
    labs(
        x = "",
        y = "Correlation",
    ) +
    theme(legend.position = "none")
ggsave(file="cross_correlations.svg", plot=correlations)

## ENERGY RANKINGS
collector <- collector %>% mutate(cpu = fct_inorder(cpu)) %>% as.data.frame()

# Assign type groups   
collector$type <- "not covered"
collector$type[collector$language %in% comp ] <- "Compiled"
collector$type[collector$language %in% virt ] <- "Virtualized"
collector$type[collector$language %in% interp ] <- "Interpreted"

# Assign JIT groups
collector$jit <- "No JIT"
collector$jit[collector$language %in% comp] <- "Compiled"
collector$jit[collector$language %in% jit] <- "JIT"


# Boxplot energy on language type
eng_on_cat <- ggplot(collector, aes(y=energy, x=type, color=type)) + geom_boxplot() +theme_bw()+ 
    labs(x = "", y  ="Energy (normalized)") + theme(legend.position= "none")
ggsave(file="energy_category.svg", plot=eng_on_cat)

# Boxplot energy on language type and JIT
eng_on_cat_jit <- ggplot(collector, aes(y=energy, x=type, color=jit)) + geom_boxplot() +theme_bw()+ 
    labs(x = "", y  ="Energy (normalized)")
ggsave(file="energy_category_jit.svg", plot=eng_on_cat_jit)

# Boxplot energy on JIT features
eng_on_jit <- ggplot(collector, aes(y=energy, x=jit, color=jit)) + geom_boxplot() +theme_bw()+ 
    labs(x = "", y  ="Energy (normalized)") + theme(legend.position= "none")
ggsave(file="energy_jit.svg", plot=eng_on_jit)


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


ranked <- collector %>%
            group_by(cpu) %>%
            mutate(rank=rank(energy)) %>%
            ungroup()

wide_ranks <- ranked %>%
    select(cpu, language, rank) %>%
    tidyr::pivot_wider(names_from = cpu, values_from = rank) %>%
    column_to_rownames("language")

print(cor(wide_ranks, method = "kendall"))

# p test tau correlation

print(cpu_names <- colnames(wide_ranks))

results <- combn(cpu_names, 2, function(pair){
    test <- cor.test(wide_ranks[[pair[1]]],
                    wide_ranks[[pair[2]]],
                    method = "kendall", exact=TRUE)

    data.frame(
        cpu1 = pair[1],
        cpu2 = pair[2],
        tau = test$estimate,
        p_value= test$p.value
    )
}, simplify= FALSE)
results_df <- do.call(rbind, results)

results_df %>% arrange(desc(tau)) %>% as.data.frame()

print(results_df)

