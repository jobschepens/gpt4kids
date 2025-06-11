library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(viridis)

# from cormat_devel.R devel13
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)


# hist wf distr ----

sum(devel13$norm_chcount < 12 & devel13$norm_chcount > 0) / 
  sum(devel13$norm_chcount > 0)  
sum(devel13$norm_chht < 12 & devel13$norm_chht > 0) /
  sum(devel13$norm_chht > 0)  

sum(devel13$norm_chcount > 12 & devel13$norm_chcount > 0) / 
  sum(devel13$norm_chcount > 0)  
sum(devel13$norm_chht > 12 & devel13$norm_chht > 0) /
  sum(devel13$norm_chht > 0)  

sum(devel13$norm_chcount > 7 & devel13$norm_chcount > 0)  
sum(devel13$norm_chht > 7 & devel13$norm_chht > 0)  


colsd <- devel13 %>% select(
  # norm_logcountplus1_chcount, norm_logcountplus1_chht,
  norm_chcount, norm_chht)
ks.test(colsd$norm_chcount, colsd$norm_chht)
wilcox.test(colsd$norm_chcount, colsd$norm_chht)
(wilcox_test <- wilcox.test(colsd$norm_chcount, colsd$norm_chht))
rbc <- wilcox_test$statistic / (length(colsd$norm_chcount) * length(colsd$norm_chht))
rbc

# cols <- cols %>% mutate(sum = (norm_logcountplus1_chht + norm_logcountplus1_chcount)/2)
# summary(cols)
# cor.test(cols$norm_logcountplus1_chcount, cols$norm_logcountplus1_chht)

clsd <- colsd %>% pivot_longer(cols = everything(), names_to = "measure", values_to = "frequency")

# Plot histogram of word frequencies
histdev <- ggplot(clsd, aes(x = frequency, fill = measure)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  # facet_wrap(~measure, scales = "free") +
  # scale_x_continuous() +  
  scale_x_log10() +  
  scale_fill_viridis(discrete = T, begin = .1, end = .5, name = "Measure",
                     labels = c("norm_chcount" = "Childlex", 
                                "norm_chht" = "Child high temp.")) + 
  labs(x = "Word frequency per million (log scale)",
       y = "Number of DeveL words") +
  theme_minimal()
histdev
# ggsave(filename = "figures/wf_distr_devel.pdf", width = 6, height = 4, dpi = 300)


# hist wf all 4 distr ----

colsd <- devel13 %>% 
  # filter(value_chcount > 0,
  #        value_chht > 0, 
  #        value_chlt > 0, 
  #        value_ai_freq > 0) %>%
  select(norm_logcountplus1_chcount, 
         norm_logcountplus1_chht,
         norm_logcountplus1_chlt,
         norm_logcountplus1_ai_freq)
# select(
# norm_chcount, 
# norm_chht,
# norm_chlt,
# norm_ai_freq)

# ks.test(colsd$norm_chcount, colsd$norm_chht)
# ks.test(colsd$norm_chcount, colsd$norm_chlt)
# ks.test(colsd$norm_chcount, colsd$norm_ai_freq)
# 
# wilcox.test(colsd$norm_chcount, colsd$norm_chht)
# wilcox.test(colsd$norm_chcount, colsd$norm_chlt)
# wilcox.test(colsd$norm_chcount, colsd$norm_ai_freq)

# (wilcox_test <- wilcox.test(colsd$norm_chcount, colsd$norm_chht))
# rbc <- wilcox_test$statistic / (length(colsd$norm_chcount) * length(colsd$norm_chht))
# rbc

# cols <- cols %>% mutate(sum = (norm_logcountplus1_chht + norm_logcountplus1_chcount)/2)
# summary(cols)
# cor.test(cols$norm_logcountplus1_chcount, cols$norm_logcountplus1_chht)

clsd <- colsd %>% pivot_longer(cols = everything(), names_to = "measure", values_to = "frequency")

# clsd <- clsd %>% filter(frequency > 0)
# clsd$frequency <- round(clsd$frequency / 0.3) * 0.3
clsd$frequency[clsd$frequency < 0] <- 0
# cls$frequency[exp(cls$frequency) < 0] <- 
# clsd <- clsd %>% mutate(frequency = if_else(frequency == 0, 0.3, frequency))
# table(clsd$measure[clsd$frequency < 1], clsd$frequency[clsd$frequency < 1])


head(sort(unique(clsd$frequency)))
# Plot histogram of word frequencies
histdev <- ggplot(clsd, aes(x = frequency + 0, fill = measure)) +
  geom_histogram(alpha = 0.5, position = "dodge", bins = 30) +
  # geom_density(alpha = 0.5, position = "identity", bw = .1) +
  # facet_wrap(~measure, scales = "free") +
  # scale_x_continuous() +  
  # scale_x_log10() +  
  scale_fill_viridis(discrete = T, begin = .01, end = .99, name = "Measure",
                     # labels = c("norm_chcount" = "ChildLex",
                     #            "norm_chht" = "Child high temp.",
                     #            "norm_chlt" = "Child low temp.", 
                     #            "norm_ai_freq" = "Child Exp. 1")) +
                     labels = c("norm_logcountplus1_chcount" = "ChildLex",
                                "norm_logcountplus1_chlt" = "Child high temp.",
                                "norm_logcountplus1_chht" = "Child low temp.", 
                                "norm_logcountplus1_ai_freq" = "Child Exp. 1")) +
  labs(x = "Word frequency per million (log scale)",
       y = "Number of DeveL words") +
  theme_minimal()
histdev

# ggsave(filename = "figures/wf_distr_devel_all4.pdf", width = 6, height = 3, dpi = 300)

