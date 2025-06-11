library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(viridis)

# hist ----
load(file = "data-processed/all_wider_again_exp2.RData", verbose = T)
# colnames(all_wider_againl)

cols <- all_wider_againl %>% select(norm_chcount, norm_chht)
ks.test(cols$norm_chcount, cols$norm_chht) # higher d value
cut = 25
cut2 = 200
sum(cols$norm_chcount > cut)
sum(cols$norm_chht > cut)
# at a wf of 27, where only abot 3000 words are left, no significant difference anymore. 
ks.test(cols$norm_chcount[cols$norm_chcount > cut], cols$norm_chht[cols$norm_chht > cut]) #still significant
wilcox.test(cols$norm_chcount[cols$norm_chcount > cut], cols$norm_chht[cols$norm_chht > cut]) #still significant
(wilcox_test <- wilcox.test(cols$norm_chcount, cols$norm_chht))

ks.test(cols$norm_chcount[cols$norm_chcount > cut & cols$norm_chcount < cut2], 
        cols$norm_chht[cols$norm_chht > cut & cols$norm_chht < cut2]) #still significant
wilcox.test(cols$norm_chcount[cols$norm_chcount > cut], cols$norm_chht[cols$norm_chht > cut]) #still significant

## hist2 ---- 

cls <- cols %>% pivot_longer(cols = everything(), names_to = "measure", values_to = "frequency")
cls <- cls %>% filter(frequency > 0)

cls$frequency <- round(cls$frequency / 0.3) * 0.3

cls <- cls %>% mutate(frequency = if_else(frequency == 0, 0.3, frequency))

table(cls$measure[cls$frequency < 2], cls$frequency[cls$frequency < 2])

# Plot histogram of word frequencies

histall <- ggplot(cls, aes(x = frequency, fill = measure)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 100) +
  # geom_density(alpha = 0.5, position = "identity", bw = .5) +
  # scale_x_continuous(limits = c(5, 50)) +
  # scale_x_log10(limits = c(10, 30)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_viridis(discrete = T, begin = .1, end = .5, name = "Measure",
                     labels = c("norm_chcount" = "Childlex", 
                                "norm_chht" = "Child high temp.")) + 
  labs(x = "Word frequency per million (log scale)",
       y = "Number of Words") +
  theme_minimal()
histall
# ggsave(filename = "figures/wf_distr_all.pdf", width = 6, height = 4, dpi = 300)

# llm is higher starting at about 20
ggplot(cls, aes(x = frequency, color = measure)) +
  # geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  geom_freqpoly(alpha = 0.5, position = "identity", bins = 100) +
  # geom_density(alpha = 0.5, position = "identity", bw = .5) +
  # scale_x_continuous(limits = c(10, 30)) +
  # scale_x_log10(limits = c(10, 30)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_viridis(discrete = T, begin = .1, end = .5, name = "Measure",
                     labels = c("norm_chcount" = "Childlex", 
                                "norm_chht" = "Child high temp.")) + 
  labs(x = "Word frequency per million",
       y = "Number of Words") +
  theme_minimal()

ggplot(cls, aes(x = frequency, color = measure)) +
  # geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
  geom_density(alpha = 0.5, position = "identity", bw = .1) +
  # scale_x_continuous(limits = c(-1.5, 10)) +
  # scale_x_log10(limits = c(-1.5, 10)) +
  scale_x_log10() +
  scale_y_log10(limits = c(.01, NA)) +
  scale_fill_viridis(discrete = T, begin = .1, end = .5, name = "Measure",
                     labels = c("norm_chcount" = "Childlex", 
                                "norm_chht" = "Child high temp.")) + 
  labs(x = "Word Frequency (log scale)",
       y = "Number of Words") +
  theme_minimal()
# ggsave(filename = "figures/wf_distr_all.pdf", width = 6, height = 4, dpi = 300)


## hist4 ----

cols <- all_wider_againl %>% 
  # filter(value_chcount > 0,
  #        value_chht > 0,
  #        value_chlt > 0,
  #        value_ai_freq > 0) %>%
  select(norm_logcountplus1_chcount, 
         norm_logcountplus1_chht, 
         norm_logcountplus1_chlt,
         norm_logcountplus1_ai_freq)
# value_chcount,
# value_chht,
# value_chlt,
# value_ai_freq) 

cls <- cols %>% pivot_longer(cols = everything(), names_to = "measure", values_to = "frequency")
print(cls[cls$frequency > 10,], n = 100)
summary(cls$frequency)
summary(log10(cls$frequency))
sum(is.nan(log(cls$frequency)))

cls$frequency[cls$frequency < 0] <- 0


# cls$frequency[cls$frequency > 10] <- 10
# cls <- cls %>% mutate(frequency = if_else(frequency == 0, 0.3, frequency))

histall <- ggplot(cls, aes(x = frequency, fill = measure)) +
  geom_histogram(alpha = 0.5, position = "dodge", bins = 30) +
  # scale_y_log10() +
  scale_y_continuous(trans = scales::transform_pseudo_log(sigma = 1),
                     # limits = c(.1, NA),
                     breaks = c(0, 0.1, 1, 10, 100, 1000, 10000), minor_breaks = NULL) +
  # breaks = c(1, 100, 10000), minor_breaks = NULL) + 
  # scale_y_continuous(trans = 'log10', limits = c(1e-6, NA)) +
  # scale_y_continuous(breaks = c(0, 10, 100, 1000, 10.000), trans = 'log1p') +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) + 
  scale_fill_viridis(discrete = T, begin = .01, end = .99, name = "Measure",
                     labels = c("norm_logcountplus1_chcount" = "ChildLex",
                                "norm_logcountplus1_chlt" = "Child high temp.",
                                "norm_logcountplus1_chht" = "Child low temp.", 
                                "norm_logcountplus1_ai_freq" = "Child Exp. 1")) +
  labs(x = "Word frequency per million (log scale)",
       y = "Number of Words") +
  theme_minimal()
histall

ggsave(filename = "figures/wf_distr_all_all4-j.pdf", width = 6, height = 4, dpi = 300)


library(patchwork)
combined_patchwork <- histall + histdev + 
  plot_layout(ncol = 2) +
  plot_layout(guides = "collect") + 
  plot_annotation(title = "", tag_levels = 'A') 
combined_patchwork

# ggsave(combined_patchwork, filename = "figures/combined_plot.pdf", width = 18, height = 10, dpi = 300)
# ggsave(combined_patchwork, filename = "figures/combined_hist.pdf", width = 10, height = 5, dpi = 300)
ggsave(combined_patchwork, filename = "figures/combined_hist_all4.pdf", width = 10, height = 5, dpi = 300)
ggsave(combined_patchwork, filename = "figures/combined_hist_all4-trans.pdf", width = 10, height = 5, dpi = 300)
