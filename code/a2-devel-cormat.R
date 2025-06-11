library(ggplot2)
library(reshape2)
library(textstem)
library(ggrepel)
library(tm)
library(readtext)
library(SnowballC)
library(ggplot2)
library(tidyverse)
library(scales)
library(lexicon)
library(corrplot)
library(lme4)
library(viridis)


# compute cor mat ---- 
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob-exp3.RData", verbose = T)
exp3 <- devel13

load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)
sum(is.na(devel13$Word))
sum(is.na(devel13$word))
sum(is.na(devel13$wordlower))
devel13$word[is.na(devel13$Word)] # Obstbaum, verhaspeln

# add 
sel <- exp3 %>% select(word, 
                norm_logcountplus1_dslo,
                norm_logcountplus1_dssh, 
                norm_logcountplus1_lllo, 
                norm_logcountplus1_llsh)
sel
devel13 <- left_join(devel13, sel, by = "word")
# tibble(devel13)
# summary(devel13)
colnames(devel13)
head(devel13)


## make cor matrix including devel ----

## compute matrix ----

# sel_colsl <- devel1 %>%
sel_colsl <- devel13 %>%
  # select(contains("norm_log_") | rt.g1.m | rt.g2.m | rt.avg.m)
  # select(contains("norm_log_") |logplus1 rt.g1.m | rt.g2.m | rt.avg.m | 
  select(contains("norm_logcountplus1_") | 
           rt.g1.m | rt.g2.m | rt.avg.m | # norm_log_dwds |
           rt.g2.m | rt.g3.m | rt.g4.m | rt.g6.m | 
           rt.ya.m | rt.oa.m |
           child.old20 | aoa | letter.cnt | unigram | bigram | trigram)
# select(contains("norm_log_") | rt.g1.m | rt.g2.m)

# aap <- tibble(devel13 %>% select(word, wordlower, norm_logcountplus1_chcount_devel, norm_logcountplus1_chcount))
aap <- tibble(devel13 %>% select(word, wordlower, norm_logcountplus1_chlt, norm_logcountplus1_chcount))
# print(aap, n = 100)

correlation_matrix <- cor(sel_colsl, use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix

obs_matrix <- outer(
  colnames(sel_colsl), colnames(sel_colsl),
  Vectorize(function(x, y) sum(complete.cases(sel_colsl[, c(x, y)])))
)

rownames(obs_matrix) <- colnames(sel_colsl)
colnames(obs_matrix) <- colnames(sel_colsl)

# no 0's
# sel_colslno0 <- sel_colsl %>%
#   mutate(across(everything(), ~na_if(., 0)))
# summary(sel_colslno0)
# correlation_matrixno0 <- cor(sel_colslno0, use = "pairwise.complete.obs")
# correlation_matrixno0 <- round(correlation_matrixno0, 2)
# correlation_matrixno0


## plot cormat ----

melted_cor <- melt(correlation_matrix)
colnames(melted_cor) <- c("Row", "Column", "Percentage")
melted_cor$Percentage <- round(melted_cor$Percentage, 2)

melted_obs <- melt(obs_matrix)
colnames(melted_obs) <- c("Row", "Column", "Number")
melted_cor$number <- as.integer(melted_obs$Number)


descriptive_labels <- c(
  "SUBTLEX" = "norm_logcountplus1_subtlex",
  "DWDS" = "norm_logcountplus1_dwds",
  "ChildLex" = "norm_logcountplus1_chcount",
  # "ChildLexD" = "norm_logcountplus1_chcount_devel",
  "OLD20" = "child.old20",
  "AoA" = "aoa",
  "Letter Count" = "letter.cnt",
  "Unigram" = "unigram",
  "Bigram" = "bigram",
  "Trigram" = "trigram",
  
  "Adult low temp." = "norm_logcountplus1_adlt",	
  "Adult high temp." = "norm_logcountplus1_adht",
  "Child low temp." = "norm_logcountplus1_chlt",
  "Child high temp." = "norm_logcountplus1_chht",
  
  "Deepseek long" = "norm_logcountplus1_dslo",	
  "DeepSeek short" = "norm_logcountplus1_dssh",
  "LLama long" = "norm_logcountplus1_lllo",
  "LLama short" = "norm_logcountplus1_llsh",
  
  "Exp. 1" = "norm_logcountplus1_ai_freq",
  "Grade 1 RT" = "rt.g1.m",
  "Grade 2 RT" = "rt.g2.m",
  "Grade 3 RT" = "rt.g3.m",
  "Grade 4 RT" = "rt.g4.m",
  "Grade 6 RT" = "rt.g6.m",
  "Average child RT" = "rt.avg.m",
  "YA RT" = "rt.ya.m",
  "OA RT" = "rt.oa.m"
)

# unique(melted_cor$Row)

# Recode factor levels in Row and Column based on descriptive_labels
# !!! splices the key-value pairs from the descriptive_labels list into fct_recode
melted_cor2 <- melted_cor %>%
  mutate(
    Row = fct_recode(as.factor(Row), !!!descriptive_labels),
    Column = fct_recode(as.factor(Column), !!!descriptive_labels)
  )

# Explicitly set the factor levels of Row and Column to match the order of descriptive_labels
melted_cor2$Column <- factor(melted_cor2$Column, levels = names(descriptive_labels))
melted_cor2$Row <- factor(melted_cor2$Row, levels = names(descriptive_labels))

# for obs too
melted_obs2 <- melted_obs %>%
  mutate(
    Row = fct_recode(as.factor(Row), !!!descriptive_labels),
    Column = fct_recode(as.factor(Column), !!!descriptive_labels)
  )

# Explicitly set the factor levels of Row and Column to match the order of descriptive_labels
melted_obs2$Column <- factor(melted_obs2$Column, levels = names(descriptive_labels))
melted_obs2$Row <- factor(melted_obs2$Row, levels = names(descriptive_labels))



cormat_devel <- ggplot(melted_cor2, aes(Row, Column, fill = Percentage)) +
  geom_tile() + # This adds the fill to the plot
  geom_text(aes(label = sprintf("%.2f", Percentage)), color = "black", size = 2) + # Adds text labels
  scale_fill_viridis(begin = .5, end = 1, name = "Pearson\nCorrelation") +  # Color gradient
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_fixed()
cormat_devel
# no lexstats
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel.pdf", width = 6, height = 5) 
# with lexstats
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel.pdf", width = 7, height = 6)
# with all rts
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel.pdf", width = 9, height = 8)
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_supd_plus1.pdf", width = 9, height = 8)
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_s_lower.pdf", width = 9, height = 8)
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_supd_plus1-prob.pdf", width = 9, height = 8)
ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_may.pdf", width = 9, height = 8)


obsmat_devel <- ggplot(melted_obs2, aes(Row, Column, fill = Number)) +
  geom_tile() + # This adds the fill to the plot
  geom_text(aes(label = sprintf("%.0f", Number)), color = "black", size = 2) + # Adds text labels
  scale_fill_viridis(begin = .5, end = 1, name = "Pairwise\ncomplete\nobservations") +  # Color gradient
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_fixed()
# obsmat_devel

summary(result$rt.g6.m)
summary(result$rt.g1.m)
summary(result$child.lemma.freq)
summary(result$child.type.freq)
