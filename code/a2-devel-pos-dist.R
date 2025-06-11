library(udpipe)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(viridis)

# from cormat_devel.R devel13
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)


# pos ----

cols <- select(devel13, word,
               # norm_logcountplus1_chcount,
               # norm_logcountplus1_chht,
               norm_chht,
               norm_chcount
)

# de <- udpipe_download_model(language = "german-hdt") # "german-gsd", "german-hdt"
# de <- udpipe_download_model(language = "german") # "german-gsd", "german-hdt"
# save(de, file = "data-original/udpipe-de.RData")
# load("data-original/udpipe-de.RData", verbose = T)
# de
# udmodel_german <- udpipe_load_model(file = de$file_model)

# Extract words from the dataframe
word_list <- cols$word  # Assuming 'cols' is your dataframe with words
head(word_list)
# Perform POS tagging
pos_result_devel <- udpipe_annotate(udmodel_german, x = word_list)
pos_df_devel <- as.data.frame(pos_result_devel)

# Merge POS tags back into the original dataframe
cols <- cols %>%
  left_join(select(pos_df_devel, token, upos), by = c("word" = "token"))

# Count occurrences of each POS tag
pos_counts <- cols %>%
  count(upos) %>%
  arrange(desc(n))

# Plot POS tag distribution
pos_dev <- ggplot(pos_counts, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "DeveL",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()
pos_dev
# ggsave(filename = "figures/pos_devel.pdf", width = 6, height = 4, dpi = 300)


## counting types and tokens ---- 

# Count occurrences of each POS tag
low <- cols %>%
  filter(norm_chht < 1 & norm_chcount > 1)
low

pos_counts <- cols %>%
  filter(norm_chht < 1 & norm_chcount > 1) %>% 
  count(upos) %>%
  arrange(desc(n)) %>%
  mutate(cor = "DeveL types missing in Child high temp.")

# Sum the frequency per million (norm_chcount) within each POS tag
pos_countst <- cols %>%
  filter(norm_chht > 0 ) %>%
  group_by(upos) %>%
  summarize(n = sum(norm_chht)) %>%
  mutate(cor = "Child high temp. tokens") 

# Count occurrences of each POS tag
pos_countsc <- cols %>%
  filter(norm_chcount > 0) %>%
  count(upos) %>%
  arrange(desc(n))  %>%
  mutate(cor = "DeveL types")

# Sum the frequency per million (norm_chcount) within each POS tag
pos_countsct <- cols %>%
  filter(norm_chcount > 0) %>%
  group_by(upos) %>%
  summarize(n = sum(norm_chcount)) %>%
  arrange(desc(n)) %>%
  mutate(cor = "ChildLex tokens")

## rbind ----

a <- rbind(pos_counts, pos_countsc, pos_countst, pos_countsct)
a$cor <- factor(a$cor, levels = c(
  "DeveL types missing in Child high temp.", "DeveL types", 
  "Child high temp. tokens", "ChildLex tokens"))
summary(a)

a$upos <- as.factor(a$upos)

## two orders ---- 

library(ggplot2)
library(dplyr)

# Create ordering for ChildLex tokens
tokens_order <- a %>%
  filter(cor == "ChildLex tokens") %>%
  arrange(desc(n)) %>%
  pull(upos)

# # Create ordering for ChildLex types
# types_order <- a %>%
#   filter(cor == "ChildLex types") %>%
#   arrange(desc(n)) %>%
#   pull(upos)

# Assign different orders per facet by creating a new variable
a <- a %>%
  mutate(upos_faceted = case_when(
    cor %in% c("ChildLex tokens", "LLM tokens") ~ factor(upos, levels = tokens_order),
    # cor %in% c("ChildLex types", "LLM types") ~ factor(upos, levels = types_order),
    TRUE ~ upos  # Default case (shouldn't be needed)
  ))
# a
a$upos
a$upos_faceted

# Plot using upos_faceted instead of upos
pos_f <- ggplot(a, aes(x = upos_faceted, y = n, fill = upos)) +
  facet_wrap(vars(cor), scales = "free_x") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()

pos_f
ggsave(pos_f, filename = "figures/pos_f_devel.pdf", width = 10, height = 5, dpi = 300)

options(scipen = 999)
