library(dplyr)
library(udpipe)
library(readtext)
library(tidytext)
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


# from wider_again.R
load("data-processed/all_wider_again_subt_ch_plus1_prob.RData", verbose = T)
# load("data-processed/all_wider_again_subt_ch_plus1_childlex2.RData", verbose = T)

# pos ----

# de <- udpipe_download_model(language = "german-hdt") # "german-gsd", "german-hdt"
# de <- udpipe_download_model(language = "german") # "german-gsd", "german-hdt"
# save(de, file = "data-original/udpipe-de.RData")
# load("data-original/udpipe-de.RData", verbose = T)
# de
# udmodel_german <- udpipe_load_model(file = de$file_model)

# Extract words from the dataframe
word_list <- all_wider_againl$merged_type  # Assuming 'cols' is your dataframe with words
head(word_list)
# Perform POS tagging
# pos_result <- udpipe_annotate(udmodel_german, x = word_list)
# save(pos_result, file = "data-processed/pos_result.RData")
# pos_df <- as.data.frame(pos_result)

# Merge POS tags back into the original dataframe
all_wider_againl2 <- all_wider_againl %>%
  left_join(select(pos_df, token, upos), by = c("merged_type" = "token"))

all_wider_againl2$upos <- as.factor(all_wider_againl2$upos)
summary(all_wider_againl2$upos)

## counting types and tokens ---- 

# Count occurrences of each POS tag
pos_counts <- all_wider_againl2 %>%
  filter(norm_chht > 0) %>%
  count(upos) %>%
  arrange(desc(n)) %>%
  mutate(cor = "Child high temp. types")

# Sum the frequency per million (norm_chcount) within each POS tag
pos_countst <- all_wider_againl2 %>%
  filter(norm_chht > 0) %>%
  group_by(upos) %>%
  summarize(n = sum(norm_chht)) %>%
  mutate(cor = "Child high temp. tokens") 

# Count occurrences of each POS tag
pos_countsc <- all_wider_againl2 %>%
  filter(norm_chcount > 0) %>%
  count(upos) %>%
  arrange(desc(n))  %>%
  mutate(cor = "ChildLex types")

# Sum the frequency per million (norm_chcount) within each POS tag
pos_countsct <- all_wider_againl2 %>%
  filter(norm_chcount > 0) %>%
  group_by(upos) %>%
  summarize(n = sum(norm_chcount)) %>%
  arrange(desc(n)) %>%
  mutate(cor = "ChildLex tokens")

## rbind ----

a <- rbind(pos_counts, pos_countsc, pos_countst, pos_countsct)
a$cor <- factor(a$cor, levels = c(
  "Child high temp. types", "ChildLex types", 
  "Child high temp. tokens", "ChildLex tokens"))
summary(a)


## plot types unordered ----

# Plot POS tag distribution
pos_f <- ggplot(a, aes(x = reorder(upos, n), y = n, fill = upos)) +
  facet_wrap(vars(cor), scales = "free_x") +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "Child high temp.",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()
pos_f


## reorder 1 --- 

library(ggplot2)
library(dplyr)

# Define the reference order based on "ChildLex tokens"
upos_order <- a %>%
  filter(cor == "ChildLex types") %>%
  arrange(desc(n)) %>%
  pull(upos)

# Apply this order to all facets
a <- a %>%
  mutate(upos = factor(upos, levels = upos_order))

# Plot
pos_f <- ggplot(a, aes(x = upos, y = n, fill = upos)) +
  facet_wrap(vars(cor), scales = "free_x") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Child high temp.",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()
pos_f


## two orders ---- 

library(ggplot2)
library(dplyr)

# Create ordering for ChildLex tokens
tokens_order <- a %>%
  filter(cor == "ChildLex tokens") %>%
  arrange(desc(n)) %>%
  pull(upos)

# Create ordering for ChildLex types
types_order <- a %>%
  filter(cor == "ChildLex types") %>%
  arrange(desc(n)) %>%
  pull(upos)

# Assign different orders per facet by creating a new variable
a <- a %>%
  mutate(upos_faceted = case_when(
    cor %in% c("ChildLex tokens", "LLM tokens") ~ factor(upos, levels = tokens_order),
    cor %in% c("ChildLex types", "LLM types") ~ factor(upos, levels = types_order),
    TRUE ~ upos  # Default case (shouldn't be needed)
  ))

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
ggsave(pos_f, filename = "figures/pos_f.pdf", width = 10, height = 5, dpi = 300)

options(scipen = 999)

## plot types ----

# Plot POS tag distribution
pos_alll <- ggplot(pos_counts, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "Child high temp.",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()
pos_alll
# ggsave(filename = "figures/pos_devel-all.pdf", width = 6, height = 4, dpi = 300)

# Plot POS tag distribution
pos_alllc <- ggplot(pos_countsc, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "ChildLex",
       x = "POS Tag",
       y = "Frequency") +
  theme_minimal()
pos_alllc

combined_patchwork <- pos_alll + pos_alllc + # pos_dev + 
  plot_layout(ncol = 3, 
              guides = "collect",
              axis_titles = "collect") + 
  plot_annotation(title = "", tag_levels = 'A') 
combined_patchwork

# ggsave(combined_patchwork, filename = "figures/combined_plot.pdf", width = 18, height = 10, dpi = 300)
# ggsave(combined_patchwork, filename = "figures/combined_pos.pdf", width = 10, height = 5, dpi = 300)



## plot tokens ----

# Plot POS tag distribution based on tokens per million
pos_alllt <- ggplot(pos_countst, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "Child high temp.",
       x = "POS Tag",
       y = "Tokens per Million") +
  theme_minimal()
pos_alll

pos_alllct <- ggplot(pos_countsct, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip for better readability
  labs(title = "ChildLex",
       x = "POS Tag",
       y = "Tokens per Million") +
  theme_minimal()
pos_alllc

combined_patchwork <- 
  pos_alll + pos_alllc + # pos_dev + 
  pos_alllt + pos_alllct +
  plot_layout(ncol = 2, 
              guides = "collect",
              axis_titles = "collect") + 
  plot_annotation(title = "", tag_levels = 'A') 
combined_patchwork
# ggsave(combined_patchwork, filename = "figures/combined_pos_tokens.pdf", width = 10, height = 5, dpi = 300)
