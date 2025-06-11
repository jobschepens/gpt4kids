library(udpipe)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(viridis)

# determine overlap (%) for devel

# from cormat_devel.R devel13
# load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob-exp3-2.RData", verbose = T)

a <- devel13 %>% filter(value_chht == 0) %>% select(word, value_chcount, value_chht)
a <- devel13 %>% filter(value_dsl2 == 0) %>% select(word, value_chcount, value_dsl2)
a <- devel13 %>% filter(value_lll2 == 0) %>% select(word, value_chcount, value_lll2)
length(a$word)
cat(paste(a$word, collapse = " "), "\n")

b <- devel13 %>% filter(value_chcount == 0) %>% select(word, value_chcount, value_chht)
cat(paste(b$word, collapse = " "), "\n")

# make heatmap ----

# Assuming all_wider_again is your data frame
# Selecting columns that include "value" in their names
value_columns <- select(devel13, contains("value"), 
                        # rt.g1.m, 
                        rt.g2.m) # %>%
  # select(-contains("value_chcount"))
# value_columns <- value_columns %>% mutate(across(rt.g1.m, ~replace_na(., 0)))
colnames(value_columns)

# Function to count non-zero pairs in each row for two columns
# words with F in both corpora
count_nonzero_pairs <- function(col1, col2) {
  sum(col1 != 0 & col2 != 0)
  # sum(col1 != 1 & col2 != 1)
}

# Create an empty data frame to store the results
results <- data.frame(matrix(ncol = ncol(value_columns), nrow = ncol(value_columns)))
names(results) <- names(value_columns)
rownames(results) <- names(value_columns)

# Compute the count of non-zero pairs for each combination of columns
for (i in seq_along(value_columns)) {
  for (j in seq_along(value_columns)) {
    if (i <= j) {  # To avoid redundant calculations
      results[i, j] <- count_nonzero_pairs(value_columns[[i]], value_columns[[j]])
      results[j, i] <- results[i, j]  # Symmetric matrix
    }
  }
}
# 51367/83921 # adlt_adht/adhttotal
# 51367/71423 # adlt_adht/adltttotal

# print(results)

# Convert the results into a matrix if it's not already
results <- as.matrix(results)

# Create a copy of the matrix to store percentages
percentage_matrix <- matrix(nrow = nrow(results), ncol = ncol(results))
rownames(percentage_matrix) <- rownames(results)
colnames(percentage_matrix) <- colnames(results)

# Diagonal values represent the total counts for each variable
# number of types
totals <- diag(results)

# number of tokens
# sums <- all_wider_againl %>% group_by(group) %>% 
# 	summarize(sum = sum(value))
# sums

# shared / total words
# Fill the lower triangle (retrieved from the row)
for (i in 1:nrow(results)) {
  for (j in 1:i) {
    percentage_matrix[i, j] <- results[i, j] / totals[i] * 100
  }
}
# results[2,1]

# Fill the upper triangle (retrieved from the column)
for (j in 1:ncol(results)) {
  for (i in 1:j) {
    percentage_matrix[i, j] <- results[i, j] / totals[i] * 100
  }
}

# more intuitive
percentage_matrix <- t(percentage_matrix)

# Print the transformed percentage matrix
print(percentage_matrix, digits = 2)

# Convert the matrix into a long format for ggplot
percentage_df <- as.data.frame(as.table(percentage_matrix))
colnames(percentage_df) <- c("Row", "Column", "Percentage")
percentage_df$Percentage <- round(percentage_df$Percentage, 1)

descriptive_labels <- c(
  # "rt.g1.m" = "rt.g1.m", 
  "DeveL" = "rt.g2.m", 
  "SUBTLEX" = "value_subtlex", 
  "ChildLex" = "value_chcount",
  "DeepSeek long" = "value_dsl2",
  "DeepSeek short" = "value_dss2",
  "LLama long" = "value_lll2",
  "Llama short" = "value_lls2",
  # "Child high temp." = "value_chht",
  # "Child low temp." = "value_chlt",
  # "Adult high temp." = "value_adht",
  # "Adult low temp." = "value_adlt",
  "Exp. 1" = "value_ai_freq"
)

# Applying the mapping and reordering within the Tidyverse
percentage_df <- percentage_df %>%
  mutate(
    Row = fct_recode(as.factor(Row), !!!descriptive_labels),
    Column = fct_recode(as.factor(Column), !!!descriptive_labels)
  )

percentage_df$Row <- factor(percentage_df$Row, levels = names(descriptive_labels))
percentage_df$Column <- factor(percentage_df$Column, levels = names(descriptive_labels))

# percentage_df
aap <- percentage_df %>%
  filter(!Column %in% c("SUBTLEX"),
         !Row %in% c("SUBTLEX", "DeveL"))
  # filter(!Column %in% c("SUBTLEX", "DeveL"),
  #        !Row %in% c("SUBTLEX", "DeveL"))

# Plotting the heatmap
heatm_devel <- ggplot(aap, aes(x = Column, y = Row, fill = Percentage)) +
  geom_tile(color = "white") +  # Adds border color for distinction
  geom_text(aes(label = paste0(Percentage, "%")), size = 2.5, color = "black") +  # Add text labels
  scale_fill_viridis(begin = .5, end = 1, name = "Overlap (%)") +  # Color gradient
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    # axis.title.x = element_text(size = 12, face = "bold"),
    # axis.title.y = element_text(size = 12, face = "bold")
    legend.position = "top"
  ) +
  labs(
    x = "", 
    y = "", 
    fill = "",
    title = ""
  ) +
  coord_fixed()
heatm_devel
# ggsave(plot = heatm_devel, filename = "figures/heatmap_devel.pdf", width = 6, height = 5, dpi = 300)


# # combine plots ----
# 
# # library(cowplot)
# # combined_cowplot <- plot_grid(cormat, heatm, cormat_devel)
# # combined_cowplot
# 
# library(patchwork)
# combined_patchwork <- cormat + heatm + 
#   plot_layout(ncol = 2) +
#   plot_annotation(title = "", tag_levels = 'A') 
# combined_patchwork
# 
# # ggsave(combined_patchwork, filename = "figures/combined_plot.pdf", width = 18, height = 10, dpi = 300)
# ggsave(combined_patchwork, filename = "figures/combined_plot2_devel.pdf", width = 8, height = 5, dpi = 300)