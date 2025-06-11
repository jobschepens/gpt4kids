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

# This script is used to create a correlation matrix between different frequency measures. 

# load exp1 data ----

# merged data from tm_ben.Rmd
load("data-processed/merged_df-type-3.5-full.RData", verbose = T)
old <- merged_dfx
old <- old %>% rename(merged_type = type)
# old <- old %>% select(merged_type, ai_freq) %>% replace_na(list(ai_freq = 0)) # not yet
old <- old %>% select(merged_type, ai_freq) 
# summary(old$ai_freq)
summary(old)


# new ----

# all data from tm_ben.Rmd
all_data <- read_csv(file = "data-processed/all_data.csv")
all_wide <- all_data %>% pivot_wider(id_cols = word, names_from = Source, values_from = c(count))


# join childlex ----

# loaded from excel in tm_ben.Rmd
load(file = "data-processed/childLexAgg.RData", verbose = T)
tibble(childLexAgg)
childLexAgg$chcount1 <- ((childLexAgg$childlex_norm * 9850786) / 1000000) - 1
childLexAgg$chcount2 <- ((childLexAgg$childlex_norm * 9850786) / 1000000)
# number taken from paper
childLexAgg$chcount3 <- ((childLexAgg$childlex_norm * 7352941) / 1000000) - 1
childLexAgg$chcount4 <- ((childLexAgg$childlex_norm * 7352941) / 1000000) 
# number taken from original childlex corpus

childLexAgg$chcount <- childLexAgg$chcount4

# summary(childLexAgg$chcount3) # assuming they did  count + 1
# summary(childLexAgg$chcount4) # assuming they did not do count + 1
# min(childLexAgg$childlex_norm * 7.352941)

chlex <- childLexAgg %>% select(type, chcount)
all_wide_ch <- full_join(chlex, all_wide, join_by(type == word)) # used for plotting


# join old ----

all_wide_ch_old <- full_join(old, all_wide_ch, join_by(merged_type == type)) # used for plotting


# join subtlex ----

# cleaned up version
# if cap and non-cap words exist, only the most frequent has an entry
# converted to utf8 with notepad++
du <- read_delim(file = "data-original/SUBTLEX-DE_cleaned_with_Google00.txt", 
								 delim = "\t", 
								 locale = locale(decimal_mark = ",")) 

# du2 <- read_xlsx(path = "data-original/SUBTLEX-DE cleaned version with Zipf values.xlsx", 
# 								sheet = 1)

# du$Word[du$Word == "Brücke"]
# du$Word[du$Word == "brücke"]
# du$Word[du$Word == "Obstbaum"]
# du$Word[du$Word == "obstbaum"] 

sum_subtlex_norm <- sum(du$SUBTLEX, na.rm = T) # 984,689.1
sum_raw_subtlex <- sum(du$CUMfreqcount, na.rm = T) # 25,006,561 # case independent WF

# lgSUBTLEX: log10(CUMfreqcount+1)
# SUBTLEX: CUMfreqcount/scaling_factor

scaling_factor <- 25.399 # official scaling factor

# du$subtlex_norm <- du$CUMfreqcount / subtlex_value

# dus <- du %>% rename(merged_type = Word) %>% select(Google00pm, SUBTLEX, merged_type)
du <- du %>% select(CUMfreqcount, Word) %>% 
	rename(subtlex = CUMfreqcount)
	# mutate(norm_log_subtlex = log(1 + norm_subtlex)) %>%
	# mutate(subtlex = (norm_subtlex * sum_subtlex_norm) / 1000000) %>% # wrong
	# mutate(subtlex = (norm_subtlex * sum_raw_subtlex) / 1000000) %>%
	# mutate(subtlex = (norm_subtlex * scaling_factor) ) %>%
	# select(!norm_subtlex)

joined <- full_join(all_wide_ch_old, du, join_by(merged_type == Word)) # used for plotting


# DO NOT collapse all to lower ---- 

# joined <- joined %>%
# 	mutate(merged_type_lower = tolower(merged_type))

summary(joined) # many NAs


# Replace all NA values in specific columns ----

joined3 <- joined %>% mutate(across(c(
	ai_freq, chcount, adlt, adht, chlt, chht, subtlex), ~replace_na(., 0)))
# all_wide_ch_old


# longer and group_by word----

all_longer <- joined3 %>%
	pivot_longer(
		cols = c(ai_freq, chcount, adlt, adht, chlt, chht, subtlex),  # Adjust the cols argument as needed
		names_to = "group")
# all_longer
# save(all_longer, file = "data-processed/all_longer.RData")

sums <- all_longer %>% group_by(group) %>% 
	summarize(sum = sum(value))
sums

# all_longer <- all_longer %>% filter(value > 10)


# # Create a frequency table of words
# word_freq <- table(all_longer$merged_type_lower)
# 
# # Convert the table to a data frame for easier manipulation
# word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
# 
# # Rename the columns for clarity
# colnames(word_freq_df) <- c("Word", "Frequency")
# 
# # Sort the data frame by frequency in descending order
# word_freq_df <- word_freq_df %>%
# 	arrange(desc(Frequency))
# 
# # Print the top 10 most frequent words
# print(head(word_freq_df, 10))
# 
# word_freq_summary <- word_freq_df %>%
# 	group_by(Word) %>%
# 	summarize(Total_Frequency = sum(Frequency)) %>%
# 	arrange(desc(Total_Frequency))

#
all_longer2 <- all_longer %>% 
	# group_by(merged_type_lower, group) %>% 
	group_by(merged_type, group) %>% 
	summarise(value_sum = sum(value)) %>%
	rename(value = value_sum)
# all_longer2


# LAPLACE TRANSFORMATION ----
# log(1 + x) - missing values are still zero

# all_longerl <- all_longer %>% group_by(group) %>% 
all_longerl <- all_longer2 %>% group_by(group) %>% 
	mutate(norm = 1000000 * value / (sum(value))) %>%
	mutate(normplus1 = 1000000 * (value + 1) / (sum(value))) %>%
	mutate(normplus1prob = (value + 1) / (sum(value) + 2)) %>%
	mutate(norm_log = log(1 + norm)) %>% 
	mutate(norm_logcountplus1 = log( normplus1)) %>% 
	mutate(norm_logcountplus1prob = -log( normplus1prob)) %>%
	mutate(norm_lognonorm = log( 1 + value ))
summary(all_longerl)

sums2 <- all_longer2 %>% group_by(group) %>% 
	summarize(sum = sum(value))
sums2

all_wider_againl <- all_longerl %>%
	pivot_wider(
		id_cols = merged_type,
		# id_cols = merged_type_lower,
		names_from = group,
		values_from = c(value, 
										norm, 
										# normplus1, 
										norm_log, 
										norm_logcountplus1prob,
										norm_logcountplus1,
										norm_lognonorm))

summary(all_wider_againl)
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_lower.RData")
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_ch_plus1_prob.RData")
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_ch_plus1.RData")
# updated subtlex, childlex error, and plus1 variables

# load("data-processed/all_wider_again_subt_ch_plus1_prob.RData", verbose = T)


## start plotting ----

# dont use this anymore
# sel_colsl <- all_wider_againl %>%
	# select(contains("norm_log_"))

sel_colsl <- all_wider_againl %>%
	select(contains("norm_logcountplus1_"))

# sel_colsl <- all_wider_againl %>%
# 	select(contains("norm_"))

# sel_cols <- all_wider_again %>% 
# 	select(contains("value_"))

# sel_colsl <- all_wider_againl %>%
# 	select(norm_adlt, norm_adht, norm_chlt, norm_chht, norm_ai_freq, norm_chcount)
# 

correlation_matrix <- cor(sel_colsl, use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix

# check corrlelation when 0s are removed
# sel_colslno0 <- sel_colsl %>%
# 	mutate(across(everything(), ~na_if(., 0)))

all_wider_againl00 <- all_wider_againl %>%
	mutate(across(starts_with("value_"), ~na_if(., 0)))

# test
# all_wider_againl00$norm_logcountplus1_adht <- all_wider_againl00$norm_logcountplus1_adht[is.na(
	# all_wider_againl00$value_adht)] <- NA


# Get the names of value_ and norm_logcountplus1_ columns
# value_cols <- names(all_wider_againl00)[str_starts(names(all_wider_againl00), "value_")]
norm_cols <- names(all_wider_againl00)[str_starts(names(all_wider_againl00), "norm_logcountplus1_")]

# Loop through each norm_logcountplus1_ column
for (norm_col in norm_cols) {
	# Construct the corresponding value_ column name
	value_col <- str_replace(norm_col, "norm_logcountplus1_", "value_")
	
	# Check if the value column exists in the dataframe
	if (value_col %in% names(all_wider_againl00)) {
		# Update the norm_logcountplus1_ column based on the value_ column
		all_wider_againl00[[norm_col]][is.na(all_wider_againl00[[value_col]])] <- NA
	}
}
sel_colslno0 <- all_wider_againl00 %>%
	select(contains("norm_logcountplus1_"))
sel_colslno0


# summary(sel_colsl)
# table(sel_colsl$norm_log_chlt)
# table(sel_colsl$norm_log_chht)
# summary(sel_colslno0)

correlation_matrixno0 <- cor(sel_colslno0, use = "pairwise.complete.obs")
correlation_matrixno0 <- round(correlation_matrixno0, 2)
correlation_matrixno0


# Create empty matrices to store the values you want to extract
n_matrix <- matrix(NA, nrow = ncol(sel_colslno0), ncol = ncol(sel_colslno0))
colnames(n_matrix) <- colnames(sel_colslno0)
rownames(n_matrix) <- colnames(sel_colslno0)

# Loop through each pair of columns and perform cor.test() if both columns are numeric
for(i in 1:ncol(sel_colslno0)) {
	for(j in 1:ncol(sel_colslno0)) {
		# Extract columns as numeric vectors
		col_i <- as.numeric(sel_colslno0[[i]])
		col_j <- as.numeric(sel_colslno0[[j]])
		
		# Check if both columns are numeric
		if (i != j) {
			test_result <- cor.test(col_i, col_j, use = "pairwise.complete.obs")
			# Extract the number of observations used (this is in the 'parameter' field, add 2 for the degrees of freedom)
			n_matrix[i, j] <- test_result$parameter + 2
		} else {
			n_matrix[i, j] <- NA  # Or set it to some other meaningful value
		}
	}
}

# View the matrix with the number of observations used for each correlation
n_matrix

# Convert n_matrix to a data frame for manipulation
n_matrix_df <- as.data.frame(n_matrix)

# # Function to format numbers
# format_number <- function(x) {
# 	if (is.na(x)) return(NA)
#   if (x >= 1e3) {
# 		return(paste0(round(x / 1e3, 1), "k"))
# 	} else {
# 		return(as.character(x))
# 	}
# }
# 
# formatted_n_matrix_df <- n_matrix_df %>%
# 	mutate(across(everything(), ~ sapply(., format_number)))
formatted_n_matrix_df <- n_matrix_df

# Convert the formatted dataframe back to a matrix
# formatted_n_matrix <- as.matrix(formatted_n_matrix_df)

upper.tri(n_matrix)
upper.tri(correlation_matrix)

correlation_matrix2 <- correlation_matrixno0
correlation_matrix2[upper.tri(correlation_matrix2)] <- formatted_n_matrix_df[upper.tri(formatted_n_matrix_df)]

# # Create upper and lower triangle masks
# upper_triangle <- correlation_matrixno0[upper.tri(correlation_matrixno0)]
# lower_triangle <- formatted_n_matrix_df[lower.tri(formatted_n_matrix_df)]

# correlation_matrix <- correlation_matrix # original based on all data
correlation_matrix <- correlation_matrix2 # based on no 0s and with n
correlation_matrix <- correlation_matrixno0 # based on no 0s



## plot ----


descriptive_labels <- c(
	# "DWDS" = "norm_log_dwds",
	# "SUBTLEX" = "norm_logcountplus1_subtlex",
	"ChildLex" = "norm_logcountplus1_chcount",
	"Adult low temp." = "norm_logcountplus1_adlt",	
	"Adult high temp." = "norm_logcountplus1_adht",
	"Child low temp." = "norm_logcountplus1_chlt",
	"Child high temp." = "norm_logcountplus1_chht",
	"Exp. 1" = "norm_logcountplus1_ai_freq"
)

print(correlation_matrix)

# Melt the correlation matrix for plotting
melted_cor <- melt(correlation_matrix)
colnames(melted_cor) <- c("Row", "Column", "Percentage")
# melted_cor$Percentage <- round(melted_cor$Percentage, 2)
melted_cor$Percentage <- as.numeric(as.character(melted_cor$Percentage))

melted_cor <- melted_cor %>%
	mutate(
		Row = fct_recode(as.factor(Row), !!!descriptive_labels),
		Column = fct_recode(as.factor(Column), !!!descriptive_labels)
	)

melted_cor$Row <- factor(melted_cor$Row, levels = names(descriptive_labels))
melted_cor$Column <- factor(melted_cor$Column, levels = names(descriptive_labels))

melted_cor <- melted_cor %>%
	filter(Column != "SUBTLEX",
				 Row != "SUBTLEX")

melted_cor_upper <- melted_cor %>%
	filter(as.numeric(Row) <= as.numeric(Column)) %>%
	filter(Percentage != 1)
	# filter(!is.na(Percentage))

cormat <- ggplot(melted_cor_upper, aes(Row, Column, fill = Percentage)) +
	geom_tile() + # This adds the fill to the plot
	geom_text(aes(label = sprintf("%.2f", Percentage)), color = "black", size = 2.5) + # Adds text labels
	# geom_text(aes(label = Percentage), color = "black", size = 2.5) + # Adds text labels
	# scale_fill_viridis(begin = .5, end = 1, name = "Pearson\nCorrelation") +  # Color gradient
	scale_fill_viridis(begin = .5, end = 1, name = "Pearson\nCorrelation",
										 breaks = seq(0.60, 1, by = 0.1)) + # Set breaks to avoid overlap
										 # guide = guide_colorbar(barwidth = 10, barheight = 1.5, 
										 											 # title.position = "top", title.hjust = 0.5)) +  # Adjust guide size and title position
	# scale_fill_gradient2(low = "blue", high = "red", mid = "white",
	# 										 midpoint = 0.75, limit = c(0.5, 1), space = "Lab",
	# 										 name="Pearson\nCorrelation") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
				axis.title.x = element_blank(),
				axis.title.y = element_blank(),
				axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
				legend.position = "top") +
	coord_fixed()
cormat



print(n_matrix)

# Melt the correlation matrix for plotting
melted_cor <- melt(n_matrix)
colnames(melted_cor) <- c("Row", "Column", "Percentage")
# melted_cor$Percentage <- round(melted_cor$Percentage, 2)
melted_cor$Percentage <- as.numeric(as.character(melted_cor$Percentage))

melted_cor <- melted_cor %>%
	mutate(
		Row = fct_recode(as.factor(Row), !!!descriptive_labels),
		Column = fct_recode(as.factor(Column), !!!descriptive_labels)
	)

melted_cor$Row <- factor(melted_cor$Row, levels = names(descriptive_labels))
melted_cor$Column <- factor(melted_cor$Column, levels = names(descriptive_labels))

melted_cor <- melted_cor %>%
	filter(Column != "SUBTLEX",
				 Row != "SUBTLEX")


melted_cor_upper <- melted_cor %>%
	filter(as.numeric(Row) <= as.numeric(Column)) %>%
	# filter(Percentage != 1)
	filter(!is.na(Percentage))

cormatc <- ggplot(melted_cor_upper, aes(Row, Column, fill = Percentage)) +
	geom_tile() + # This adds the fill to the plot
	# geom_text(aes(label = sprintf("%.2f", Percentage)), color = "black", size = 2.5) + # Adds text labels
	geom_text(aes(label = Percentage), color = "black", size = 2.5) + # Adds text labels
	# geom_text(aes(label = Percentage), color = "black", size = 2.5) + # Adds text labels
	scale_fill_viridis(begin = .5, end = 1, name = "Shared\nwords") +  # Color gradient
	
	# scale_fill_gradient2(low = "blue", high = "red", mid = "white",
	# 										 midpoint = 0.75, limit = c(0.5, 1), space = "Lab",
	# 										 name="Pearson\nCorrelation") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
				axis.title.x = element_blank(),
				axis.title.y = element_blank(),
				axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
				legend.position = "top") +
	coord_fixed()
cormatc

# Assuming 'melted_cor' is a data frame, create row and column index variables


# ggsave(plot = cormat, file = "figures/correlation_matrix_s2_plus1.pdf", width = 6, height = 5)
ggsave(plot = cormat, file = "figures/correlation_matrixc.pdf", width = 6, height = 5)


library(patchwork)
combined_patchwork <- cormat + cormatc + 
	plot_layout(ncol = 2) +
	plot_annotation(title = "", tag_levels = 'A') 
combined_patchwork

# ggsave(combined_patchwork, filename = "figures/combined_plot.pdf", width = 18, height = 10, dpi = 300)
ggsave(combined_patchwork, filename = "figures/combined_plotc.pdf", width = 8, height = 5, dpi = 300)

## add corpus sizes in the middle


# devel ----

load("data-original/devel/DeveL_Words.RData", verbose = T) # DeveL Dataset

result <- ld.rt %>% 
	left_join(nam.on, by = "word") %>% 
	left_join(ld.acc, by = "word") %>% 
	left_join(nam.dur, by = "word") %>% 
	left_join(nam.acc, by = "word") %>%
	left_join(item, by = "word")
colnames(result)

# result
# result <- result %>% mutate(rt.avg.m = rowMeans(across(c(rt.g1.m, rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m)), na.rm = T))
result <- result %>% mutate(rt.avg.m = rowMeans(across(c(rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m)), na.rm = T))
summary(result)
# sfreq
df <- result %>% 	
	select(contains("m") & contains("rt") | 
				 	word | child.type.freq | dwds.type.freq | child.old20 | aoa | 
				 	letter.cnt | unigram | bigram | trigram | sfreq) %>%
	select(where(~ !is.character(.)) | word)
tibble(df)
summary(df)

# a <- df %>% select(word, child.type.freq, dwds.type.freq)
# tibble(a)

# save(df, file = "data-processed/devel-avg.RData")
# load(file = "data-processed/devel-avg.RData", verbose = T)

# summary(df)
# summary(df$child.type.freq)
# df$child.type.freq2 = df$child.type.freq * 7.352941
# df$child.type.freq2[df$word == "Obstbaum"]
# df$child.type.freq2 = df$child.type.freq * 9.850786
# summary(df$child.type.freq2)

df <- df %>% 
	# mutate(norm_log_dwds = log(1 + dwds.type.freq)) %>%
	mutate(norm_logcountplus1_dwds = log(dwds.type.freq)) %>%
	# select(rt.avg.m, rt.g1.m, rt.g2.m, word, norm_log_dwds)
	select(rt.avg.m, rt.g1.m, rt.g2.m, word, norm_logcountplus1_dwds, 
				 rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m,
				 rt.ya.m, rt.oa.m, 
				 # sfreq, # subjective frequency
				 child.old20, aoa, letter.cnt, unigram, bigram, trigram)
# tibble(df)
# summary(df$dwds.type.freq)
# summary(df$norm_logcountplus1_dwds)
# summary(df$norm_log_dwds)

# df <- df %>%
	# mutate(word_lower = tolower(word))
# tibble(df)

# # Credf_l# # Create a frequency table of words
# word_freq <- table(df$word_lower)
# 
# # Convert the table to a data frame for easier manipulation
# word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
# 
# # Rename the columns for clarity
# colnames(word_freq_df) <- c("Word", "Frequency")
# 
# # Sort the data frame by frequency in descending order
# word_freq_df <- word_freq_df %>%
# 	arrange(desc(Frequency))
# 
# # Print the top 10 most frequent words
# print(head(word_freq_df, 10))

## --> so no capitalization issues

# tibble(df)


# join with all_wider_againl from above ---- 

# load("data-processed/all_wider_again_subt.RData", verbose = T)
# load("data-processed/all_wider_again_subt_ch_plus1.RData", verbose = T)
load("data-processed/all_wider_again_subt_ch_plus1_prob.RData", verbose = T)
summary(all_wider_againl$norm_chcount)
b <- all_wider_againl %>% 
	filter(norm_chcount != 0)
summary(b$norm_chcount)
length(b$norm_chcount)
aap <- b %>% filter(norm_chcount < 10)
100 * (nrow(aap)) / nrow(b)


all_wider_againls <- all_wider_againl %>% select(!contains("subtlex"))
# colnames(all_wider_againls)

# Remove rows where all columns starting with 'value_' are 0
all_wider_againls2 <- all_wider_againls %>%
	filter(!rowSums(select(., starts_with("value_")) == 0) == length(select(., starts_with("value_"))))

devel1 <- left_join(df, all_wider_againls, join_by(word == merged_type))

# add subtlex
du2 <- du %>%
	mutate(Word = tolower(Word))
devel12 <- devel1 %>%
	mutate(word = tolower(word))

devel13 <- left_join(devel12, du2, join_by(word == Word))
summary(devel13)
summary(du2)

# all_longerl <- all_longer %>% group_by(group) %>% 
devel13 <- devel13 %>% 
	mutate(across(subtlex, ~replace_na(., 0))) %>%
	rename(value_subtlex = subtlex) %>%
	mutate(norm_subtlex = 1000000 * value_subtlex / (sum(value_subtlex))) %>%
	mutate(normplus1_subtlex = 1000000 * (value_subtlex + 1) / (sum(value_subtlex))) %>%
	mutate(normplus1prob_subtlex = (value_subtlex + 1) / (sum(value_subtlex) + 2)) %>%
	mutate(norm_log_subtlex = log(1 + norm_subtlex)) %>% 
	mutate(norm_logcountplus1_subtlex = log( normplus1_subtlex)) %>% 
	mutate(norm_logcountplus1prob_subtlex = -log( normplus1prob_subtlex)) %>%
	mutate(norm_lognonorm_subtlex = log( 1 + value_subtlex ))
summary(devel13)


aap <- devel13 %>% filter(norm_chcount > 5)
aap <- devel13 %>% filter(norm_chcount < 10)
aap <- devel13 %>% filter(norm_chcount < 5)
nrow(aap)
(nrow(devel13) - nrow(aap)) / nrow(devel13)

aap <- devel13 %>% filter(norm_chcount < 10)
100 * (nrow(aap)) / nrow(devel13)

aap <- devel13 %>% filter(value_chcount > 10)
nrow(devel13) - nrow(aap)


# a <- devel1 %>% select(word, norm_log_chcount, norm_log_ai_freq)
# tibble(a)

# load("data-processed/all_wider_again_subt_lower.RData", verbose = T)
# devel1 <- left_join(df, all_wider_againl, join_by(word_lower == merged_type_lower))
# b <- devel1 %>% select(word_lower, norm_log_chcount, norm_log_ai_freq)
# tibble(b)

# cor.test(devel1$norm_log_subtlex, devel1$sfreq)
# cor.test(devel1$norm_log_dwds, devel1$sfreq)
# cor.test(devel1$norm_log_chcount, devel1$norm_log_ai_freq) # .76 for both lower and not lower
# tibble(devel1)
# tibble(df)
# tibble(all_wider_againl)


# save ----

# save(devel1, file = "data-processed/devel-avg-merged.RData")
# save(devel1, file = "data-processed/devel-avg-merged.RData")
# save(devel1, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars.RData")
save(devel13, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData")
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)
# save(devel1, file = "data-processed/devel-avg-merged-lower.RData")
tibble(devel13)
summary(devel13)

# summary(devel1$norm_log_subtlex)
# summary(devel1)


## start plotting ----

sel_colsl <- devel1 %>%
	# select(contains("norm_log_") | rt.g1.m | rt.g2.m | rt.avg.m)
	# select(contains("norm_log_") |logplus1 rt.g1.m | rt.g2.m | rt.avg.m | 
	select(contains("norm_logcountplus1_") | 
				 	rt.g1.m | rt.g2.m | rt.avg.m | # norm_log_dwds |
				 	rt.g2.m | rt.g3.m | rt.g4.m | rt.g6.m | 
				 	rt.ya.m | rt.oa.m |
				 	child.old20 | aoa | letter.cnt | unigram | bigram | trigram)
	# select(contains("norm_log_") | rt.g1.m | rt.g2.m)

summary(sel_colsl)

correlation_matrix <- cor(sel_colsl, use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix

sel_colslno0 <- sel_colsl %>%
	mutate(across(everything(), ~na_if(., 0)))
# summary(sel_colsl)
# table(sel_colsl$norm_log_chlt)
# table(sel_colsl$norm_log_chht)
# summary(sel_colslno0)

correlation_matrixno0 <- cor(sel_colslno0, use = "pairwise.complete.obs")
correlation_matrixno0 <- round(correlation_matrixno0, 2)
correlation_matrixno0


## plot cormat ----

melted_cor <- melt(correlation_matrix)
colnames(melted_cor) <- c("Row", "Column", "Percentage")
melted_cor$Percentage <- round(melted_cor$Percentage, 2)
# 
# descriptive_labels <- c(
# 	"SUBTLEX" = "norm_log_subtlex",
# 	"DWDS" = "norm_log_dwds",
# 	"ChildLex" = "norm_log_chcount",
# 	"OLD20" = "child.old20",
# 	"AoA" = "aoa",
# 	"Letter Count" = "letter.cnt",
# 	"Unigram" = "unigram",
# 	"Bigram" = "bigram",
# 	"Trigram" = "trigram",
# 	"Adult low temp." = "norm_log_adlt",	
# 	"Adult high temp." = "norm_log_adht",
# 	"Child low temp." = "norm_log_chlt",
# 	"Child high temp." = "norm_log_chht",
# 	"Exp. 1" = "norm_log_ai_freq",
# 	"Grade 1 RT" = "rt.g1.m",
# 	"Grade 2 RT" = "rt.g2.m",
# 	"Grade 3 RT" = "rt.g3.m",
# 	"Grade 4 RT" = "rt.g4.m",
# 	"Grade 6 RT" = "rt.g6.m",
# 	"Average child RT" = "rt.avg.m",
# 	"YA RT" = "rt.ya.m",
# 	"OA RT" = "rt.oa.m"
# )

descriptive_labels <- c(
	"SUBTLEX" = "norm_logcountplus1_subtlex",
	"DWDS" = "norm_logcountplus1_dwds",
	"ChildLex" = "norm_logcountplus1_chcount",
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

# Recode factor levels in Row and Column based on descriptive_labels
# !!! splices the key-value pairs from the descriptive_labels list into fct_recode
melted_cor <- melted_cor %>%
	mutate(
		Row = fct_recode(as.factor(Row), !!!descriptive_labels),
		Column = fct_recode(as.factor(Column), !!!descriptive_labels)
	)

# Explicitly set the factor levels of Row and Column to match the order of descriptive_labels
melted_cor$Column <- factor(melted_cor$Column, levels = names(descriptive_labels))
melted_cor$Row <- factor(melted_cor$Row, levels = names(descriptive_labels))

cormat_devel <- ggplot(melted_cor, aes(Row, Column, fill = Percentage)) +
	geom_tile() + # This adds the fill to the plot
	geom_text(aes(label = sprintf("%.2f", Percentage)), color = "black", size = 2) + # Adds text labels
	scale_fill_viridis(begin = .5, end = 1, name = "Pearson\nCorrelation") +  # Color gradient
	# scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
	# 										 midpoint = 0.75, limit = c(0.5, 1), space = "Lab", 
	# 										 name="Pearson\nCorrelation") +
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
ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_supd_plus1-prob.pdf", width = 9, height = 8)
# ggsave(plot = cormat_devel, file = "figures/correlation_matrix_devel_s_lower.pdf", width = 9, height = 8)

# bw = 20
bw = .01
bw = .05
n_obs = length(devel1$rt.avg.m)



# plot transformations -----

# choose panels
# tibble(df_long)
# tibble(devel1)

df_long <- devel1 %>%
	select(word, 
				 contains("value_"), 
				 contains("norm_log_"), 
				 contains("norm_logcountplus1"),
				 contains("norm_logcountplus1prob"),
				 contains("norm_lognonorm")) %>%
	pivot_longer(cols = !word, names_to = "var_name", values_to = "measure")

# Transform the data frame
df_transformed <- df_long %>%
	mutate(
		category = case_when(
			startsWith(var_name, "value_") ~ "value_",
			startsWith(var_name, "norm_lognonorm_") ~ "norm_lognonorm_",
			startsWith(var_name, "norm_logcountplus1prob_") ~ "norm_logcountplus1prob_",
			startsWith(var_name, "norm_logcountplus1_") ~ "norm_logcountplus1_",
			startsWith(var_name, "norm_log_") ~ "norm_log_"
		),
		remainder = case_when(
			category == "value_" ~ str_remove(var_name, "value_"),
			category == "norm_lognonorm_" ~ str_remove(var_name, "norm_lognonorm_"),
			category == "norm_logcountplus1prob_" ~ str_remove(var_name, "norm_logcountplus1prob_"),
			category == "norm_logcountplus1_" ~ str_remove(var_name, "norm_logcountplus1_"),
			category == "norm_log_" ~ str_remove(var_name, "norm_log_")
		)
	) %>% 
	select(-var_name) %>%
	filter(remainder != "dwds")

# all_longerl <- all_longer %>% group_by(group) %>% 
# all_longerl <- all_longer2 %>% group_by(group) %>% 
# 	mutate(norm = 1000000 * value / (sum(value))) %>%
# 	mutate(normplus1 = 1000000 * (value + 1) / (sum(value))) %>%
# 	mutate(normplus1prob = (value + 1) / (sum(value) + 2)) %>%
# 	mutate(norm_log = log(1 + norm)) %>% 
# 	mutate(norm_logcountplus1 = log( normplus1)) %>% 
# 	mutate(norm_logcountplus1prob = -log( normplus1prob)) %>%
# 	mutate(norm_lognonorm = log( 1 + value ))
# summary(all_longerl)

custom_labelscolor <- c(
	"norm_log_" = expression(log(1 + frequency * "*" * 10^6 / corpus_size)),
	"norm_logcountplus1_" = expression(log((1 + frequency) * "*" * 10^6 / corpus_size)),
	"norm_logcountplus1prob_" = expression(-log((1 + frequency) * "*" * 1 / corpus_size)),
	"norm_lognonorm_" = expression(log(1 + frequency)))

custom_labelscolor2 <- c(
	"norm_log_" = "log(1 + frequency * 10^6 / corpus_size)",
	"norm_logcountplus1_" = "log((1 + frequency) * 10^6 / corpus_size)",
	"norm_logcountplus1prob_" = "-log((1 + frequency) * 1 / corpus_size)",
	"norm_lognonorm_" = "log(1 + frequency))")

custom_labelsfacets <- c(
	"dwds" = "DWDS",
	"adht" = "Adults High Temp.",
	"adlt" = "Adults Low Temp.",
	"ai_freq" = "Exp 1",
	"chcount" = "childLex",
	"chht" = "Children High Temp.",
	"chlt" = "Children Low Temp.",
	"subtlex" = "SUBTLEX"
)

# pivot wider such that value becomes a variable that i can use for a scatter plot

# Specify order of facets
desired_order_facets <- c("ai_freq", "adht", "adlt", "chht", "chlt", "chcount", "subtlex")
desired_order_trans <- c("norm_logcountplus1prob_", "norm_lognonorm_", "norm_log_", "norm_logcountplus1_")


## hist plot  ---- 


bw <- 0.2  # Adjust as necessary
df_transformedv <- df_transformed %>% filter(category != "value_") %>% as_tibble()

df_transformedv$category <- factor(df_transformedv$category, levels = desired_order_trans)
# df_transformedv$remainder <- factor(df_transformedv$remainder)
df_transformedv$remainder <- factor(df_transformedv$remainder, levels = desired_order_facets)

histogram_plot <- ggplot(df_transformedv, aes(x = measure)) +
	geom_histogram(aes(y = after_stat(density)), binwidth = bw, alpha = 0.5, position = "identity") +
	facet_grid(category ~ remainder, scales = "free",
						 labeller = labeller(category = custom_labelscolor2, 
						 										remainder = custom_labelsfacets)) +
	theme_minimal() +
	xlab("Transformed Frequency") +
	ylab("Density") +
	theme(strip.text.y = element_text(angle = 0))
# histogram_plot
ggsave(histogram_plot, file = "figures/histogram_plot_devel.pdf", width = 14, height = 8)

# generate a figure caption
histogram_plot_caption <- "Histograms of transformed frequency values. The x-axis shows the transformed frequency values, and the y-axis shows the density of the values. The facets show the different transformations and the different corpora. The transformations are: log(1 + frequency * 10^6 / corpus_size), log((1 + frequency) * 10^6 / corpus_size), -log((1 + frequency) * 1 / corpus_size), and log(1 + frequency). The corpora are: DWDS, Adults High Temp., Adults Low Temp., Children High Temp., Children Low Temp., childLex, and SUBTLEX."

## scatter plot  ---- 

df_transformed2 <- df_transformed %>%
	pivot_wider(names_from = category, values_from = measure)

df_transformed3 <- df_transformed2 %>% 
	pivot_longer(cols = c("norm_log_", "norm_logcountplus1_", "norm_logcountplus1prob_", "norm_lognonorm_"), 
							 names_to = "category", values_to = "yvalue_")

df_transformed4 <- df_transformed3 %>%
	filter(category == "norm_log_" | 
				 	category == "norm_logcountplus1_" | 
				 	category == "norm_logcountplus1prob_" |
				 	category == "norm_lognonorm_")


# Modify the factor levels of remainder to control facet order
df_transformed4$remainder <- factor(df_transformed4$remainder, levels = desired_order_facets)

df_transformed4$category <- factor(df_transformed4$category, 
																	 levels = c("norm_logcountplus1prob_", 
																	 					 "norm_lognonorm_", 
																	 					 "norm_log_", 
																	 					 "norm_logcountplus1_"))

scatter_plot <- ggplot(df_transformed4, aes(x = value_, y = yvalue_, color = category)) +
	geom_point(size = .5) +
	geom_line() +
	facet_grid( ~ remainder, scales = "free", 
							labeller = labeller(remainder = custom_labelsfacets)) +
	scale_color_discrete(name = "Transformation Type",  
											 labels = custom_labelscolor) + 
	scale_x_continuous(limits = c(0, 50)) +
	# add axes labels
	xlab("Frequency") +
	ylab("Transformed Frequency") +
	theme_minimal() +
	theme(strip.text.y = element_text(angle = 0),
				legend.position = "bottom")
scatter_plot
ggsave(scatter_plot, file = "figures/scatter_plot_devel.pdf", width = 11, height = 5)


## scatter plot zoom out ---- 



# df_transformed5 <- df_transformed4 %>%
# 	# mutate only for category is norm_logcountplus1_ using case when
# 	mutate(value_ = case_when(
# 		category == "norm_logcountplus1_" ~ value_ + 1000,
# 		category == "norm_lognonorm_" ~ value_ - 1000,
# 		TRUE ~ value_
# 	))


scatter_plot2 <- ggplot(df_transformed4, aes(x = value_, y = yvalue_, color = category)) +
	geom_point(size = .5) +
	geom_line() +
	# geom_jitter(width = 300, height = 0) +  # Add horizontal jitter with controlled width
	facet_grid( ~ remainder, scales = "free", 
							labeller = labeller(remainder = custom_labelsfacets)) +
	scale_color_discrete(name = "Transformation Type",  
											 labels = custom_labelscolor) +  
	xlab("Frequency") +
	ylab("Transformed Frequency") +
	theme_minimal() +
	theme(strip.text.y = element_text(angle = 0),
				legend.position = "bottom")
# scatter_plot2
ggsave(scatter_plot2, file = "figures/scatter_plot2_devel.pdf", width = 11, height = 5)

# generate a caption
scatter_plot_caption <- "Scatter plot of transformed frequency values. The x-axis shows the frequency values, and the y-axis shows the transformed frequency values. The facets show the different transformations and the different corpora. The transformations are: log(1 + frequency * 10^6 / corpus_size), log((1 + frequency) * 10^6 / corpus_size), -log((1 + frequency) * 1 / corpus_size), and log(1 + frequency). The corpora are: DWDS, Adults High Temp., Adults Low Temp., Children High Temp., Children Low Temp., childLex, and SUBTLEX."


# Differences between cor mats ----

# # Calculate the Frobenius norm of the difference
# frobenius_norm <- norm(correlation_matrix - correlation_matrixno0, type = "F")
# frobenius_norm
# 
# # Conduct a test for significant difference using the Jennrich Test
# library(psych)
# jennrich_result <- cortest.jennrich(correlation_matrix, correlation_matrixno0, n1 = 300000, n2 = 300000) # Assuming sample size 100
# jennrich_result
# 
# # Difference Heatmap
# diff_matrix <- correlation_matrix - correlation_matrixno0
# library(reshape2)
# diff_melted <- melt(diff_matrix)
# 
# ggplot(diff_melted, aes(Var1, Var2, fill = value)) +
# 	geom_tile() +
# 	scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.1, 0.1), name = "Difference") +
# 	theme_minimal() +
# 	ggtitle("Difference Heatmap between Matrix 1 and Matrix 2") +
# 	xlab("") +
# 	ylab("") +
# 	theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Dendrogram visualization
# hc1 <- hclust(as.dist(1 - correlation_matrix))
# hc2 <- hclust(as.dist(1 - correlation_matrixno0))
# par(mfrow = c(1, 2))
# plot(hc1, main = "Dendrogram for Matrix 1")
# plot(hc2, main = "Dendrogram for Matrix 2")
# par(mfrow = c(1, 1))

# dendrogram is the same, but the correlation matrix is different
