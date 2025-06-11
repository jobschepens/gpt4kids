library(dplyr)
library(quanteda)
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

# This script is used to create a correlation matrix between different frequency measures. 

load("data-processed/all_wider_again_exp3_all.RData", verbose = T)
load("data-processed/all_wider_again_exp3-2_all.RData", verbose = T)
colnames(all_wider_againl)

# compute correlation matrix ----

# dont use this anymore
# sel_colsl <- all_wider_againl %>%
	# select(contains("norm_log_"))

sel_colsl <- all_wider_againl %>%
	select(contains("norm_logcountplus1_"))
colnames(sel_colsl)

# sel_colsl <- all_wider_againl %>%
# 	select(contains("norm_"))

# sel_cols <- all_wider_again %>% 
# 	select(contains("value_"))

# sel_colsl <- all_wider_againl %>%
# 	select(norm_adlt, norm_adht, norm_chlt, norm_chht, norm_ai_freq, norm_chcount)

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



# correlation matrix plot ----


descriptive_labels <- c(
	# "DWDS" = "norm_log_dwds",
	"SUBTLEX" = "norm_logcountplus1_subtlex",
	"ChildLex" = "norm_logcountplus1_chcount",
	# "Adult low temp." = "norm_logcountplus1_adlt",	
	# "Adult high temp." = "norm_logcountplus1_adht",
	# "Child low temp." = "norm_logcountplus1_chlt",
	# "Child high temp." = "norm_logcountplus1_chht",
	# "DeepSeek long" = "norm_logcountplus1_dslo",	
	# "DeepSeek short" = "norm_logcountplus1_dssh",
	# "LLama long" = "norm_logcountplus1_lllo",
	# "LLama short" = "norm_logcountplus1_llsh",
	"DeepSeek long" = "norm_logcountplus1_dsl2",	
	"DeepSeek short" = "norm_logcountplus1_dss2",
	"LLama long" = "norm_logcountplus1_lll2",
	"LLama short" = "norm_logcountplus1_lls2",
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

# ggsave(plot = cormat, file = "figures/correlation_matrix_s2_plus1.pdf", width = 6, height = 5)
# ggsave(plot = cormat, file = "figures/correlation_matrixc.pdf", width = 6, height = 5)
# ggsave(plot = cormat, file = "figures/correlation_matrix_exp3.pdf", width = 6, height = 5)


# shared words ----

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
	scale_fill_viridis(begin = .5, end = 1, name = "Shared\nwords", 
										 breaks = c(30000, 45000, 60000)) + 
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


# combine plots ----

library(patchwork)
combined_patchwork <- cormat + cormatc + 
	plot_layout(ncol = 2) +
	plot_annotation(title = "", tag_levels = 'A') 
combined_patchwork

# ggsave(combined_patchwork, filename = "figures/combined_plot.pdf", width = 18, height = 10, dpi = 300)
# ggsave(combined_patchwork, filename = "figures/combined_plotc.pdf", width = 8, height = 5, dpi = 300)
ggsave(combined_patchwork, filename = "figures/combined_plotc_exp3.pdf", width = 8, height = 5, dpi = 300)

## add corpus sizes in the middle