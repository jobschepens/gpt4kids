# install.packages("ggh4x")
library(ggh4x)
library(tidyverse)
library(ggrepel)
library(tm)
library(readtext)

# script to split the data into two halves and calculate the correlation between the two halves
# takes a long time to run 

# Parameters --------------------------------------------------------------

rm(list = ls())
reps <- list()

set.seed(123)
# NUMBEROFTEXTSSPERBOOK = 5
NUMBEROFTEXTSSPERBOOK = 3 
# 1500 texts for adlt = 3 texts per book
# 1500 texts for adht = 3 texts per book
# 5500 texts for chlt = 11 texts per book
# 5500 texts for chht = 11 texts per book

MAXBOOKS = 499
# MAXBOOKS = 40
SPLIT = (MAXBOOKS+1)/2
MAXREPS = 100
# MAXREPS = 2
allreps <- 1:MAXREPS


# funcs ----

kl_divergence <- function(p, q) {
	sum(p * log(p / q), na.rm = TRUE)}
js_divergence <- function(p, q) {
	m <- 0.5 * (p + q)
	0.5 * kl_divergence(p, m) + 0.5 * kl_divergence(q, m)}

getFreqMat <- function(df, opts = list()) {
	corpus <- Corpus(VectorSource(df$text))
	tdm <- as.matrix(TermDocumentMatrix(corpus, control = opts))
	feqmat <- data.frame(type = rownames(tdm), 
											 ai_freq = rowSums(tdm), 
											 row.names = NULL)
	# x <- lexicon::freq_first_names %>% slice_max(prop, n = 4000)
	# feqmat <- feqmat[!(feqmat$type %in% x$Name),]
}

opts <- list(removeNumbers = T,
						 removePunctuation = T,
						 # preserve_intra_word_contractions = TRUE,
						 # preserve_intra_word_dashes = TRUE,
						 stopwords = F,
						 stemming = FALSE, 
						 tolower=FALSE,
						 wordLengths = c(2, Inf))


# load data ----

for(i in allreps) {
	# i = 1
	print(paste0("rep: ", i))
	
	conditions = c("adlt", "adht", "chlt", "chht")
	ftables <- list()
	
	for(cond in conditions) {
		# cond = "adlt"
		print(cond)
		
		# from tm_ben
		# load(file = "data-processed/corpus-adlt.RData", verbose = T) # used for udpipe
		# load(file = "data-processed/corpus-adht.RData", verbose = T) # used for udpipe
		# load(file = "data-processed/corpus-chlt.RData", verbose = T) # used for udpipe
		# load(file = "data-processed/corpus-chht.RData", verbose = T) # used for udpipe
		load(file = paste0("data-processed/corpus-", cond, ".RData")) # used for udpipe
		# df_l <- df_l %>% 
		# 	# mutate(last_number = str_extract(doc_id, "(?<=_)\\d+(?=\\.txt$)") %>% 
		# 	mutate(last_number = str_extract(doc_id, "(?<=_)\\d+(?=_low|\\.txt$)") %>% 
		# 				 	as.numeric())
		
		df_l <- df_l %>%
			mutate(
				# Extract the number from doc_id
				extracted_number = str_extract(doc_id, "(?<=_)\\d+(?=_low|_high|\\.txt$)") %>% as.numeric(),
				
				# Adjust the number if _low is present
				# last_number = ifelse(str_detect(doc_id, "_low"), extracted_number - 1, extracted_number)
				last_number = ifelse(cond == "adlt" & str_detect(doc_id, "_low\\.txt$"), 
														 extracted_number - 1, extracted_number),
				# last_number = ifelse(str_detect(doc_id, "_high\\.txt$"), extracted_number - 1, extracted_number)
			) %>%
			select(-extracted_number)  # Remove the temporary column
		# text_types <- tibble(df_l)
		summary(df_l$last_number)
		table(df_l$last_number)
		
		# make sure all corpora have similar number of words and identical number of texts by reducing the number of texts to 1500
		# text_types <- text_types[sample(1:nrow(text_types), 1500), ]
		# text_types <- text_types[sample(1:nrow(text_types), 3), ]
		
		# Split the data frame into two subsets based on last_number
		# df_l_lt_250 <- df_l %>% filter(last_number < 250)
		# df_l_ge_250 <- df_l %>% filter(last_number >= 250)
		# length(unique(df_l_lt_250$last_number))
		# length(unique(df_l_ge_250$last_number))
		
		# Sample 250 unique rows randomly from the entire data frame
		x1 <- sample(0:MAXBOOKS, size = SPLIT, replace = FALSE, )
		x2 <- setdiff(0:MAXBOOKS, x1)
		
		table(df_l$last_number %in% x1)
		table(df_l$last_number)
		# Split the sampled data frame into two separate data frames
		df_l_set1 <- df_l %>% filter(last_number %in% x1)
		df_l_set2 <- df_l %>% filter(last_number %in% x2)
		df_l_set2$last_number
		
		# Function to check for sufficient rows and sample 5 rows
		sample_from_groups <- function(df) {
			# Check if all groups have at least 5 rows
			row_counts <- df %>%
				# row_counts <- df_l_set2 %>%
				group_by(last_number) %>%
				summarise(n = n(), .groups = 'drop')
			
			if (any(row_counts$n < NUMBEROFTEXTSSPERBOOK)) {
				print(paste("row_counts:", row_counts$n))
				stop("One or more groups have fewer than NUMBEROFTEXTSSPERBOOK rows.")
			}
			
			# Sample 5 rows from each group
			sampled_df <- df %>%
				group_by(last_number) %>%
				slice_sample(n = NUMBEROFTEXTSSPERBOOK) %>%
				ungroup()
			
			return(sampled_df)
		}
		
		# Apply the sampling function to both subsets
		df_l <- sample_from_groups(df_l_set1)
		df_r <- sample_from_groups(df_l_set2)
		
		# # split text_types in two halves at random
		# # Check if the number of rows is odd
		# if (nrow(text_types) %% 2 != 0) {
		# 	# If odd, remove one random row
		# 	set.seed(123) # for reproducibility
		# 	text_types <- text_types[-sample(1:nrow(text_types), 1), ]
		# }
		# 
		# # Create a sequence of alternating 1s and 2s
		# split <- rep(1:2, length.out = nrow(text_types))
		# 
		# # Shuffle the split sequence to randomize row assignments
		# split <- sample(split)
		# 
		# # Split the data frame into two equal parts
		# df_l <- text_types[split == 1,]
		# df_r <- text_types[split == 2,]
		
		# do this for both df_l and df_r
		freqmat_l <- getFreqMat(df_l, opts) # types
		freqmat_r <- getFreqMat(df_r, opts) # types
		freqmat_l$half <- "l"
		freqmat_r$half <- "r"
		
		# rowbind the two halves
		
		freqmat_l$ai_normplus1 <- 1000000 * (freqmat_l$ai_freq + 1) / (sum(freqmat_l$ai_freq))
		freqmat_l$ai_norm_log <- log(freqmat_l$ai_normplus1)
		
		freqmat_r$ai_normplus1 <- 1000000 * (freqmat_r$ai_freq + 1) / (sum(freqmat_r$ai_freq))
		freqmat_r$ai_norm_log <- log(1 + freqmat_r$ai_norm)
		
		freqmat_lr <- rbind(freqmat_l, freqmat_r)
		
		# save table ----
		ftables[[cond]] <- freqmat_lr # trees with head annotation
		
		
	}
	# save(ftables, file = paste0("data-processed/splitha", i, ".RData"))
	# tibble(ftables)
	
	ftables_with_source <- Map(function(tbl, cond) {
		tbl$Source <- cond  # Add a new column with the condition as the source identifier
		return(tbl)
	}, ftables, conditions)
	tibble(ftables_with_source)
	ftables_with_source2 <- do.call(rbind, ftables_with_source)
	tibble(ftables_with_source2)
	summary(ftables_with_source2)
	
	wide <- ftables_with_source2 %>% 
		pivot_wider(id_cols = c(type, Source), 
								names_from = half, 
								values_from = c(ai_norm_log))
	# tibble(wide)
	
	cors <- wide %>%
		group_by(Source) %>%
		mutate(prob1 = exp(l) / sum(exp(l), na.rm = T),
					 prob2 = exp(r) / sum(exp(r), na.rm = T)) %>%
		summarize(cor = cor(x = l, y = r, 
												use = "complete.obs", 
												method = "pearson"),
							cors = cor(x = l, y = r, 
												 use = "complete.obs", 
												 method = "spearman"),
							kl_div = kl_divergence(prob1, prob2),
							js_div = js_divergence(prob1, prob2),
							n = n())
	# cors
	
	reps[[i]] <- cors # trees with head annotation
}
save(reps, file = "data-processed/splitha.RData")
# load("data-processed/splitha.RData", verbose = T)
# summary(reps[[1]])


# options(timeout = 600)
# load("data-processed/splitha.RData", verbose = T)
# MAXREPS = 100
# allreps <- 1:MAXREPS

# str(reps)
# str(allreps)

reps_withi <- list()

# for (i in seq_along(reps)) {
# 	tbl <- reps[[i]]
# 	tbl$rep <- allreps[[i]]
# 	reps_withi[[i]] <- tbl
# }
# reps_withi_df <- do.call(rbind, reps_withi)
# reps_withi_df

reps_withi <- Map(function(tbl, i) {
	tbl$rep <- i  # Add a new column with the condition as the source identifier
	return(tbl)
}, reps, allreps)
tibble(reps_withi)
reps_withi_df <- do.call(rbind, reps_withi)

# reps_withi_df
save(reps_withi_df, file = "data-processed/splithalf.RData")
