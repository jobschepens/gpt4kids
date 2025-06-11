# experiment 1 merged data from tm_ben.Rmd
load("data-processed/corpus-3.5-PUNC.RData", verbose = T)
# load("data-processed/merged_df-type-3.5.RData")
# load("data-processed/corpus.RData", verbose = T)
# load("data-processed/corpus-3.5.RData", verbose = T)
# load("data-processed/corpus-lemma.RData", verbose = T)
# load("data-processed/tidycorpus-text.RData", verbose = T)

df <- df_l
# subset_sizes <- seq(1000, nrow(tdm), by = 1000)
# subset_sizes <- seq(10, nrow(df), by = 50)
subset_sizes1 <- seq(2, 100, by = 1)
subset_sizes2 <- seq(100, nrow(df), by = 10)
subset_sizes <- c(subset_sizes1, subset_sizes2)
subset_sizes <- seq(1, 400, by = 1)
cor_coeffs <- rep(NA, length(subset_sizes))
opts <- list(removeNumbers = TRUE,
						 removePunctuation = T,
						 # preserve_intra_word_contractions = TRUE,
						 # preserve_intra_word_dashes = TRUE,
						 stopwords = F,
						 stemming = FALSE, 
						 tolower=FALSE,
						 wordLengths = c(2, Inf))

load("data-processed/childLexAgg.RData", verbose = T)
summary(childLexAgg)

for(i in 2:length(subset_sizes)) {
	# i = 2
	s <- sample(nrow(df), size = subset_sizes[i], replace = F)
	df_s <- df[s,]
	print(nrow(df_s))
	corpus <- Corpus(VectorSource(df_s$text))
	tdm_s <- as.matrix(TermDocumentMatrix(corpus, control = opts))
	freqmat <- data.frame(type = rownames(tdm_s), 
												ai_freq = rowSums(tdm_s), 
												row.names = NULL)
	# x <- lexicon::freq_first_names %>% slice_max(prop, n = 4000)
	# tdm_s <- tdm[!rownames(tdm) %in% x$Name,]
	# freqmat <- freqmat[!(freqmat$type %in% x$Name),]
	freqmat$ai_norm <- 1000000 * freqmat$ai_freq / (sum(freqmat$ai_freq))
	# merged_df <- inner_join(childLexAgg, freqmat, join_by(type == type))
	# merged_df <- left_join(childLexAgg, freqmat, join_by(type == type))
	merged_df <- full_join(childLexAgg, freqmat, join_by(type == type))
	merged_df$ai_norm[is.na(merged_df$ai_norm)] = .1
	merged_df$childlex_norm[is.na(merged_df$childlex_norm)] = .1
	merged_df$childlex_log <- log(merged_df$childlex_norm)
	merged_df$ai_log <- log(merged_df$ai_norm)
	# merged_df$ai_log[is.na(merged_df$ai_log)] = log(.1)
	# c <- with(merged_df, cor(childlex_norm, ai_norm, use = "complete.obs"))
	c <- with(merged_df, cor(childlex_log, ai_log, use = "complete.obs"))
	print(c)
	cor_coeffs[i] <- c
}
# cor_coeffs
cors_sim <- data.frame(subset_sizes, cor_coeffs)

save(cors_sim, file = "data-processed/cors_sim_many-3.5-logs-full.RData")
# save(cors_sim, file = "data-processed/cors_sim_many-3.5-logs.RData")
# save(cors_sim, file = "data-processed/cors_sim_many-3.5.RData")
# save(cors_sim, file = "data-processed/cors_sim_many.RData")
# save(cors_sim, file = "data-processed/cors_sim_many-lemma.RData")

# load("cors_sim.RData", verbose = T)
# ggplot(cors_sim, aes(x = subset_sizes, y = cor_coeffs)) +
# 	geom_line() +
# 	xlab("Number of Texts") +
# 	ylab("Correlation Coefficient")

