# install.packages("udpipe")
library(dplyr)
library(quanteda)
library(udpipe)
library(readtext)
library(tidyverse)
library(tidytext)
library(tm)
library(readtext)
library(SnowballC)
# conditions = c("lllo", "llsh", "dslo", "dssh")
conditions = c("dslo", "dssh")

# make a list to store the frequency table for each condition
ftables <- list()

for(cond in conditions) {

	print(cond)
	
	# cond = "dslo"
	# cond = "gpt4"
	
	# GET LEMMAS ----
	
	# udpipe_download_model(language = "german-hdt", model_dir = paste0(getwd(), "/data-original")) # "german-gsd", "german-hdt"
	# udpipe_download_model(language = "german-gsd", model_dir = paste0(getwd(), "/data-original")) # "german-gsd", "german-hdt"
	udmodel_german <- udpipe_load_model(file = "data-original/german-gsd-ud-2.5-191206.udpipe") # used for all earlier lemma analyses
	# udmodel_german <- udpipe_load_model(file = "data-original/german-hdt-ud-2.5-191206.udpipe") # used for newer POS analyses
	
	# corpus from tm_ben.R
	load(file = paste0("data-processed/corpus-", cond, ".RData"), verbose = T) # used for udpipe
	# df_l[1,2]
	
	a <- df_l[,2] # select only the texts
	rm(df_l)
	
	# takes some time
	new_df2 <- lapply(a, function(x) {
	    print(".")
	    x %>%
	        as.character() %>%
	        udpipe_annotate(udmodel_german, tagger = "default", parser = "none", x = .) %>%
	        as.data.frame() %>%
	        select(lemma)
	})
	save(new_df2, file = paste0("data-processed/lemmas-", cond, ".RData"))
	
	new_df <- new_df2
	
	# fast: get df with lemmas
	# lemmas on new lines
	# not used anywhere else
	# df <- do.call(rbind, lapply(
	#     seq_along(new_df),
	#     function(i) {
	#         data.frame(
	#             element_number = i,
	#             lemma = unlist(new_df[[i]]$lemma)
	#         )
	#     }
	# ))
	# df <- df %>% rename(word = lemma)
	# text_df <- tibble(df) # lemmas on lines 
	# save(text_df, file = paste0("data-processed/tidycorpus-", cond, ".RData"))
	
	# lemmatext on new lines
	df2 <- do.call(rbind, lapply(
	    seq_along(new_df),
	    function(i) {
	        data.frame(
	            element_number = i,
	            text = paste(new_df[[i]]$lemma, collapse = " ")
	        )
	    }
	))
	text_df2 <- tibble(df2) # lemmatized text on lines
	save(text_df2, file = paste0("data-processed/tidycorpus-text-", cond, ".RData"))

	
	# get freq table  ---- 
	
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
	
	freqmat_l <- getFreqMat(text_df2, opts) # lemma
	freqmat_l$ai_norm <- 1000000 * freqmat_l$ai_freq / (sum(freqmat_l$ai_freq))
	freqmat_l$ai_normplus1 <- 1000000 * (freqmat_l$ai_freq + 1) / (sum(freqmat_l$ai_freq))
	freqmat_l$ai_norm_log <- log(freqmat_l$ai_normplus1)
	wordtype = "lemma"
	save(freqmat_l, file = paste0("data-processed/freqmat_l-", wordtype, "-", cond, ".RData")) # used for table
}