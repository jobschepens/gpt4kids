# install.packages("udpipe")
library(quanteda)
library(udpipe)
library(readtext)
library(tidyverse)

### GET LEMMAS ###
# de <- udpipe_download_model(language = "german") # "german-gsd", "german-hdt"
# save(de, file = "data-original/udpipe-de.RData")
load("data-original/udpipe-de.RData", verbose = T)
udmodel_german <- udpipe_load_model(file = de$file_model)
# ud_model <- udpipe_load_model(file = de$file_model)
# load(file = "data-processed/corpus.RData", verbose = T) # types
load(file = "data-processed/corpus-3.5.RData", verbose = T) # types
a <- df_l[,2]
rm(df_l, de)
# takes some time
new_df2 <- lapply(a, function(x) {
	print(".")
	x %>% 
		as.character() %>% 
		udpipe_annotate(udmodel_german, tagger = 'default', parser = 'none', x = .) %>% 
		as.data.frame() %>%
		select(lemma)
})
# save(new_df, file = "data-processed/lemmas.RData")
# save(new_df2, file = "data-processed/lemmas-3.5-2ch.RData")

# load("data-processed/lemmas.RData", verbose = T)
# load("data-processed/lemmas-3.5.RData", verbose = T)
load("data-processed/lemmas-3.5-2ch.RData", verbose = T)
# new_df2 <- as.data.frame(new_df)
new_df <- new_df2
# summary(new_df)
# a <- new_df[[2]]

# takes some time, write lemmas to file
x <- lapply(new_df, function(x) {
	write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized-3.5-ch.txt", append = T)
	# write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized-3.5.txt", append = T)
	# write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized.txt", append = T) 
})

df_le <- readtext::readtext("data-processed/corpus-lemmatized-3.5-ch.txt")
# df_le <- readtext::readtext("data-processed/corpus-lemmatized-3.5.txt")
# df_le <- readtext::readtext("data-processed/corpus-lemmatized.txt") 

# fast: get df with lemmas
df <- do.call(rbind, lapply(seq_along(new_df), 
														function(i) 
															data.frame(element_number = i, 
																				 lemma = unlist(new_df[[i]]$lemma))))
df <- df %>% rename(word = lemma)
df2 <- do.call(rbind, lapply(seq_along(new_df), 
														function(i) 
															data.frame(element_number = i, 
																				 text = paste(new_df[[i]]$lemma, collapse = " "))))

library(dplyr)
text_df <- tibble(df) # lemmas on lines 
text_df2 <- tibble(df2) # lemmatized text on lines
# save(text_df, file = "data-processed/tidycorpus.RData")
# save(text_df2, file = "data-processed/tidycorpus-text.RData")
# save(text_df, file = "data-processed/tidycorpus-3.5.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-3.5.RData")
# save(text_df, file = "data-processed/tidycorpus-3.5-ch.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-3.5-ch.RData")


# takes some time 
# df_l <- readtext::readtext("data-original/gpt3_5_texts/gen_texts*/*.txt")
# faster: load result from readtext::readtext from before
# load(file = "data-processed/corpus.RData", verbose = T)
# load(file = "data-processed/corpus-3.5.RData", verbose = T)
load(file = "data-processed/corpus-3.5-PUNC.RData", verbose = T)
text_types <- tibble(df_l)

library(tidytext)
ftable <- text_df %>% 
	count(word, sort = TRUE) # 35733 lemmas

ftable_types <- text_types %>% 
	unnest_tokens(type, text) %>%  # output column name, input column name,  
	count(type, sort = TRUE)
sum(ftable_types$n) # types without TM

# word length
ftable_types <- ftable_types %>%
	mutate(word_length = nchar(type))
ftable_types

ftable2 <- text_df2 %>% 
	unnest_tokens(lemma, text) %>% 
	count(lemma, sort = TRUE)
ftable2 # less lemmas? 34,288
tail(unique(ftable$n))
tail(unique(ftable2$n))

stopwords = stopwords::stopwords(language = "de", source = "snowball")
custom_stop_words <- c("NA", ",", "-", ":", ".", "'", "!", "?", "2", "1", "4", "3", "5", "\"", NA, stopwords)
head(custom_stop_words)
# a[476,]
# custom_stop_words <- bind_rows(data_frame(word = c("NA", ",", ".", "\"", NA),
																					# lexicon = "custom"), stop_words)
custom_stop_words <- as.data.frame(custom_stop_words) %>% rename(word = custom_stop_words)
head(custom_stop_words)

text_df_rem <- text_df %>%
	anti_join(custom_stop_words)
# save(text_df_rem, file = "data-processed/word_rem-lemmas.RData")
# save(text_df_rem, file = "data-processed/word_rem-lemmas-3.5.RData")

# 3,024,481 tokens
text_df_rem <- text_types %>%
	unnest_tokens(word, text) %>% 
	anti_join(custom_stop_words)
# save(text_df_rem, file = "data-processed/word_rem-types.RData")
# save(text_df_rem, file = "data-processed/word_rem-types-3.5.RData")

# 6,275,041 tokens
text_df_all <- text_types %>%
	unnest_tokens(word, text) 
# save(text_df_all, file = "data-processed/word_types.RData")
# save(text_df_all, file = "data-processed/word_types-3.5.RData")
# load("data-processed/word_types-3.5.RData", verbose = T)
# text_df_all[text_df_all$word == "Oma", ]

# 43,061 types
text_df_all_cnt <- text_types %>%
	unnest_tokens(word, text) %>% 
	count(word, sort = TRUE)
# save(text_df_all_cnt, file = "data-processed/word_types_cnt.RData")
# save(text_df_all_cnt, file = "data-processed/word_types_cnt-3.5.RData")
# load("data-processed/word_types_cnt-3.5.RData", verbose = T)
text_df_all_cnt[text_df_all_cnt$word == "oma", ]
text_df_all_cnt[text_df_all_cnt$word == "Oma", ]
text_df_all_cnt[text_df_all_cnt$word == "und", ]


ftable <- text_df %>%
	count(word, sort = TRUE) %>%
	mutate(word = reorder(word, n)) %>%
	anti_join(custom_stop_words)
# save(ftable, file = "data-processed/ftable-lemmas.RData")
# save(ftable, file = "data-processed/ftable-lemmas-3.5.RData")
# save(ftable, file = "data-processed/ftable-lemmas-3.5-ch.RData")
ftable
