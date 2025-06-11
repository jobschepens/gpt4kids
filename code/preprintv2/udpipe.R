# install.packages("udpipe")
library(dplyr)
library(quanteda)
library(udpipe)
library(readtext)
library(tidyverse)
library(tidytext)


### GET LEMMAS ###
# de <- udpipe_download_model(language = "german") # "german-gsd", "german-hdt"
# save(de, file = "data-original/udpipe-de.RData")
load("data-original/udpipe-de.RData", verbose = T)
udmodel_german <- udpipe_load_model(file = de$file_model)
# ud_model <- udpipe_load_model(file = de$file_model)

# choose corpus to load from tm_ben.R
# load(file = "data-processed/corpus.RData", verbose = T) # types
# load(file = "data-processed/corpus-3.5.RData", verbose = T) # types
# load(file = "data-processed/corpus-3.5-PUNC.RData", verbose = T) # used for udpipe,  includes PUNC! no freqmat
# load(file = "data-processed/corpus-adlt.RData", verbose = T) # used for udpipe
# load(file = "data-processed/corpus-adht.RData", verbose = T) # used for udpipe
# load(file = "data-processed/corpus-chht.RData", verbose = T) # used for udpipe
load(file = "data-processed/corpus-chlt.RData", verbose = T) # used for udpipe
# df_l[1,2]

a <- df_l[,2] # select only the texts
rm(df_l, de)

# takes some time
new_df2 <- lapply(a, function(x) {
    print(".")
    x %>%
        as.character() %>%
        udpipe_annotate(udmodel_german, tagger = "default", parser = "none", x = .) %>%
        as.data.frame() %>%
        select(lemma)
})
# save(new_df, file = "data-processed/lemmas.RData")
# save(new_df2, file = "data-processed/lemmas-3.5-2ch.RData") # used for the rest
# save(new_df2, file = "data-processed/lemmas-adlt.RData")
# save(new_df2, file = "data-processed/lemmas-adht.RData")
# save(new_df2, file = "data-processed/lemmas-chhtrue.RData") # double saved, change back!
# save(new_df2, file = "data-processed/lemmas-chlt.RData") # double saved, this!
# load("data-processed/lemmas.RData", verbose = T)
# load("data-processed/lemmas-3.5.RData", verbose = T)
# load("data-processed/lemmas-3.5-2ch.RData", verbose = T) 
# load("data-processed/lemmas-adlt.RData", verbose = T)
# load("data-processed/lemmas-adht.RData", verbose = T)
load("data-processed/lemmas-chlt.RData", verbose = T)
# load("data-processed/lemmas-chht.RData", verbose = T)

# new_df2 <- as.data.frame(new_df)
new_df <- new_df2
# summary(new_df)
# a <- new_df[[2]]

# takes some time, write lemmas to file
# x <- lapply(new_df, function(x) {
# 	write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized-3.5-ch.txt", append = T)
# 	# write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized-3.5.txt", append = T)
# 	# write_lines(paste(x$lemma, collapse = " "), "data-processed/corpus-lemmatized.txt", append = T) 
# })
# df_le <- readtext::readtext("data-processed/corpus-lemmatized-3.5-ch.txt")
# df_le <- readtext::readtext("data-processed/corpus-lemmatized-3.5.txt")
# df_le <- readtext::readtext("data-processed/corpus-lemmatized.txt") 

# fast: get df with lemmas
# lemmas on new lines
df <- do.call(rbind, lapply(
    seq_along(new_df),
    function(i) {
        data.frame(
            element_number = i,
            lemma = unlist(new_df[[i]]$lemma)
        )
    }
))
df <- df %>% rename(word = lemma)
text_df <- tibble(df) # lemmas on lines 
# save(text_df, file = "data-processed/tidycorpus.RData")
# save(text_df, file = "data-processed/tidycorpus-3.5.RData")
# save(text_df, file = "data-processed/tidycorpus-3.5-ch.RData") # used for table
# save(text_df, file = "data-processed/tidycorpus-adlt.RData") # used for table
# save(text_df, file = "data-processed/tidycorpus-adht.RData") # used for table
# save(text_df, file = "data-processed/tidycorpus-chht.RData") # used for table
# save(text_df, file = "data-processed/tidycorpus-chlt.RData") # used for table

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
# save(text_df2, file = "data-processed/tidycorpus-text.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-3.5.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-3.5-ch.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-adlt.RData") 
# save(text_df2, file = "data-processed/tidycorpus-text-adht.RData")
save(text_df2, file = "data-processed/tidycorpus-text-chlt.RData")
# save(text_df2, file = "data-processed/tidycorpus-text-chht.RData")


##### create ftable with tidytext ########

# load data again (loaded above already)

# takes some time
# uses readtext instead of tm package
# df_l <- readtext::readtext("data-original/gpt3_5_texts/gen_texts*/*.txt")

# faster: load result from readtext::readtext from before
# uses text minining package
# load(file = "data-processed/corpus.RData", verbose = T)
# load(file = "data-processed/corpus-3.5.RData", verbose = T)
# load(file = "data-processed/corpus-3.5-PUNC.RData", verbose = T) #  includes PUNC! no freqmat

# load df_l
# load(file = "data-processed/corpus-adlt.RData", verbose = T) # used for udpipe
load(file = "data-processed/corpus-adht.RData", verbose = T) # used for udpipe
# load(file = "data-processed/corpus-chht.RData", verbose = T) # used for udpipe
# df_l[2,2] # includes PUNC
text_types <- tibble(df_l)


# Lemma 
library(tidytext)
ftable <- text_df %>% # lemmas on lines 
	count(word, sort = TRUE) 
# exp1 35,733 lemmas
# adlt 53,772 
# adht 62,692 

# Type ftable to lower
ftable_types <- text_types %>% # raw texts
	unnest_tokens(type, text, token = "words", 
								to_lower = TRUE) %>%  # output column name, input column name,  
	count(type, sort = TRUE)
sum(ftable_types$n) # types without TM
# adlt 66230 adlt sum 7 231 375
# adht 76685 adlt sum 7 405 719

# word length
# ftable_types <- ftable_types %>%
# 	mutate(word_length = nchar(type))
# ftable_types


# Lemma 
ftable2 <- text_df2 %>%  # lemmatized text on lines
	unnest_tokens(lemma, text) %>% 
	count(lemma, sort = TRUE)
ftable2 # less lemmas? due to punctuation?
# exp1 34,288, 
# adlt 51,451
# adht 62,692
tail(unique(ftable$n))
tail(unique(ftable2$n))

stopwords = stopwords::stopwords(language = "de", source = "snowball")
custom_stop_words <- c("NA", ",", "-", ":", ".", "'", "!", "?", "2", "1", "4", "3", "5", "\"", NA, stopwords)
head(custom_stop_words)
# a[476,]
# custom_stop_words <- bind_rows(data_frame(word = c("NA", ",", ".", "\"", NA), lexicon = "custom"), stop_words)
custom_stop_words <- as.data.frame(custom_stop_words) %>% rename(word = custom_stop_words)
head(custom_stop_words)

# Lemma 
text_df_rem <- text_df %>% # lemmas on lines 
	anti_join(custom_stop_words)
# save(text_df_rem, file = "data-processed/word_rem-lemmas.RData")
# save(text_df_rem, file = "data-processed/word_rem-lemmas-3.5.RData")

# Type removed stopwords
text_df_rem <- text_types %>% # raw texts
	unnest_tokens(word, text) %>% 
	anti_join(custom_stop_words)
# exp1: 3,024,481 tokens
# adlt: 3 668 691
# adht: 3 777 701
# save(text_df_rem, file = "data-processed/word_rem-types.RData")
# save(text_df_rem, file = "data-processed/word_rem-types-3.5.RData")

# Type raw texts
text_df_all <- text_types %>% # raw texts
	unnest_tokens(word, text) 
# exp1: 6,275,041 tokens
# adlt: 7 231 375
# adht: 7 405 719
# save(text_df_all, file = "data-processed/word_types.RData")
# save(text_df_all, file = "data-processed/word_types-3.5.RData")
# load("data-processed/word_types-3.5.RData", verbose = T)
# text_df_all[text_df_all$word == "Oma", ]

# Type count, ftable
text_df_all_cnt <- text_types %>% # raw texts
	unnest_tokens(word, text) %>% 
	count(word, sort = TRUE)
# exp1: 43,061 types
# adlt: 66,230 types
# adht: 76,685 types
# save(text_df_all_cnt, file = "data-processed/word_types_cnt.RData")
# save(text_df_all_cnt, file = "data-processed/word_types_cnt-3.5.RData")
# load("data-processed/word_types_cnt-3.5.RData", verbose = T)
text_df_all_cnt[text_df_all_cnt$word == "oma", ]
text_df_all_cnt[text_df_all_cnt$word == "Oma", ] # no case sensitivity
text_df_all_cnt[text_df_all_cnt$word == "und", ]

# Lemma 
ftable <- text_df %>% # lemmas on lines 
	count(word, sort = TRUE) %>%
	mutate(word = reorder(word, n)) %>%
	anti_join(custom_stop_words)
# save(ftable, file = "data-processed/ftable-lemmas.RData")
# save(ftable, file = "data-processed/ftable-lemmas-3.5.RData")
# save(ftable, file = "data-processed/ftable-lemmas-3.5-ch.RData") # used for table
# save(ftable, file = "data-processed/ftable-lemmas-adlt.RData") # used for table
# save(ftable, file = "data-processed/ftable-lemmas-adht.RData") # used for table
# save(ftable, file = "data-processed/ftable-lemmas-chht.RData") # used for table
ftable
# exp1:
# adlt: 53,571
# adht: 62,537