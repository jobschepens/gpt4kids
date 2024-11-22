# https://www.linguistics.rub.de/litkeycorpus/access.html
library(readxl)
xlsx = "data-original/litkey/Litkey-DB-removefirstrow.xlsx"
# litkey1 <- read_excel(xlsx, sheet = 1, range = cell_cols(c("A:AF")), col_names = T)
litkey1 <- read_excel(xlsx, sheet = 1, col_names = T)
# litlemma <- litkey1 %>% summarise(.by = chl_lemma, litk_lem_norm = as.double(chl_lemma.norm[1]))
# litlemma$litk_lem_norm <- round(litlemma$litk_lem_norm, digits = 1)
# litlemma <- litlemma %>% arrange(desc(litk_lem_norm))
# save(litlemma, file = "data-processed/litlemma.RData") # this is childlex lemma
# litkey1$litkey_norm_childlex <- as.double(litkey1$chl_lemma.norm)
litkey1$litkey_norm_childlex <- as.double(litkey1$chl_type.norm)
litkey1$litkey_norm_childlex_r <- round(litkey1$litkey_norm_childlex, digits = 0)
litkey1 <- litkey1[!is.na(litkey1$litkey_norm_childlex_r),]
litkey1 <- litkey1 %>% filter(litkey_norm_childlex > 0)
litkey1 <- litkey1 %>% select(target, litkey_norm_childlex_r)
litkey1 <- litkey1 %>% distinct()
# litkey1[litkey1$target == "beginnen",]

xlsx = "data-original/litkey/Litkey-Tab-removefirstrow.xlsx"
# litkey_types <- read_excel(xlsx, sheet = 1, range = cell_cols(c("A:AV")), col_names = T, progress = T)
litkey_types <- read_excel(xlsx, sheet = 1, col_names = T, progress = T)
# litkey_types[litkey_types$target == "beginnen",]
# litkey_types[litkey_types$target == "Rucksack", ]
# littab <- litkey_types %>% select(type, chl_type.norm)
littab <- litkey_types %>% select(chl_lemma, chl_type.norm)
littab$litkey_norm_childlex <- as.double(littab$chl_type.norm)
# littab$litkey_norm_childlex <- as.double(littab$chl_lemma.norm)
littab$litkey_norm_childlex_r <- round(littab$litkey_norm_childlex, digits = 0)
# litkey1 <- litkey1[!is.na(litkey1$litkey_norm_childlex_r),]
# littab <- littab[!is.na(littab$chl_type.norm),]

litkey2 <- littab %>% summarise(.by = chl_lemma, litkey_abs = n())
litkey2$litkey_norm_children <- 1000000 * litkey2$litkey_abs / (sum(litkey2$litkey_abs))
litkey2 <- litkey2 %>% filter(litkey_norm_children > 0)
litkey2 %>% arrange(desc(litkey_abs))
litkey2 <- litkey2[litkey2$chl_lemma != "NA", ]
litkey2[litkey2$chl_lemma == "beginnen", ]
litkey2 <- litkey2 %>% rename(target = chl_lemma)
litkey <- right_join(litkey1, litkey2, by = join_by(target))

length(unique(litkey1$target))
length(unique(litkey2$target))
length(unique(litkey$target))
litkey[litkey$target == "beginnen", ]
litkey[litkey$target == "Rucksack", ]
cor.test(litkey$litkey_norm_childlex_r, litkey$litkey_norm_children) #.82

# litkey
# save(litkey, file = "litkey.RData")
# save(litkey, file = "litkey2.RData")
# save(litkey2, file = "data-processed/litkey-lemma.RData")