# load("data-processed/merged_df-type.RData", verbose = T)
# load("data-processed/merged_df-lemma.RData", verbose = T)
# load("data-processed/merged_df-type-3.5.RData", verbose = T)
load("data-processed/merged_df-type-3.5-full.RData", verbose = T)
merged_df <- merged_dfx
merged_df$ai_norm[is.na(merged_df$ai_norm)] = .1
merged_df$childlex_norm[is.na(merged_df$childlex_norm)] = .1
merged_df <- merged_df %>% rename(lemma_type = type)

ai <- merged_df %>% arrange(desc(ai_norm)) 
ch <- merged_df %>% arrange(desc(childlex_norm))
summary(ai$ai_norm)
summary(ch$childlex_norm)
a <- ch %>% filter(childlex_norm < 2)
table(a$childlex_norm)
b <- ai %>% filter(ai_norm < 2)
table(round(b$ai_norm, digits = 2))

# su <- read_delim(file = "data-original/SUBTLEX_clean.txt", delim = " ", col_types = "cdi")
# su <- su %>% arrange(desc(lgfreqCount))
du <- read_delim(file = "data-original/SUBTLEX-DE_cleaned_with_Google00.txt", delim = "\t", locale = locale(decimal_mark = ","))
du <- du %>% arrange(desc(SUBTLEX))
du2 <- du %>% arrange(desc(Google00pm))
ais <- ai %>% select(ai_norm, lemma_type)
chs <- ch %>% select(childlex_norm, lemma_type)
dus <- du %>% rename(lemma_type = Word) %>% select(SUBTLEX, lemma_type)
du2s <- du2 %>% rename(lemma_type = Word) %>% select(Google00pm, lemma_type)

# add ranks
# ais <- ai %>% mutate(xai=1:length(lemma_type)) %> select(ai_norm, xai, lemma_type)
# chs <- ch %>% mutate(xch=1:length(lemma_type)) %>% select(childlex_norm, xch, lemma_type)
# dus <- du %>% rename(lemma_type = Word) %>% mutate(xsu=1:length(lemma_type)) %>% select(SUBTLEX, xsu, lemma_type)
# du2s <- du2 %>% rename(lemma_type = Word) %>% mutate(xgo=1:length(lemma_type)) %>% select(Google00pm, xgo, lemma_type)

# join to be able to add ranks afterwards
df <- ais %>% 
	inner_join(chs, by = 'lemma_type') %>% 
	inner_join(dus, by = 'lemma_type') %>% 
	inner_join(du2s, by = 'lemma_type')

dfx <- ais %>% 
	inner_join(chs, by = 'lemma_type')

# add ranks
dfa <- df %>% arrange(desc(ai_norm)) %>% mutate(xai=1:length(lemma_type)) %>%
							arrange(desc(childlex_norm)) %>% mutate(xch=1:length(lemma_type)) %>%
							arrange(desc(SUBTLEX)) %>% mutate(xsu=1:length(lemma_type)) %>%
							arrange(desc(Google00pm)) %>% mutate(xgo=1:length(lemma_type)) 

# add ranks
dfxa <- dfx %>% arrange(desc(ai_norm)) %>% mutate(xai=1:length(lemma_type)) %>%
	arrange(desc(childlex_norm)) %>% mutate(xch=1:length(lemma_type))

# plot(tail(dfa$Google00pm, n = 10000))

# df2 <- df[!is.na(df$childlex_norm),]
# df2a <- df2[complete.cases(df2),]

# normal rank plot, does not help showing the diffs
ggplot(data = dfa) +
	geom_line(aes(x=xai, y=ai_norm), color = "red") +
	geom_line(aes(x=xch, y=childlex_norm), color = "blue") +
	geom_line(aes(x=xsu, y=SUBTLEX), color = "green") +
	geom_line(aes(x=xgo, y=Google00pm), color = "purple") +
	# scale_x_continuous(trans = "log10") +
	# scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency") +
	ggtitle("Word frequency against rank on a logarithmic scale") + 
	my_theme + 
	theme(plot.margin = margin(.1, 0, .1, 0, "cm")) +
	coord_fixed(ratio = 1) +
	scale_x_continuous(trans = "log10", 
										 breaks = c(.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, "1k", "5k", "10k", "50k"),
										 expand = c(0, 0), 
										 limits = c(1, 200000)) +
	scale_y_continuous(trans = "log10", 
										 breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 # labels = scales::label_number(accuracy = 1), 
										 expand = c(0, 0), 
										 limits = c(0.09, 100000)) 
# ggsave(filename = paste("figures/", "rankf1-all4.pdf", sep = ""), scale = 1, width = 4, height = 4)

# summary(dfxa)
a <- dfxa %>% select(xai, ai_norm) %>% 
	rename(rank = xai, f = ai_norm) %>% mutate(corpus = "LLM")
b <- dfxa %>% select(xch, childlex_norm) %>% 
	rename(rank = xch, f = childlex_norm) %>% mutate(corpus = "childlex")
c <- rbind(a,b)

ggplot(data = c, aes(x=log(1+f), color = corpus)) +
	geom_density() +
	scale_x_continuous(limits = c(0.1, 5)) 

ggplot(data = c, aes(x=f, color = corpus)) +
	geom_density() +
	scale_x_continuous(limits = c(0.01, 2)) 


ggplot(data = c, aes(x=rank, y=f, color = corpus)) +
	geom_line() +
	# geom_line(aes(x=xch, y=childlex_norm), color = "blue") +
	# geom_line(aes(x=xai, y=ai_norm), color = "red") +
	xlab("Rank") +
	ylab("Frequency") +
	# ggtitle("Word frequency against rank") + 
	my_theme + 
	scale_color_discrete(name ="Corpus name", labels=c("ChildLEX", "LLM")) +
	theme(
				# title = element_text(size = rel(1)),
				legend.position = c(.99, .99), legend.justification = c(1, 1),
				legend.background = element_rect(color = "#B8B8B8", fill = "white"),
				legend.title = element_text(size = rel(1)),
				legend.text = element_text(size = rel(.65))) + 
	theme(plot.margin = margin(.1, 0, .1, 0, "cm")) +
	coord_fixed(ratio = 1) +
	scale_x_continuous(trans = "log10", 
										 breaks = c(.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, "1k", "5k", "10k", "50k"),
										 expand = c(0, 0), 
										 limits = c(1, 200000)) +
	scale_y_continuous(trans = "log10", 
										 breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 # labels = scales::label_number(accuracy = 1), 
										 expand = c(0, 0), 
										 limits = c(0.09, 100000)) 

# ggsave(filename = paste("figures/", "rankf1.pdf", sep = ""), scale = 1, width = 4, height = 4)

# compute diffs
df3 <- dfa %>% mutate(difch = ai_norm - childlex_norm, 
											difsu = ai_norm - SUBTLEX, 
											difgo = ai_norm - Google00pm)
# compute diffs
df3x <- dfxa %>% mutate(difch = ai_norm - childlex_norm)

# plot, but ranks are different
ggplot(data = df3x) +
	# geom_line(aes(x=xai, y=ai_norm), color = "red") +
	geom_line(aes(x=xai, y=difch), color = "blue") +
	# geom_line(aes(x=xai, y=difsu), color = "green") +
	# geom_line(aes(x=xai, y=difgo), color = "purple") +
	scale_x_continuous(trans = "log10") +
	scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency dif") +
	ggtitle("Word frequency against rank on a logarithmic scale") + 
	my_theme

# join by rank
sai <- dfa %>% select(ai_norm, xai, lemma_type)
sai <- dfxa %>% select(ai_norm, xai, lemma_type)
sch <- dfa %>% select(childlex_norm, xch, lemma_type)
sch <- dfxa %>% select(childlex_norm, xch, lemma_type)
ssu <- dfa %>% select(SUBTLEX, xsu, lemma_type)
sgo <- dfa %>% select(Google00pm, xgo, lemma_type)
df4 <- sai %>%
	inner_join(sch, by = join_by('xai' == "xch")) %>% 
	inner_join(ssu, by = join_by('xai' == "xsu")) %>% 
	inner_join(sgo, by = join_by('xai' == "xgo"))
# df4x 58456
# 58,456
df4x <- sai %>%
	inner_join(sch, by = join_by('xai' == "xch")) 
df4 <- df4 %>% rename(lemma_type.ai = lemma_type.x,
							 lemma_type.ch = lemma_type.y,
							 lemma_type.su = lemma_type.x.x,
							 lemma_type.go = lemma_type.y.y)
colnames(df4x)
df4x <- df4x %>% rename(lemma_type.ai = lemma_type.x,
											lemma_type.ch = lemma_type.y)


# sanity check, yes, same as normal rank plot
ggplot(data = df4x) +
	geom_line(aes(x=xai, y=ai_norm), color = "red") +
	geom_line(aes(x=xai, y=childlex_norm), color = "blue") +
	# geom_line(aes(x=xai, y=SUBTLEX), color = "green") +
	# geom_line(aes(x=xai, y=Google00pm), color = "purple") +
	scale_x_continuous(trans = "log10") +
	scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency") +
	ggtitle("Word frequency against rank on a logarithmic scale") + 
	my_theme

# save(df4, file = "data-processed/lemmafrequenciespercorpus.RData")
# save(df4, file = "data-processed/typefrequenciespercorpus.RData")
# save(df4, file = "data-processed/typefrequenciespercorpus-3.5.RData")
# save(df4x, file = "data-processed/typefrequenciespercorpus-3.5-2.RData")

# compute diffs again
df4a <- df4 %>% mutate(difch = (round(ai_norm - childlex_norm, digits = 1)),
						  					difsu = (round(ai_norm - SUBTLEX, digits = 1)),
							  				difgo = (round(ai_norm - Google00pm, digits = 1)))
df4ax <- df4x %>% mutate(difch = (round(ai_norm - childlex_norm, digits = 1)))

# 
# compute diffs with transformation
# df4a <- df4 %>% mutate(difch = abs(10000 + ai_norm - childlex_norm)^(1/4)*sign(10000 + ai_norm - childlex_norm)- 10,
# 											 difsu = abs(10000 + ai_norm - SUBTLEX)^(1/4)*sign(10000 + ai_norm - SUBTLEX)- 10,
# 											 difgo = abs(10000 + ai_norm - Google00pm)^(1/4)*sign(10000 + ai_norm - Google00pm)- 10) 

# x=(-5000:5000)/1000
# y = abs(x)^(1/10)*sign(x)
# plot(x,y)
# y
df4a <- df4a %>% arrange(xai)
df4a$a <- "1-10"
df4a$a[10:100] <- "10-100"
df4a$a[100:1000] <- "100-1000"
df4a$a[1000:nrow(df4a)] <- "1000-max"
df4a$a <- as.factor(df4a$a)
# summary(df4a)
# table(df4a$a)

df4ax <- df4ax %>% arrange(xai)
df4ax$a <- "1-10"
df4ax$a[10:100] <- "10-100"
df4ax$a[100:1000] <- "100-1000"
df4ax$a[1000:nrow(df4ax)] <- "1000-max"
df4ax$a <- as.factor(df4ax$a)


# df4a <- df4a[complete.cases(df4a),]
# plot, where do all these strange spikes come from? 
# basically around 0
ggplot(data = df4a, aes(x=xai)) +
	# geom_line(aes(x=xai, y=ai_norm), color = "red") +
	geom_line(aes(y=difch), color = "blue") +
	geom_line(aes(y=difsu), color = "green") +
	geom_line(aes(y=difgo), color = "purple") +
	# scale_x_continuous(trans = "log10", limits = c(20,100)) +
	scale_x_continuous(trans = "log10", limits = c(10,22000)) +
	# scale_y_continuous(limits = c(-1000, 4000)) +
	scale_y_continuous(limits = c(-100, 2500)) +
	xlab("Rank") +
	# ylab("Frequency") +
	ggtitle("Word frequency against rank on a logarithmic scale") + 
	my_theme

# pivot longer
df5 <- df4a %>% pivot_longer(cols = c(ai_norm, childlex_norm, SUBTLEX, Google00pm),
														 values_to = "f", names_to = "corpus")
df5x <- df4ax %>% pivot_longer(cols = c(ai_norm, childlex_norm), 
															 values_to = "f", names_to = "corpus")

# sanity check, same as normal rank plot
ggplot(data = df5x, aes(x=xai, y=f, color = corpus)) +
	geom_line() +
	geom_smooth(method = "lm", 
							data = df5x %>% filter(xai < 10000, xai > 10),
							size = .5,
							linetype = "dashed") + 
	# geom_smooth(method = "nls",
	# 						formula = y ~ 1/x, 
	# 						# formula = y ~ a/x^b + c, 
	# 						# method.args = list(start = c(a = 1, b = 1, c = 1)),
	# 						se = FALSE, color = "red") + 
	# geom_smooth(method = "lm") +
	# geom_smooth(method = "gam") +
	# scale_x_continuous(trans = "log10") +
	# scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency") +
	my_theme + 
	# scale_color_discrete(name ="Corpus name", 
											 # labels=c("LLM", "ChildLEX", "SubtLEX", "Google Books")) + 
	scale_color_discrete(name ="Corpus name", labels=c("LLM", "ChildLEX")) +
	theme(title = element_text(size = rel(1)),
				legend.position = c(.99, .99), legend.justification = c(1, 1), 
				legend.background = element_rect(color = "#B8B8B8", fill = "white"),
				legend.title = element_text(size = rel(.65)),
				legend.text = element_text(size = rel(.65))) + 
	theme(plot.margin = margin(.1, 0, .1, 0, "cm")) +
	coord_fixed(ratio = 1) +
	scale_x_continuous(trans = "log10", 
										 breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, "50k", "10k", "50k"),
										 # labels = scales::label_number(accuracy = 1), 
										 expand = c(0, 0), 
										 limits = c(1, 200000)) +
	scale_y_continuous(trans = "log10", 
										 breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
										 expand = c(0, 0), 
										 limits = c(.09, 100000)) 
# ggsave(filename = paste("figures/", "rankplot-normal-3.5-2.pdf", sep = ""), scale = 1, width = 4, height = 4)
# ggsave(filename = paste("figures/", "rankplot-normal.pdf", sep = ""), scale = 1, width = 4, height = 4)
# ggsave(filename = paste("figures/", "rankplot-normal-lemma.pdf", sep = ""), scale = 1, width = 4, height = 4)

# rank_subset <- df5 %>%
# 	filter(xai < 1000,
# 				 xai > 10)
# rank_subset$corpus <- as.factor(rank_subset$corpus)
# rank_subset %>% summarise(.by = corpus, lm(log10(f) ~ log10(xai), data = .))


library(dplyr)
library(broom)
# df5a <- df4a %>% pivot_longer(cols = c(difch, difsu, difgo), values_to = "f", names_to = "corpus")
df5ax <- df4ax %>% pivot_longer(cols = c(difch), values_to = "f", names_to = "corpus")

a <- df5ax %>% filter(f < -1 & f > -5)
# summary(df5ax)
a <- df5ax %>% filter(childlex_norm < 2)
# table(a$childlex_norm)
b <- df5ax %>% filter(ai_norm < 2)
# table(round(b$ai_norm, digits = 2))
# table(round(b$f, digits = 2))
df5ax$f_l <- (f + 1) / (N + 2)

# summary(df5a)
# same plot as before
# max(df5ax$xai)
ggplot(data = df5ax, aes(x=xai, y=f, color = corpus)) +
	# facet_wrap(~ a, nrow = 1, scales = "free") + 
	geom_line() +
	scale_x_continuous(trans = "log10", limits = c(0.9,200000)) +
	# scale_x_continuous(trans = "log10", limits = c(10,22000)) +
	# scale_y_continuous(limits = c(-1000, 4000)) +
	# scale_y_continuous(limits = c(-850, 2500)) +
	# scale_x_continuous(trans = "log10") +
	# scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency") +
	my_theme + 
	# coord_fixed(ratio = 1) +
	# scale_color_discrete(name ="Corpus name", labels=c("ChildLEX", "SubtLEX", "Google Books")) + 
	theme(title = element_text(size = rel(1)),
				legend.position = c(.99, .99), legend.justification = c(1, 1), 
				legend.background = element_rect(color = "#B8B8B8", fill = "grey92"),
				legend.title = element_text(size = rel(.65)),
				legend.text = element_text(size = rel(.65)))
# ggsave(filename = paste("figures/", "rankplot-difs-nopanels-3.5-2.pdf", sep = ""), scale = 1, width = 8, height = 4)
# ggsave(filename = paste("figures/", "rankplot-difs-nopanels.pdf", sep = ""), scale = 1, width = 8, height = 4)
# ggsave(filename = paste("figures/", "rankplot-difs-nopanels-lemma.pdf", sep = ""), scale = 1, width = 8, height = 4)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
ggplot(data = df5ax, aes(x=xai, y=f)) +
# ggplot(data = df5ax, aes(x=xai, y=f, color = corpus)) +
	facet_wrap(~ a, nrow = 1, scales = "free") + 
	geom_line() +
	# scale_x_continuous(trans = "log10") +
	# scale_y_continuous(trans = "log10") +
	xlab("Rank") +
	ylab("Frequency") +
	my_theme + 
	# coord_fixed(ratio = 1) +
	# scale_color_discrete(name ="Corpus name", labels=c("ChildLEX", "SubtLEX", "Google Books")) + 
	# scale_x_continuous(trans = "log10") + #breaks = c(1, 5, 10, 50, 100, 500, 1000, 5000, 10000), 
										 #labels = scales::label_number(accuracy = 1), 
										 #expand = c(0, 0), 
										 #limits = c(1, 22000)) +	
	scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) +
	# scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
	# scale_y_log10() +
	# annotation_logticks() +
	# coord_equal() +
	theme(title = element_text(size = rel(1)),
				legend.position = c(.99, .99), legend.justification = c(1, 1), 
				legend.background = element_rect(color = "#B8B8B8", fill = "white"),
				legend.title = element_text(size = rel(.65)),
				legend.text = element_text(size = rel(.65)))
# ggsave(filename = paste("figures/", "rankplot-difs-3.5-2.pdf", sep = ""), scale = 1, width = 8, height = 4)
# ggsave(filename = paste("figures/", "rankplot-difs.pdf", sep = ""), scale = 1, width = 8, height = 4)
# ggsave(filename = paste("figures/", "rankplot-difs-lemma.pdf", sep = ""), scale = 1, width = 8, height = 4)


# save(df5a, file = "data-processed/rankfrequencypercorpus-3.5.RData")
# save(df5a, file = "data-processed/rankfrequencypercorpus.RData")