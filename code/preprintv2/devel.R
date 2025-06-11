my_theme = theme(
	title = element_text(size = rel(.75)),
	axis.title.x = element_text(size = rel(.9)),
	axis.title.y = element_text(size = rel(.9)),
	axis.text.x = element_text(size = rel(.7)),
	axis.text.y = element_text(size = rel(.7)),
	# legend.position = "none",
	panel.background = element_rect(fill = "white"),
	legend.background = element_rect(fill = "white"),
	panel.grid.minor = element_line(color = "#B8B8B8",
																	linewidth = 0.1,
																	linetype = 1), 
	panel.grid.major = element_line(color = "#B8B8B8",
																	linewidth = 0.3,
																	linetype = 1))

MIN_FREQUENCY = 10
MIN_FREQUENCY = 0
MIN_FREQUENCY_COR <- 0
MIN_FREQUENCY_JOINED = 0
MIN_FREQUENCY_LABEL = 17
MIN_FREQUENCY_LABEL = 2 # logplu1
MAX_FREQUENCY = 10001 # for plotting

library(dplyr)
library(tidyr)

# check ben's version
# devel_ben <- read_csv("data-processed/DeveL_ben.csv") 
# tibble(devel_ben)
# summary(devel_ben$subtlex_freq_log)
# colnames(devel_ben)
# a <- full_join(devel_ben, devel1, by = "word")
# cor.test(a$subtlex_freq_log, a$norm_log_subtlex)	

# from cormat.R
load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)
# load("data-processed/devel-avg-merged-lower.RData", verbose = T)
# load("data-processed/devel-avg-merged-nochildlexerr-plus1vars.RData", verbose = T)


# Plotting ----

# tibble(devel13)
devel1 <- devel13
df_long <- devel1 %>%
	select(norm_logcountplus1_chcount, norm_logcountplus1_ai_freq, word, contains("m") & contains("rt")) %>%
	pivot_longer(cols = !word & !norm_logcountplus1_ai_freq & !norm_logcountplus1_chcount, 
							 names_to = "var_name", values_to = "measure")
df_long2 <- df_long %>%
	pivot_longer(cols = !word & !measure & !var_name, 
							 names_to = "prod", values_to = "f_measure")
# df_long2

MIN_FREQUENCY_COR = 0
df_long2 %>% 
	filter(f_measure > MIN_FREQUENCY_COR) %>%
	summarise(.by = c(var_name, prod),
						r = cor(measure, f_measure, use = "complete.obs"),
						p = cor.test(measure, f_measure, use = "complete.obs")$p.value)
# df_long2



# df <- df %>%
#   rename_with(~ str_replace_all(., "rt", ""))
df_long3 <- df_long2 %>%
	mutate(var_name2 = fct_relabel(var_name, ~str_replace_all(., "rt.g", ""))) %>% 
	mutate(var_name2 = fct_relabel(var_name2, ~str_replace_all(., "rt.", ""))) %>% 
	mutate(var_name2 = fct_relabel(var_name2, ~str_replace_all(., ".m", ""))) %>% 
	filter(var_name2 != "dwds.type.freq")
# df_long3

summary(as.numeric(scale(log10(1+df_long3$measure))))
summary(as.numeric(scale(df_long3$measure)))
summary(df_long3)
summary(df_long2)
summary(df_long)

n <- 2
important1 <- df_long3 %>% 
	group_by(var_name2, prod) %>% 
	filter(f_measure > MIN_FREQUENCY_LABEL) %>%
	filter(!is.na(measure)) %>%
	filter(measure > MIN_FREQUENCY_LABEL) %>%
	mutate(lsqd2 = abs(max(log10(f_measure)) - (log10(f_measure)) - log10(measure))) %>% 
	slice_max(n = n, order_by = lsqd2)
important1
important2 <- df_long3 %>% 
	group_by(var_name2, prod) %>% 
	filter(f_measure > MIN_FREQUENCY_LABEL) %>%
	filter(measure > MIN_FREQUENCY_LABEL) %>%
	# mutate(lsqd = abs(log10(f_measure) - log10(measure))) %>%
	mutate(lsqd2 = abs(max(log10(f_measure)) - (log10(f_measure)) - log10(measure))) %>% 
	slice_min(n = n, order_by = lsqd2)
important <- rbind(important1, important2)
# important <- rbind(important1)
important

custom_labelsfacets <- c(
	"avg" = "Average across children",
	"1" = "Grade 1",
	"adlt" = "Grade 2",
	"ai_freq" = "Grade 3",
	"chcount" = "Grade 4",
	"chht" = "Grade 6",
	"oa" = "Older Adults",
	"ya" = "Younger Adults"
)

df_long3
summary(df_long3$var_name2)
# df_long3$measure <- log(df_long3$measure)
df_long3 %>%
	ggplot(aes(x = f_measure, y = measure, color = prod)) +
	geom_point(alpha = .1, size = 1) +
	# geom_smooth(se = F, linewidth = .5) + 
	geom_smooth(se = F, linewidth = .5, method = "lm") + 
	my_theme +
	# geom_text_repel(
	# 	data = important,
	# 	aes(label = word,
	# 			color = prod,
	# 			# colour="white", 
	# 			# segment.colour="red",
	# 			segment.size=.4),
	# 	size = 3,
	# 	# color = "red",
	# 	min.segment.length = 0,
	# 	# nudge_x = -.5,
	# 	# nudge_y = .5,
	# 	# force = 50,
	# 	max.iter = 5000,
	# 	verbose = T
	# ) + 
	scale_color_discrete(name ="Frequency\nmeasure", 
											 labels=c("LLM Exp1", "ChildLEX")) + 
	# labs(x = paste("Word type frequency:", expression(log((1 + frequency) * 10^6 / corpus_size)), sep = " "),, 
	labs(x = expression("Word type frequency: " * log((1 + frequency) * "*" * 10^6 / corpus_size)), 
			 y = "Lexical decision time") +
	theme(title = element_text(size = rel(1)),
				# legend.position = c(.99, .2), 
				legend.justification = c(1, 1),
				legend.position = "inside",
				legend.position.inside = c(.9, .25),
				# legend.justification = c("bottom", "right"),
				legend.background = element_rect(color = "#B8B8B8", fill = "white"),
				legend.title = element_text(size = rel(.65)),
				legend.text = element_text(size = rel(.65))) + 
	# scale_x_continuous(breaks = c(.1, 1, 10, 100, 1000), 
	# 									 labels = scales::label_number(accuracy = 1),
	# 									 expand = c(0.02, 0)) + 
	# scale_x_reverse(trans = "log10C", 
	# scale_x_reverse(#trans = "log10",
	scale_x_continuous(
										 # breaks = c(.1, 1, 10, 100, 1000),
										 labels = scales::label_number(accuracy = 1),
										 # limits = c(.1, MAX_FREQUENCY), # 1 for litkey, .1 for childlex
										 expand = c(0.02, 0)) + 
	# scale_y_continuous(trans = c("log10", "reverse"), # breaks = c(.1, 1, 10, 100, 1000),
	# scale_y_continuous(trans = c("log10"), # breaks = c(.1, 1, 10, 100, 1000),
	scale_y_continuous(#trans = c("log10"), # breaks = c(.1, 1, 10, 100, 1000),
										 # labels = scales::label_number(accuracy = 1),
										 # limits = c(.1, MAX_FREQUENCY),
										 expand = c(0.02, 0)) + 
	# coord_fixed() + 
	facet_wrap(~ var_name2, scales = "free", 
						 labeller = labeller(var_name2 = custom_labelsfacets))
# ggsave("figures/exp18log_lower.pdf", width = 10, height = 5)
# ggsave("figures/exp18log_lower10.pdf", width = 10, height = 5)
# ggsave("figures/exp18log.pdf", width = 10, height = 5)
# ggsave("figures/exp18.pdf", width = 10, height = 5)
ggsave("figures/exp18plus1-log.pdf", width = 10, height = 7)
ggsave("figures/exp18plus1-nolog.pdf", width = 10, height = 7)
