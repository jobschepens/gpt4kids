# install.packages("zipfR")
library(zipfR)
library(xtable)
a <- readxl::read_xlsx("data-processed/childlexstats.xlsx")
a
# load("data-processed/tidycorpus.RData", verbose = T) # lemmas on lines
# load("data-processed/tidycorpus-3.5.RData", verbose = T) # lemmas on lines
load("data-processed/tidycorpus-3.5-ch.RData", verbose = T) # lemmas on lines

# load("data-processed/ftable-lemmas.RData", verbose = T)
# load("data-processed/ftable-lemmas-3.5.RData", verbose = T)
load("data-processed/ftable-lemmas-3.5-ch.RData", verbose = T)
ftable
# load("data-processed/freqmat_l_lemma.RData", verbose = T)
load("data-processed/freqmat_l_lemma-3.5.RData", verbose = T)
f_lem <- freqmat_l
f_lem %>% arrange(desc(ai_freq)) %>% head(n = 10)
# load("data-processed/freqmat_l-types.RData", verbose = T) # 6549619    53241
# load("data-processed/freqmat_l-types-3.5.RData", verbose = T) # 6110176 45808    
load("data-processed/freqmat_l-types-3.5-PUNC.RData", verbose = T) # 6252808    46409    
f_typ <- freqmat_l

nrow(f_lem) # number of unique lemmas 40026 34375 34519
nrow(f_typ) # number of unique types all:53241 3.5:45691 2chwords: 45808
(a$`GPT 3.5`[a$Measure == "Types"] <- nrow(f_typ))
a

# load("data-processed/typefrequenciespercorpus.RData", verbose = T) # from rankplot
# df4$aif <- round((df4$ai_norm * 13106425/1000000), digits = 0)
# df4[df4$lemma.ai == "beginnen", ]

# sum(freqmat_l$ai_freq == 1)
df_tfl_l <- tfl(f_lem$ai_freq, type = f_lem$type)
df_spc_l <- tfl2spc(df_tfl_l)
tail(df_spc_l)
(a$`GPT 3.5`[a$Measure == "Lemmas"] <-
		attr(df_spc_l, "V")) # 40026 34375 34519
a

df_tfl_t <- tfl(f_typ$ai_freq, type = f_typ$type)
df_spc_t <- tfl2spc(df_tfl_t)
(a$`GPT 3.5`[a$Measure == "Tokens"] <-
		attr(df_spc_t, "N")) # 6549619 5567293 6110176
a
# (a$`GPT 3.5`[a$Measure == "Type/Token ratio"] <- 
# 		attr(df_spc_t, "V")/attr(df_spc_t, "N")) # .0082
# (a$`GPT 3.5`[a$Measure == "Lemma/Token ratio"] <- 
# 		attr(df_spc_l, "V")/attr(df_spc_t, "N")) # .0061

Vm(df_spc_l, 1) # number of lemmas occurring only once 13501 11378
Vm(df_spc_l, 2) # number of lemmas occurring 2 5203  4485
Vm(df_spc_t, 1) # number of types occurring only once 18169 15193

(a$`GPT 3.5`[a$Measure == "% Lemmas > 4"] <-
		100 * (1 - ((
			Vm(df_spc_l, 1) +
				Vm(df_spc_l, 2) +
				Vm(df_spc_l, 3) +
				Vm(df_spc_l, 4)
		) / nrow(f_lem)))) # number of lemmas occurring 4 or less # 1- (23696 / 40026) = 0.41
a
(a$`GPT 3.5`[a$Measure == "% Types > 4"] <-
		100 * (1 - ((
			Vm(df_spc_t, 1) +
				Vm(df_spc_t, 2) +
				Vm(df_spc_t, 3) +
				Vm(df_spc_t, 4)
		) / nrow(f_typ)))) # number of types occurring 4 or less # 1- (31640 / 53241) = .41
a
(a$`GPT 3.5`[a$Measure == "% Tokens > 4"] <-
		100 * (1 - ((
			Vm(df_spc_t, 1) +
				Vm(df_spc_t, 2) +
				Vm(df_spc_t, 3) +
				Vm(df_spc_t, 4)
		) / attr(df_spc_t, "N")))) # .99
a

(a$`GPT 3.5`[a$Measure == "% Hapax tokens"] <-
		100 * Vm(df_spc_t, 1) / N(df_spc_t)) # hapax by sample size .27
(a$`GPT 3.5`[a$Measure == "% Hapax types"]  <-
		100 * Vm(df_spc_t, 1) / nrow(f_typ)) # hapaxes out of all types 34.12
(a$`GPT 3.5`[a$Measure == "% Hapax lemmas"] <-
		100 * Vm(df_spc_l, 1) / nrow(f_lem)) # hapaxes out of all lemmas 33.7
a

# (a$`GPT 3.5` <- round(as.double(a$`GPT 3.5`), digits = 2))
# (a$`GPT 3.5`[c(1,2,3,4)] <- round(as.double(a$`GPT 3.5`)[c(1,2,3,4)], digits = 0))

a$`GPT 3.5` <- as.double(a$`GPT 3.5`)
mdat1 <- matrix(0, nrow = 4, ncol = 4)

mdat2 <- matrix(2, nrow = 6, ncol = 4)

mdat <- rbind(mdat1, mdat2)
# mdat<-matrix(2,nrow=10,ncol=4);
xtable(a, digits = mdat)

print(
	xtable(a, digits = mdat),
	include.rownames = F,
	type = "latex",
	file = "data-processed/out.tex"
)
print(xtable(a, digits = mdat),
			include.rownames = F,
			type = "latex")
a
