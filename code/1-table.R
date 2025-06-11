options(scipen = 999)

# create stats table
# first run tm_ben.Rmd and udpipe.R

# install.packages("zipfR")
library(zipfR)
library(xtable)
library(tidyverse)

a <- readxl::read_xlsx("data-original/childLex/childlexstats.xlsx")
a

# exp 2
# conditions = c("adlt", "adht", "chlt", "chht")
# conditions = c("adlt", "adht")
# exp 3
# conditions = c("lllo", "dslo", "chlt", "chht")
conditions = c("lllo", "llsh", "dslo", "dssh")
conditions = c("lll2", "lls2", "dsl2", "dss2")

combined <- NULL
# combined[1:10, ] <- NA

for(cond in conditions) {
	print(cond)
	# cond = "dslo"

  # LEMMAS ftable from freqmat_l from udpipe.R
  # load("data-processed/freqmat_l_lemma-3.5.RData", verbose = T)
  load(paste0("data-processed/freqmat_l-lemma-", cond, ".RData"), verbose = T) # used for table
  f_lem <- freqmat_l
  # f_lem %>% arrange(desc(ai_freq)) %>% head(n = 10)
  
  # TYPES: freqmat_l from tm_ben.Rmd
  # load("data-processed/freqmat_l-types-3.5-PUNC.RData", verbose = T) # 6252808    46409
  load(paste0("data-processed/freqmat_l-types-", cond, ".RData")) #
  f_typ <- freqmat_l
  
  load("data-processed/du.RData", verbose = T) #
  du <- du %>% rename(ai_freq = subtlex, type = Word)
  f_typ <- du
  
  nrow(f_lem) # number of unique lemmas 40026 34375 34519
  nrow(f_typ) # number of unique types all:53241 3.5:45691 2chwords: 45808, lllo 51333
  (a$LLM[a$Measure == "Types"] <- nrow(f_typ))
  
  # type frequency list
  # sum(freqmat_l$ai_freq == 1)
  df_tfl_l <- tfl(f_lem$ai_freq, type = f_lem$type)
  df_spc_l <- tfl2spc(df_tfl_l)
  tail(df_spc_l)
  (a$LLM[a$Measure == "Lemmas"] <-
  		attr(df_spc_l, "V")) # 40026 34375 34519
  
  df_tfl_t <- tfl(f_typ$ai_freq, type = f_typ$type)
  df_spc_t <- tfl2spc(df_tfl_t)
  (a$LLM[a$Measure == "Tokens"] <-
  		attr(df_spc_t, "N")) # 6549619 5567293 6110176 (llllo 10.333.059)
  
  # (a$LLM[a$Measure == "Type/Token ratio"] <- 
  # 		attr(df_spc_t, "V")/attr(df_spc_t, "N")) # .0082
  # (a$LLM[a$Measure == "Lemma/Token ratio"] <- 
  # 		attr(df_spc_l, "V")/attr(df_spc_t, "N")) # .0061
  
  Vm(df_spc_l, 1) # number of lemmas occurring only once 13501 11378
  Vm(df_spc_l, 2) # number of lemmas occurring 2 5203  4485
  Vm(df_spc_t, 1) # number of types occurring only once 18169 15193 (lllo 17260)
  
  # number of lemmas occurring 4 or less # 1- (23696 / 40026) = 0.41
  (a$LLM[a$Measure == "% Lemmas > 4"] <-
  		100 * (1 - ((
  			Vm(df_spc_l, 1) +
  				Vm(df_spc_l, 2) +
  				Vm(df_spc_l, 3) +
  				Vm(df_spc_l, 4)
  		) / nrow(f_lem)))) 
  
  # number of types occurring 4 or less # 1- (31640 / 53241) = .41
  (a$LLM[a$Measure == "% Types > 4"] <-
  		100 * (1 - ((
  			Vm(df_spc_t, 1) +
  				Vm(df_spc_t, 2) +
  				Vm(df_spc_t, 3) +
  				Vm(df_spc_t, 4)
  		) / nrow(f_typ)))) 
   
  (a$LLM[a$Measure == "% Tokens > 4"] <-
  		100 * (1 - ((
  			Vm(df_spc_t, 1) +
  				Vm(df_spc_t, 2) +
  				Vm(df_spc_t, 3) +
  				Vm(df_spc_t, 4)
  		) / attr(df_spc_t, "N")))) # .99
  
  (a$LLM[a$Measure == "% Hapax tokens"] <-
  		100 * Vm(df_spc_t, 1) / N(df_spc_t)) # hapax by sample size .27
  (a$LLM[a$Measure == "% Hapax types"]  <-
  		100 * Vm(df_spc_t, 1) / nrow(f_typ)) # hapaxes out of all types 34.12
  # 38.21451 adht
  # 38.20602 chht
  (a$LLM[a$Measure == "% Hapax lemmas"] <-
  		100 * Vm(df_spc_l, 1) / nrow(f_lem)) # hapaxes out of all lemmas 33.7
  
  # (a$LLM <- round(as.double(a$LLM), digits = 2))
  # (a$LLM[c(1,2,3,4)] <- round(as.double(a$LLM)[c(1,2,3,4)], digits = 0))
  
  a$LLM <- as.double(a$LLM)
  mdat1 <- matrix(0, nrow = 4, ncol = 4)
  mdat2 <- matrix(2, nrow = 6, ncol = 4)
  mdat <- rbind(mdat1, mdat2)
  # mdat<-matrix(2,nrow=10,ncol=4);
  # xtable(a, digits = mdat)
  
  r <- a %>% rename_with(~ cond, "GPT 3.5")
  r
  
  if (is.null(combined)) {
  	combined <- r[, 3, drop = FALSE]
  	# combined <- r
  } else {
  	combined <- cbind(combined, r[, 3, drop = FALSE])
  }
  combined
}

combined <- cbind(a[,1:2], combined)

ncol = 5 
# ncol = 2 
mdat1 <- matrix(0, nrow = 4, ncol = ncol+2)
mdat2 <- matrix(2, nrow = 6, ncol = ncol+2)
mdat <- rbind(mdat1, mdat2)


# html
print(xtable(combined, digits = mdat),
      include.rownames = F,
      type = "html",
      file = paste0("data-processed/combined", "2", ".html"))



# tex
print(xtable(combined, digits = mdat),
      include.rownames = F,
			type = "latex",
			file = paste0("data-processed/combined", ".tex"))

round(combined[,-1],2)
combined
