library(xtable)
conditions = c("adlt", "adht", "chlt", "chht")
for(cond in conditions) {
	print(cond)

	# load("data-processed/merged_df-type-3.5-full.RData", verbose = T)
	load(paste0("data-processed/merged_df-type-", cond, ".RData"), verbose = T)
		
	### low in #### 
	
	merged_df <- merged_dfx
	merged_df <- merged_df %>%
		mutate(word_length = nchar(type)) %>%
		arrange(desc(word_length))
	
	c <- merged_df %>% 
		arrange(desc(childlex_norm)) %>% 
		arrange(ai_norm) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("ChildLEX" = type)
	
	d <- merged_df %>% 
		arrange(desc(childlex_norm)) %>% 
		arrange(ai_norm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("ChildLEX >10" = type)
	
	a <- merged_df %>% 
		arrange(desc(ai_norm)) %>% 
		arrange(childlex_norm) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("GPT" = type)
	
	b <- merged_df %>% 
		arrange(desc(ai_norm)) %>% 
		arrange(childlex_norm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("GPT >10" = type)
	
	lowin <- cbind(c, d, a,b)
	lowin
	
	#### not in #### 
	
	merged_df$ai_norm[is.na(merged_df$ai_norm)] = .1
	merged_df$childlex_norm[is.na(merged_df$childlex_norm)] = .1
	# merged_df <- merged_df %>% slice_sample(n = 300)
	
	c <- merged_df %>% 
		arrange(desc(childlex_norm)) %>% 
		arrange(ai_norm) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("ChildLEX" = type)
	
	d <- merged_df %>% 
		arrange(desc(childlex_norm)) %>% 
		arrange(ai_norm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("ChildLEX >10" = type)
	
	a <- merged_df %>% 
		arrange(desc(ai_norm)) %>% 
		arrange(childlex_norm) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("GPT" = type)
	
	b <- merged_df %>% 
		arrange(desc(ai_norm)) %>% 
		arrange(childlex_norm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
		select(type) %>% 
		rename("GPT >10" = type)
		# summarise(Words = str_flatten(type, collapse = ", ")) %>%
		# mutate("Only in" = "GPT3.5",
					 # "Word length" = ">10", .before = 1)
	
	
	notin <- cbind(c, d, a,b)
	
	xtable(notin)
	xtable(lowin)
	
	cat(paste("Condition:", cond, "\n"),
				file = paste0("data-processed/notin",".tex"), append = T)
	print(xtable(notin),
				include.rownames = F,
				type = "latex",
				file = paste0("data-processed/notin",".tex"), append = T)
	
	cat(paste("Condition:", cond, "\n"),
				file = paste0("data-processed/lowin",".tex"), append = T)
	print(xtable(lowin),
				include.rownames = F,
				type = "latex",
				file = paste0("data-processed/lowin",".tex"), append = T)
}