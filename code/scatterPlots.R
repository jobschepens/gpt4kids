scatterPlotFacets <- function(temp, x_label, y_label, datap) {
	# temp = a
	n <- 5
	important1 <- temp %>% group_by(corpus) %>%  
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		mutate(lsqd2 = log10(1+x) - log10(1+y)) %>% 
		slice_max(n = n, order_by = lsqd2)
	important2 <- temp %>% group_by(corpus) %>%  
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		mutate(lsqd2 = log10(1+x) - log10(1+y)) %>% 
		slice_min(n = n, order_by = lsqd2)
	important <- rbind(important1, important2)
	print(important)
	custom_labels <- c("norm_logcountplus1_adht" = "Adults High Temperature",
										 "norm_logcountplus1_adlt" = "Adults Low Temperature",
										 "norm_logcountplus1_chht" = "Children High Temperature",
										 "norm_logcountplus1_chlt" = "Children Low Temperature")
	labels_a <- c("x" = "childLex",
								"y" = "LLM")
	
	ggplot(temp, aes(x = x, y = y)) +
		geom_abline(color="#B8B8B8", linewidth = .3, linetype = 1) +
		facet_wrap(vars(corpus), labeller = as_labeller(custom_labels)) + 
		scale_color_discrete(name ="Corpus name", labels=labels_a) +
		geom_bin_2d(bins = 100, show.legend = T) + 
		scale_fill_viridis(limits=c(-2, datap), trans = "log10", name = "Count") +
		geom_label_repel(
			data = important,
			aes(label = type, colour="white",segment.colour="red",segment.size=.4),
			size = 3, color = "red", min.segment.length = .5, max.overlaps = 10000,
			max.iter = 50000, verbose = T) + 
		my_theme +
		coord_fixed(ratio = 1) +
		labs(x = x_label, y = y_label)  + 
		theme(
			plot.margin = margin(.5, .5, .5, .5, "cm"), 
			legend.position = "bottom", 
			legend.justification = "centre",
			legend.direction = "horizontal",
			legend.background = element_rect(color = "white", fill = "white"),
			legend.title = element_text(size = rel(.8)),
			legend.text = element_text(size = rel(.55))) + 
		scale_x_continuous(# trans = "log10", 
											 # breaks = c(.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # labels = c(0.1, 1, 5, 10, 50, 100, 500, "1k", "5k", "10k", "50k"),
											 # limits = c(.05, MAX_FREQUENCY), + # 2.5 for litkey, .05 for childlex
											 # limits = c(-2, 12),
											 expand = c(0, 0)) +
		scale_y_continuous(# trans = "log10", 
											 # breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # limits = c(0.05, MAX_FREQUENCY),
											 # limits = c(-2, 12),
											 expand = c(0, 0))
}


scatterPlotFacetsZoom <- function(temp = merged_df, x_label, y_label, datap){
	n <- 5
	important1 <- temp %>% group_by(corpus) %>%  
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		mutate(lsqd2 = log10(1+x) - log10(1+y)) %>% 
		slice_max(n = n, order_by = lsqd2)
	important2 <- temp %>% group_by(corpus) %>%  
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		mutate(lsqd2 = log10(1+x) - log10(1+y)) %>% 
		slice_min(n = n, order_by = lsqd2)
	important <- rbind(important1, important2)
	print(important)
	custom_labels <- c("norm_logcountplus1_adht" = "Adults High Temperature",
										 "norm_logcountplus1_adlt" = "Adults Low Temperature",
										 "norm_logcountplus1_chht" = "Children High Temperature",
										 "norm_logcountplus1_chlt" = "Children Low Temperature")
	labels_a <- c("x" = "childLex",
								"y" = "LLM")
	
	ggplot(temp, aes(x = x, y = y)) +
		geom_abline(color="#B8B8B8", linewidth = .3, linetype = 1) +
		facet_wrap(vars(corpus), labeller = as_labeller(custom_labels)) + 
		scale_color_discrete(name ="Corpus name", labels=labels_a) +
		geom_bin_2d(bins = 100, show.legend = T) + 
		scale_fill_viridis(limits=c(-2, datap), trans = "log10", name = "Count") +
		geom_label_repel(
			data = important,
			aes(label = type, colour="white",segment.colour="red",segment.size=.4),
			size = 3, color = "red", min.segment.length = .5, max.overlaps = 10000,
			max.iter = 50000, verbose = T) + 
		my_theme +
		coord_fixed(ratio = 1) +
		labs(x = x_label, y = y_label)  + 
		theme(
			plot.margin = margin(.5, .5, .5, .5, "cm"), 
			legend.position = "bottom", 
			legend.justification = "centre",
			legend.direction = "horizontal",
			legend.background = element_rect(color = "white", fill = "white"),
			legend.title = element_text(size = rel(.8)),
			legend.text = element_text(size = rel(.55))) + 
		scale_x_continuous(# trans = "log10", 
											 # breaks = c(1000, 5000, 10000, 50000),
											 # labels = c("1k", "5k", "10k", "50k"),
											 # limits = c(1000, MAX_FREQUENCY), + # 2.5 for litkey, .05 for childlex
											 expand = c(0, 0)) + 
		scale_y_continuous(# trans = "log10", 
											 # breaks = c(1000, 5000, 10000, 50000),
											 # labels = c(1000, 5000, 10000, 50000),
											 # limits = c(1000, MAX_FREQUENCY),
											 expand = c(0, 0))}

scatterPlot <- function(temp, x_label, y_label, datap){
	# temp = a
	# x_label <- "ChildLex-based (6-12 y/o children) type frequency (ppm)"
	# y_label <- "LLM-based type frequency (ppm)"
	# temp <- merged_df %>% dplyr::select(type, ai_norm, childlex_norm) %>% rename("type" = 1, "x" = 2, "y" = 3)
	n <- 5
	important1 <- temp %>% 
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		# mutate(lsqd = abs(log10(x) - log10(y))) %>%
		mutate(lsqd2 = log10(x) - log10(y)) %>% 
		slice_max(n = n, order_by = lsqd2)
	important2 <- temp %>% 
		filter(x > MIN_FREQUENCY_LABEL) %>%
		filter(y > MIN_FREQUENCY_LABEL) %>%
		# mutate(lsqd = abs(log10(x) - log10(y))) %>%
		mutate(lsqd2 = log10(x) - log10(y)) %>% 
		slice_min(n = n, order_by = lsqd2)
	important <- rbind(important1, important2)
	
	# Compute 2d density estimate 
	# also works, but less beautiful
	# library(MASS)
	# dens <- MASS::kde2d(log10(temp$x), log10(temp$y), n = 200)
	# dens_df <- data.frame(expand.grid(x = 10^dens$x, y = 10^dens$y), z = as.vector(dens$z))
	# quantile_level <- quantile(dens_df$z, probs = 0.82)
	
	## better, but removed for now
	# library(ks)
	# 	# d <- temp %>% dplyr::select(x, y) %>% mutate(x = log10(x), y = log10(y))
	# 	d <- temp %>% dplyr::select(x, y)
	#   # kd <- ks::kde(d, compute.cont=TRUE, xmin = c(0,0), positive = T)
	#   kd <- ks::kde(d, compute.cont=TRUE, h=.01, positive = T, xmin = c(.1,.1))
	#   get_contour <- function(kd_out=kd, prob="5%") {
	#   	kd_out = kd
	#   	contour_95 <- with(kd_out, contourLines(
	#   		x=eval.points[[1]],
	#   		y=eval.points[[2]],
	#   		z=estimate,
	#   		levels=cont[prob])[[1]])
	#   	as_tibble(contour_95) %>% mutate(prob = as.factor(prob))
	#   }
	#   # sum(temp$x < 5) / length(temp$x)
	#   dat_out <- map_dfr(c("1%", "10%", "50%", "70%"), ~get_contour(kd, .)) %>%
	#   	group_by(prob) %>%
	#   	mutate(n_val = 1:n()) %>%
	#   	ungroup()
	#  	dat_out2 <- filter(dat_out, !n_val %in% 1:3)
	
	# summary(dat_out)
	## clean kde output
	# kd_df <- expand_grid(x=kd$eval.points[[1]], 
	# 										 y=kd$eval.points[[2]]) %>% 
	# 	mutate(z = c(kd$estimate %>% t))
	# kd_df
	
	ggplot(temp, aes(x = x, y = y)) +
		## AB LINE 
		geom_abline(color="#B8B8B8", linewidth = .3, linetype = 1) +
		# facet_wrap(vars(corpus)) + 
		
		## POINTS
		# geom_jitter(color="grey40", alpha = .1, size = .1, width = 0.12, height = 0.12) +  # add jitter
		# geom_pointdensity(size = .5, adjust = 1) + scale_color_viridis_c() +
		geom_bin_2d(bins = 100, show.legend = T) + 
		# scale_fill_continuous(type = "viridis") +
		# scale_fill_viridis(limits=c(0,50), breaks=seq(0, 40, by=10)) + 
		# scale_fill_viridis(limits=c(0,512), breaks=rev(512 - c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256))) +
		# scale_fill_viridis(limits=c(0,50), breaks=c(0, 1, 2, 4, 8, 16, 32)) + 
		# datap = 50
		scale_fill_viridis(limits=c(-2, datap), trans = "log10", name = "Count") +
		# scale_fill_gradientn(limits=c(0,50), breaks=seq(0, 40, by=10), colours=rainbow(4)) + 
		
		## SMOOTH 
		# geom_smooth(method = "loess", span = 1, se = F, linewidth = .4, colour = I("red")) +
		# geom_smooth(method = "lm", se = T, linewidth = .4, colour = I("red")) +
		
		## CONTOUR
		# less nice
		# geom_contour(data = dens_df, 
		# 						 aes(x = x, y = y, z = z), 
		# 						 breaks = quantile_level,
	# 						 linewidth = .5) +
	
	# nicer
	# geom_path(aes(x, y, group = prob),
	# 					data = dat_out2, colour = I("green"),
	# 					linewidth = .4) +
	# geom_text(aes(label = prob),
	# 					data = filter(dat_out, (prob %in% c("1%", "10%", "50%") & n_val==331)), 
	# 					colour = I("blue"), 
	# 					size = I(5)) +
	# filter(dat_out, (prob %in% c("1%") & n_val==10))
	
	## DENSITY
	# stat_ellipse(linewidth = 2, color = "red", type = "norm", linetype = 2) +
	# stat_ellipse(linewidth = 2, color = "green", type = "t", linetype = 2) +
	# stat_ellipse(linewidth = .5, color = "red", type = "norm", linetype = 2, size = ) +
	# stat_ellipse(aes(x = x, y = x), linewidth = .5, color = "blue") +
	# stat_ellipse(type = "euclid", linewidth = .5, level = 2, color = "green") +
	# geom_density_2d(linewidth = .5, color = "red", type = "t", linetype = 1, bins = 1000) + 
	# 	geom_density_2d(
	#     mapping = aes(color = after_stat(level),
	#     							x = log10(x), y = log10(y)),  # Color the contours by their density levels
	#     # size = c(0.1, 2),  # Set the sizes of the contour lines
	#     binwidth = .1,
	#     linewidth = 0.5,  # Set the sizes of the contour lines
	#     n = 100  # Number of points to use for density estimation
	#   ) +
	# scale_color_gradient(low = "blue", high = "red") +  # Color scale for contours
	
	## WORDS
		
	geom_text_repel(
		data = important,
		aes(label = type,
				colour="white", 
				segment.colour="red",
				segment.size=.4),
		size = 3,
		color = "red",
		min.segment.length = .5,
		# nudge_x = -.5,
		# nudge_y = .5,
		# force = 50,
	  max.overlaps = 10000,
		max.iter = 5000,
		verbose = T
	) + 
		
		my_theme +
		coord_fixed(ratio = 1) +
		# theme(aspect.ratio=1)
		labs(x = x_label, y = y_label)  + 
		theme(
			plot.margin = margin(.5, .5, .5, .5, "cm"), 
			# plot.margin=grid::unit(c(0,0,0,0), "mm"),
			# title = element_text(size = rel(1)),
			# legend.title.align = 0,
			legend.position = "bottom", 
			# legend.position = c(.99, .99), legend.justification = c(1, 1), 
			legend.justification = "centre",
			legend.direction = "horizontal",
			legend.background = element_rect(color = "white", fill = "white"),
			legend.title = element_text(size = rel(.8)),
			# legend.key.size = element_text(size = rel(.65)),
			# legend.key = element_text(size = rel(.65)),
			legend.text = element_text(size = rel(.55))) + 
		scale_x_continuous(# trans = "log10", 
											 # breaks = c(5, 10, 50, 100, 500, 1000, 5000, 10000),
											 # breaks = c(.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # labels = c(0.1, 1, 5, 10, 50, 100, 500, "1k", "5k", "10k", "50k"),
											 # limits = c(.05, MAX_FREQUENCY),
											 limits = c(-2, 12),
											 expand = c(0, 0)) + # 2.5 for litkey, .05 for childlex
		# limits = c(2.5, MAX_FREQUENCY)) + # 2.5 for litkey, .05 for childlex
		scale_y_continuous(# trans = "log10", 
											 # breaks = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # labels = c(0.1, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000),
											 # limits = c(0.05, MAX_FREQUENCY),
											 limits = c(-2, 12),
											 expand = c(0, 0))

}




