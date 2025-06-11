# plot transformations -----

# load(devel1, file = "data-processed/devel-avg-merged.RData", verbose = T)

# choose panels
# tibble(df_long)
# tibble(devel1)

df_long <- devel1 %>%
  select(word, 
         contains("value_"), 
         contains("norm_log_"), 
         contains("norm_logcountplus1"),
         contains("norm_logcountplus1prob"),
         contains("norm_lognonorm")) %>%
  pivot_longer(cols = !word, names_to = "var_name", values_to = "measure")

# Transform the data frame
df_transformed <- df_long %>%
  mutate(
    category = case_when(
      startsWith(var_name, "value_") ~ "value_",
      startsWith(var_name, "norm_lognonorm_") ~ "norm_lognonorm_",
      startsWith(var_name, "norm_logcountplus1prob_") ~ "norm_logcountplus1prob_",
      startsWith(var_name, "norm_logcountplus1_") ~ "norm_logcountplus1_",
      startsWith(var_name, "norm_log_") ~ "norm_log_"
    ),
    remainder = case_when(
      category == "value_" ~ str_remove(var_name, "value_"),
      category == "norm_lognonorm_" ~ str_remove(var_name, "norm_lognonorm_"),
      category == "norm_logcountplus1prob_" ~ str_remove(var_name, "norm_logcountplus1prob_"),
      category == "norm_logcountplus1_" ~ str_remove(var_name, "norm_logcountplus1_"),
      category == "norm_log_" ~ str_remove(var_name, "norm_log_")
    )
  ) %>% 
  select(-var_name) %>%
  filter(remainder != "dwds")

# all_longerl <- all_longer %>% group_by(group) %>% 
# all_longerl <- all_longer2 %>% group_by(group) %>% 
# 	mutate(norm = 1000000 * value / (sum(value))) %>%
# 	mutate(normplus1 = 1000000 * (value + 1) / (sum(value))) %>%
# 	mutate(normplus1prob = (value + 1) / (sum(value) + 2)) %>%
# 	mutate(norm_log = log(1 + norm)) %>% 
# 	mutate(norm_logcountplus1 = log( normplus1)) %>% 
# 	mutate(norm_logcountplus1prob = -log( normplus1prob)) %>%
# 	mutate(norm_lognonorm = log( 1 + value ))
# summary(all_longerl)

custom_labelscolor <- c(
  "norm_log_" = expression(log(1 + frequency * "*" * 10^6 / corpus_size)),
  "norm_logcountplus1_" = expression(log((1 + frequency) * "*" * 10^6 / corpus_size)),
  "norm_logcountplus1prob_" = expression(-log((1 + frequency) * "*" * 1 / corpus_size)),
  "norm_lognonorm_" = expression(log(1 + frequency)))

custom_labelscolor2 <- c(
  "norm_log_" = "log(1 + frequency * 10^6 / corpus_size)",
  "norm_logcountplus1_" = "log((1 + frequency) * 10^6 / corpus_size)",
  "norm_logcountplus1prob_" = "-log((1 + frequency) * 1 / corpus_size)",
  "norm_lognonorm_" = "log(1 + frequency))")

custom_labelsfacets <- c(
  "dwds" = "DWDS",
  "adht" = "Adults High Temp.",
  "adlt" = "Adults Low Temp.",
  "ai_freq" = "Exp 1",
  "chcount" = "childLex",
  "chht" = "Children High Temp.",
  "chlt" = "Children Low Temp.",
  "subtlex" = "SUBTLEX"
)

# pivot wider such that value becomes a variable that i can use for a scatter plot

# Specify order of facets
desired_order_facets <- c("ai_freq", "adht", "adlt", "chht", "chlt", "chcount", "subtlex")
desired_order_trans <- c("norm_logcountplus1prob_", "norm_lognonorm_", "norm_log_", "norm_logcountplus1_")


## hist plot  ---- 


bw <- 0.2  # Adjust as necessary
df_transformedv <- df_transformed %>% filter(category != "value_") %>% as_tibble()

df_transformedv$category <- factor(df_transformedv$category, levels = desired_order_trans)
# df_transformedv$remainder <- factor(df_transformedv$remainder)
df_transformedv$remainder <- factor(df_transformedv$remainder, levels = desired_order_facets)

histogram_plot <- ggplot(df_transformedv, aes(x = measure)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = bw, alpha = 0.5, position = "identity") +
  facet_grid(category ~ remainder, scales = "free",
             labeller = labeller(category = custom_labelscolor2, 
                                 remainder = custom_labelsfacets)) +
  theme_minimal() +
  xlab("Transformed Frequency") +
  ylab("Density") +
  theme(strip.text.y = element_text(angle = 0))
# histogram_plot
ggsave(histogram_plot, file = "figures/histogram_plot_devel.pdf", width = 14, height = 8)

# generate a figure caption
histogram_plot_caption <- "Histograms of transformed frequency values. The x-axis shows the transformed frequency values, and the y-axis shows the density of the values. The facets show the different transformations and the different corpora. The transformations are: log(1 + frequency * 10^6 / corpus_size), log((1 + frequency) * 10^6 / corpus_size), -log((1 + frequency) * 1 / corpus_size), and log(1 + frequency). The corpora are: DWDS, Adults High Temp., Adults Low Temp., Children High Temp., Children Low Temp., childLex, and SUBTLEX."


## scatter plot  ---- 

df_transformed2 <- df_transformed %>%
  pivot_wider(names_from = category, values_from = measure)

df_transformed3 <- df_transformed2 %>% 
  pivot_longer(cols = c("norm_log_", "norm_logcountplus1_", "norm_logcountplus1prob_", "norm_lognonorm_"), 
               names_to = "category", values_to = "yvalue_")

df_transformed4 <- df_transformed3 %>%
  filter(category == "norm_log_" | 
           category == "norm_logcountplus1_" | 
           category == "norm_logcountplus1prob_" |
           category == "norm_lognonorm_")


# Modify the factor levels of remainder to control facet order
df_transformed4$remainder <- factor(df_transformed4$remainder, levels = desired_order_facets)

df_transformed4$category <- factor(df_transformed4$category, 
                                   levels = c("norm_logcountplus1prob_", 
                                              "norm_lognonorm_", 
                                              "norm_log_", 
                                              "norm_logcountplus1_"))

scatter_plot <- ggplot(df_transformed4, aes(x = value_, y = yvalue_, color = category)) +
  geom_point(size = .5) +
  geom_line() +
  facet_grid( ~ remainder, scales = "free", 
              labeller = labeller(remainder = custom_labelsfacets)) +
  scale_color_discrete(name = "Transformation Type",  
                       labels = custom_labelscolor) + 
  scale_x_continuous(limits = c(0, 50)) +
  # add axes labels
  xlab("Frequency") +
  ylab("Transformed Frequency") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "bottom")
scatter_plot
ggsave(scatter_plot, file = "figures/scatter_plot_devel.pdf", width = 11, height = 5)


## scatter plot zoom out ---- 


# df_transformed5 <- df_transformed4 %>%
# 	# mutate only for category is norm_logcountplus1_ using case when
# 	mutate(value_ = case_when(
# 		category == "norm_logcountplus1_" ~ value_ + 1000,
# 		category == "norm_lognonorm_" ~ value_ - 1000,
# 		TRUE ~ value_
# 	))


scatter_plot2 <- ggplot(df_transformed4, aes(x = value_, y = yvalue_, color = category)) +
  geom_point(size = .5) +
  geom_line() +
  # geom_jitter(width = 300, height = 0) +  # Add horizontal jitter with controlled width
  facet_grid( ~ remainder, scales = "free", 
              labeller = labeller(remainder = custom_labelsfacets)) +
  scale_color_discrete(name = "Transformation Type",  
                       labels = custom_labelscolor) +  
  xlab("Frequency") +
  ylab("Transformed Frequency") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "bottom")
# scatter_plot2
ggsave(scatter_plot2, file = "figures/scatter_plot2_devel.pdf", width = 11, height = 5)

# generate a caption
scatter_plot_caption <- "Scatter plot of transformed frequency values. The x-axis shows the frequency values, and the y-axis shows the transformed frequency values. The facets show the different transformations and the different corpora. The transformations are: log(1 + frequency * 10^6 / corpus_size), log((1 + frequency) * 10^6 / corpus_size), -log((1 + frequency) * 1 / corpus_size), and log(1 + frequency). The corpora are: DWDS, Adults High Temp., Adults Low Temp., Children High Temp., Children Low Temp., childLex, and SUBTLEX."


# Differences between cor mats ----

# # Calculate the Frobenius norm of the difference
# frobenius_norm <- norm(correlation_matrix - correlation_matrixno0, type = "F")
# frobenius_norm
# 
# # Conduct a test for significant difference using the Jennrich Test
# library(psych)
# jennrich_result <- cortest.jennrich(correlation_matrix, correlation_matrixno0, n1 = 300000, n2 = 300000) # Assuming sample size 100
# jennrich_result
# 
# # Difference Heatmap
# diff_matrix <- correlation_matrix - correlation_matrixno0
# library(reshape2)
# diff_melted <- melt(diff_matrix)
# 
# ggplot(diff_melted, aes(Var1, Var2, fill = value)) +
# 	geom_tile() +
# 	scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.1, 0.1), name = "Difference") +
# 	theme_minimal() +
# 	ggtitle("Difference Heatmap between Matrix 1 and Matrix 2") +
# 	xlab("") +
# 	ylab("") +
# 	theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Dendrogram visualization
# hc1 <- hclust(as.dist(1 - correlation_matrix))
# hc2 <- hclust(as.dist(1 - correlation_matrixno0))
# par(mfrow = c(1, 2))
# plot(hc1, main = "Dendrogram for Matrix 1")
# plot(hc2, main = "Dendrogram for Matrix 2")
# par(mfrow = c(1, 1))

# dendrogram is the same, but the correlation matrix is different