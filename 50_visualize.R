# 50_visualize_clean.R — unique & complementary visualizations
source("R/00_setup.R")

library(ggplot2)
library(dplyr)
library(tidyr)
library(fs)

out_dir <- here::here("data","processed")
fig_dir <- here::here("figures")
fs::dir_create(fig_dir)

# Load processed data
freq <- readr::read_rds(file.path(out_dir,"emotion_freq.rds"))
pairs <- readr::read_rds(file.path(out_dir,"emotion_pairs.rds"))
bylen <- readr::read_rds(file.path(out_dir,"length_by_emotion.rds"))
lv_by_emo <- readr::read_rds(file.path(out_dir,"lexdiv_by_emotion.rds"))
compare_tbl <- readr::read_rds(file.path(out_dir,"dict_vs_labels.rds"))
toks <- readr::read_rds(file.path(out_dir,"tokens.rds"))
tidy_labels <- readr::read_rds(file.path(out_dir,"goemotions_tidy_labels.rds"))

# ---------------------------
# 1) Dictionary vs labels scatter (joy example)
# ---------------------------
if ("rate_joy" %in% names(compare_tbl)) {
  p1 <- ggplot(compare_tbl, aes(x=rate_joy, y=as.numeric(emotion=='joy'))) +
    geom_jitter(alpha=.2, width=.3, height=.05) +
    labs(x="NRC joy rate per 100 tokens", 
         y="Has annotated 'joy' (0/1)",
         title="Dictionary signal vs annotated emotion (joy example)")
  ggsave(file.path(fig_dir,"01_dict_vs_labels_joy.png"), p1, width=7, height=5, dpi=200)
}

# ---------------------------
# 2) Network graph of top co-occurring emotions (top 100 pairs)
# ---------------------------
if (nrow(pairs) > 0) {
  if (!requireNamespace("igraph", quietly=TRUE)) install.packages("igraph")
  if (!requireNamespace("ggraph", quietly=TRUE)) install.packages("ggraph")
  library(igraph)
  library(ggraph)
  
  top_pairs <- head(pairs, 100)
  g <- graph_from_data_frame(top_pairs)
  p2 <- ggraph(g, layout="fr") +
    geom_edge_link(aes(width = n), alpha = .3) +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(title="Emotion co-occurrence network (top 100 pairs)")
  ggsave(file.path(fig_dir,"02_network.png"), p2, width=7, height=6, dpi=200)
}

# ---------------------------
# 3) Word clouds (top 5 emotions)
# ---------------------------
if (requireNamespace("wordcloud", quietly=TRUE)) {
  library(wordcloud)
  library(RColorBrewer)
  top_emotions <- head(freq$emotion, 5)
  tokens_with_emotion <- toks %>%
    left_join(tidy_labels, by="id")
  for (emo in top_emotions) {
    words <- tokens_with_emotion %>% filter(emotion == emo)
    word_freq <- words %>% count(lemma, sort=TRUE)
    if (nrow(word_freq) > 0) {
      png(file.path(fig_dir, paste0("03_wordcloud_", emo, ".png")), width=800, height=600)
      wordcloud(words = word_freq$lemma, freq = word_freq$n, max.words = 100,
                colors = brewer.pal(8, "Dark2"))
      dev.off()
    }
  }
}

# ---------------------------
# 4) Alluvial plot of co-occurring emotions
# ---------------------------
if (requireNamespace("ggalluvial", quietly=TRUE)) {
  library(ggalluvial)
  
  multi <- pairs %>%
    filter(n > 50) %>%
    distinct(item1, item2, .keep_all = TRUE) %>%
    mutate(item1 = factor(item1), item2 = factor(item2))
  
  if (nrow(multi) > 0) {
    p4 <- ggplot(multi, aes(axis1 = item1, axis2 = item2, y = n)) +
      geom_alluvium(aes(fill = item1), width = 1/12) +
      geom_stratum(width = 1/12, fill = "grey", color = "black") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), check_overlap = TRUE) +
      labs(title = "Alluvial flow of co-occurring emotions")
    ggsave(file.path(fig_dir,"04_alluvial.png"), p4, width=8, height=6, dpi=200)
  }
}

message("Unique visualizations saved.")

