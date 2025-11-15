# 30_metrics.R — emotion frequency, co-occurrence, and structural metrics with checks

# Load setup
source("R/00_setup.R")
out_dir <- here::here("data","processed")

# Load data
tidy_labels <- readr::read_rds(file.path(out_dir,"goemotions_tidy_labels.rds"))
dat <- readr::read_rds(file.path(out_dir,"goemotions_clean.rds"))
lexdiv <- readr::read_rds(file.path(out_dir,"lexdiv.rds"))

# --- 1. Frequency per emotion ---
freq <- tidy_labels %>%
  dplyr::count(emotion, sort=TRUE) %>%
  dplyr::mutate(p = n/sum(n))

# Check: basic summary
print("Top 10 emotions by count:")
print(head(freq, 10))
print("Sum of proportions (should be 1):")
print(sum(freq$p))

# Barplot for quick visual check
library(ggplot2)
ggplot(freq, aes(x = reorder(emotion, p), y = p)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(x="Emotion", y="Proportion", title="Emotion distribution in dataset")

# --- 2. Co-occurrence matrix ---
pairs <- tidy_labels %>%
  widyr::pairwise_count(item = emotion, feature = id, sort = TRUE, upper = FALSE)

# Quick check: top 10 co-occurring emotion pairs
print("Top 10 emotion co-occurrences:")
print(head(pairs, 10))

#  visualize as heatmap
library(igraph)
library(ggraph)
top_pairs <- head(pairs, 50)  # top 50 pairs
g <- graph_from_data_frame(top_pairs, directed=FALSE)
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width=n), alpha=0.5) +
  geom_node_point(size=5) +
  geom_node_text(aes(label=name), repel=TRUE) +
  labs(title="Top Emotion Co-occurrences")

# --- 3. Comment length by emotion ---
bylen <- tidy_labels %>%
  dplyr::left_join(dat[,c("id","n_chars","n_words")], by="id") %>%
  dplyr::group_by(emotion) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_words = mean(n_words, na.rm=TRUE),
    median_words = median(n_words, na.rm=TRUE),
    mean_chars = mean(n_chars, na.rm=TRUE),
    median_chars = median(n_chars, na.rm=TRUE)
  ) %>%
  dplyr::arrange(desc(n))

# Check: top 5 emotions by comment length
print("Comment length summary by emotion:")
print(head(bylen, 5))

# Optional: boxplot of words per emotion
ggplot(tidy_labels %>% left_join(dat[,c("id","n_words")], by="id"),
       aes(x=reorder(emotion, n_words, median), y=n_words)) +
  geom_boxplot() +
  coord_flip() +
  labs(x="Emotion", y="Number of Words", title="Comment length distribution by emotion")

# --- 4. Dominant emotion per doc ---
dom <- tidy_labels %>%
  dplyr::count(id, emotion, sort=TRUE) %>%
  dplyr::slice_max(n, by=id, n=1, with_ties=FALSE)

# --- 5. Lexical diversity by dominant emotion ---
lv <- lexdiv %>% dplyr::rename(id = document)

# Check: ensure no NAs in TTR/C
lv$TTR[is.na(lv$TTR)] <- 0
lv$C[is.na(lv$C)] <- 0

lv_by_emo <- lv %>%
  dplyr::left_join(dom, by="id") %>%
  dplyr::group_by(emotion) %>%
  dplyr::summarise(
    TTR = mean(TTR, na.rm=TRUE),
    C = mean(C, na.rm=TRUE),
    n = dplyr::n()
  ) %>%
  dplyr::arrange(desc(n))

# Check: print summary
print("Lexical diversity by dominant emotion:")
print(lv_by_emo)

# Optional: visualize lexical diversity
ggplot(lv_by_emo, aes(x=reorder(emotion, TTR), y=TTR)) +
  geom_col(fill="coral") +
  coord_flip() +
  labs(x="Emotion", y="Average TTR", title="Lexical Diversity (TTR) by Dominant Emotion")

# --- 6. Save metrics ---
readr::write_rds(freq, file.path(out_dir,"emotion_freq.rds"), compress="gz")
readr::write_rds(pairs, file.path(out_dir,"emotion_pairs.rds"), compress="gz")
readr::write_rds(bylen, file.path(out_dir,"length_by_emotion.rds"), compress="gz")
readr::write_rds(lv_by_emo, file.path(out_dir,"lexdiv_by_emotion.rds"), compress="gz")

message("Metrics computed and checks completed.")

