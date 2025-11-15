# --- Sanity Checks for NRC Lexicon Analysis ---

# 1. Check dimensions
message("Dimensions check:")
print(dim(emo_scores))       # rows = documents, cols = emotions + id
print(dim(compare_tbl))      # should match number of docs, plus dominant emotion info

# 2. Quick glimpse of first few rows
message("First 5 rows of emotion scores:")
print(head(emo_scores, 5))

message("First 5 rows of combined NRC vs dominant emotion:")
print(head(compare_tbl, 5))

# 3. Check sum of NRC rates per document
# Should be roughly <= 100% (since each word may map to multiple sentiments)
sum_check <- emo_scores %>%
  dplyr::mutate(total_rate = rowSums(across(starts_with("rate_")))) %>%
  dplyr::summarise(min_rate = min(total_rate), max_rate = max(total_rate), mean_rate = mean(total_rate))
print(sum_check)

# 4. Check for documents with zero matches
zero_match_docs <- emo_scores %>% 
  dplyr::filter(rowSums(across(starts_with("rate_"))) == 0)
message("Number of documents with zero NRC matches:")
print(nrow(zero_match_docs))

# 5. Quick summary of each emotion rate
emo_rate_summary <- emo_scores %>%
  dplyr::select(starts_with("rate_")) %>%
  summary()
message("Summary of NRC emotion rates:")
print(emo_rate_summary)

# 6. Optional: visualize distribution of top NRC emotions
library(ggplot2)
emo_scores_long <- emo_scores %>%
  tidyr::pivot_longer(cols = starts_with("rate_"), names_to = "emotion", values_to = "rate")

top_emo <- emo_scores_long %>%
  dplyr::group_by(emotion) %>%
  dplyr::summarise(mean_rate = mean(rate, na.rm = TRUE)) %>%
  dplyr::arrange(desc(mean_rate)) %>%
  head(10)

ggplot(top_emo, aes(x=reorder(emotion, mean_rate), y=mean_rate)) +
  geom_col(fill="tomato") +
  coord_flip() +
  labs(x="Emotion", y="Mean Rate (%)", title="Top 10 NRC Emotions by Average Rate")

# 7. Check consistency with dominant emotion labels
# Compare NRC top emotion vs dominant label
compare_check <- compare_tbl %>%
  rowwise() %>%
  mutate(nrc_top = names(.)[which.max(c_across(starts_with("rate_")))]) %>%
  ungroup() %>%
  dplyr::mutate(match_dom = nrc_top == emotion) 

message("Fraction of documents where NRC top emotion matches dominant label:")
print(mean(compare_check$match_dom, na.rm=TRUE))

# 8. Optional: quick embedding sanity check (if created)
if (file.exists(file.path(out_dir,"sample_embeddings.rds"))) {
  emb_sample <- readr::read_rds(file.path(out_dir,"sample_embeddings.rds"))
  message("Sample embeddings dimensions:")
  print(dim(emb_sample))
  message("First 5 rows of embeddings sample:")
  print(head(emb_sample,5))
}

message("Sanity checks complete.")

