# 60_embeddings.R — GloVe Embedding Analysis + Sanity Checks
source("R/00_setup.R")  # load libraries
out_dir <- here::here("data", "processed")
fs::dir_create(out_dir)

# ---- 1. Load GloVe ----
glove_file <- here::here("data", "glove", "glove.6B.50d.txt")
if (!file.exists(glove_file)) {
  stop("Download GloVe from https://nlp.stanford.edu/projects/glove/ and put glove.6B.50d.txt into data/glove/")
}

glove <- read.delim(glove_file, sep=" ", header=FALSE, quote="", stringsAsFactors=FALSE)
words <- glove[,1]
matrix <- as.matrix(glove[,-1])
rownames(matrix) <- words
message("Loaded GloVe embeddings: ", nrow(matrix), " words x ", ncol(matrix), " dimensions")

# ---- Sanity check: inspect first 5 rows ----
print("First 5 rows of GloVe matrix:")
print(matrix[1:5, 1:5])

# ---- 2. Cosine similarity function ----
cosine_sim <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

# ---- 3. Example similarities ----
joy_vec <- matrix["joy", ]
happiness_vec <- matrix["happiness", ]
sadness_vec <- matrix["sadness", ]

similarities <- tibble::tibble(
  pair = c("joy-happiness", "joy-sadness", "happiness-sadness"),
  cosine = c(
    cosine_sim(joy_vec, happiness_vec),
    cosine_sim(joy_vec, sadness_vec),
    cosine_sim(happiness_vec, sadness_vec)
  )
)
readr::write_rds(similarities, file.path(out_dir, "embedding_similarities.rds"))
print("Cosine similarities (sample pairs):")
print(similarities)

# ---- 4. PCA of emotion words ----
emo_words <- c("joy", "happiness", "sadness", "anger", "fear",
               "surprise", "love", "disgust", "excitement", "disappointment")

emo_matrix <- matrix[emo_words, ]
pca <- prcomp(emo_matrix, scale.=TRUE)
coords <- data.frame(pca$x[,1:2], emotion=emo_words)
readr::write_rds(coords, file.path(out_dir, "embedding_coords.rds"))

# Quick PCA plot
p <- ggplot(coords, aes(PC1, PC2, label=emotion)) +
  geom_point(color="steelblue", size=3) +
  geom_text(vjust=-0.5, size=4) +
  theme_light() +
  ggtitle("Emotion Words in GloVe Embedding Space")
ggplot2::ggsave(here::here("figures", "09_embeddings_pca.png"), p, width=7, height=5, dpi=200)

# ---- 5. Sanity check: cosine similarity heatmap of all 10 emotion words ----
emo_cosine <- matrix(0, nrow=length(emo_words), ncol=length(emo_words))
rownames(emo_cosine) <- colnames(emo_cosine) <- emo_words

for (i in 1:length(emo_words)) {
  for (j in 1:length(emo_words)) {
    emo_cosine[i,j] <- cosine_sim(matrix[emo_words[i], ], matrix[emo_words[j], ])
  }
}

# Convert to tidy format for plotting
emo_cosine_df <- as.data.frame(as.table(emo_cosine))
names(emo_cosine_df) <- c("Emotion1", "Emotion2", "Cosine")

p_heat <- ggplot(emo_cosine_df, aes(Emotion1, Emotion2, fill=Cosine)) +
  geom_tile() +
  scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0) +
  geom_text(aes(label=round(Cosine,2)), size=3) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Cosine Similarity Heatmap of 10 Emotion Words")
ggplot2::ggsave(here::here("figures", "10_embeddings_cosine_heatmap.png"), p_heat, width=7, height=6, dpi=200)

message("Embedding analysis + sanity checks complete.")
