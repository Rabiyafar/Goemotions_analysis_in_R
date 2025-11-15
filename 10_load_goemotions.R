
source("R/00_setup.R")
raw_dir <- here::here("data","raw","goemotions")
out_dir <- here::here("data","processed")
fs::dir_create(out_dir)

read_any <- function(path){
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("tsv","txt")) readr::read_tsv(path, col_names = FALSE, show_col_types = FALSE)
  else if (ext == "csv") readr::read_csv(path, show_col_types = FALSE)
  else stop("Unsupported file: ", path)
}

files <- fs::dir_ls(raw_dir, regexp="(train|dev|validation|test)\\.(tsv|txt|csv)$", recurse=TRUE)
if (length(files)==0) {
  csv <- fs::dir_ls(raw_dir, regexp="goemotions.*\\.csv$", recurse=TRUE)
  if (length(csv)) files <- csv else stop("No GoEmotions files found in data/raw/goemotions")
}

parse_tsv_like <- function(df){
  stopifnot(ncol(df) >= 2)
  colnames(df)[1:3] <- c("text","labels","id")
  df <- df %>% mutate(text = as.character(text),
                      labels = as.character(labels),
                      id = as.character(id))
  df
}

load_split <- function(path){
  df <- read_any(path)
  nm <- tolower(basename(path))
  if (tolower(tools::file_ext(nm)) %in% c("tsv","txt")) df <- parse_tsv_like(df)
  else {
    df <- janitor::clean_names(df)
    if (!"text" %in% names(df)) names(df)[1] <- "text"
    if (!"labels" %in% names(df)) {
      emo_cols <- setdiff(names(df), c("id","text","split"))
      df_long <- df %>%
        tidyr::pivot_longer(all_of(emo_cols), names_to="emotion", values_to="val") %>%
        dplyr::filter(val==1) %>%
        dplyr::group_by(id, text) %>%
        dplyr::summarise(labels = paste0(unique(emotion), collapse=","), .groups="drop")
      df <- df_long
    }
  }
  df$split <- if (grepl("train", nm)) "train" else if (grepl("dev|validation", nm)) "dev" else if (grepl("test", nm)) "test" else "unknown"
  df
}

dflist <- lapply(files, load_split)
dat <- dplyr::bind_rows(dflist)

# explode labels -> tidy long
tidy_labels <- dat %>%
  dplyr::mutate(label_vec = strsplit(labels, ",")) %>%
  tidyr::unnest_longer(label_vec, values_to="emotion_id") %>%
  dplyr::mutate(emotion_id = trimws(emotion_id)) %>%
  dplyr::filter(!is.na(emotion_id), emotion_id!="")

# Map numeric ids to names (0..27)
emo_map <- tibble::tibble(
  emotion_id = as.character(0:27),
  emotion = c("admiration","amusement","anger","annoyance","approval","caring",
              "confusion","curiosity","desire","disappointment","disapproval",
              "disgust","embarrassment","excitement","fear","gratitude","grief",
              "joy","love","nervousness","optimism","pride","realization",
              "relief","remorse","sadness","surprise","neutral")
)

#check if true for emotion mapping
official_order <- c("admiration","amusement","anger","annoyance","approval","caring",
                    "confusion","curiosity","desire","disappointment","disapproval",
                    "disgust","embarrassment","excitement","fear","gratitude","grief",
                    "joy","love","nervousness","optimism","pride","realization",
                    "relief","remorse","sadness","surprise","neutral")

all(emo_map$emotion == official_order)  # Should return TRUE

tidy_labels <- tidy_labels %>%
  dplyr::left_join(emo_map, by="emotion_id") %>%
  dplyr::mutate(emotion = dplyr::coalesce(emotion, emotion_id))

# Save artifacts
readr::write_rds(dat, file.path(out_dir, "goemotions_raw.rds"), compress="gz")
readr::write_rds(tidy_labels, file.path(out_dir, "goemotions_tidy_labels.rds"), compress="gz")
message("Wrote processed datasets to data/processed/")

#self check
library(dplyr)
tidy_labels <- readr::read_rds("data/processed/goemotions_tidy_labels.rds")

# Counts per emotion
freq <- tidy_labels %>%
  count(emotion, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

print(freq)

ggplot(freq, aes(x = reorder(emotion, percent), y = percent)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(x = "Emotion", y = "Percentage", title = "Emotion distribution in dataset")


