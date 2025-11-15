# 01_get_data.R — Download GoEmotions Dataset

source("R/00_setup.R")
raw_dir <- here::here("data","raw","goemotions")
fs::dir_create(raw_dir)

message("Attempting to download GoEmotions dataset (train/dev/test)...")
urls <- c(
  "https://raw.githubusercontent.com/google-research/google-research/master/goemotions/data/train.tsv",
  "https://raw.githubusercontent.com/google-research/google-research/master/goemotions/data/dev.tsv",
  "https://raw.githubusercontent.com/google-research/google-research/master/goemotions/data/test.tsv"
)
dests <- file.path(raw_dir, basename(urls))
for (i in seq_along(urls)) {
  dest <- dests[i]
  if (!file.exists(dest) || file.size(dest) == 0) {
    tryCatch({
      utils::download.file(urls[i], dest, mode="wb", quiet=TRUE)
      message("Downloaded: ", basename(dest))
    }, error=function(e) message("Skip (", basename(dest), "): ", e$message))
  } else {
    message("File Exist: ", basename(dest))
  }
}
message("If download fails, place train.tsv under: ", raw_dir)

