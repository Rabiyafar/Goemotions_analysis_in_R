pkgs <- c(
  "tidyverse","readr","stringr","janitor","here","fs",
  "tidytext","textstem","quanteda","quanteda.textstats",
  "widyr","igraph","ggraph","reshape2","rlang","scales",
  "RColorBrewer","knitr","wordcloud","ggalluvial","text2vec"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos='https://cloud.r-project.org')
}
invisible(lapply(pkgs, library, character.only = TRUE))
options(stringsAsFactors = FALSE)

if (!fs::dir_exists("data")) fs::dir_create("data")
if (!fs::dir_exists("data/raw")) fs::dir_create("data/raw")
if (!fs::dir_exists("data/processed")) fs::dir_create("data/processed")
if (!fs::dir_exists("figures")) fs::dir_create("figures")


