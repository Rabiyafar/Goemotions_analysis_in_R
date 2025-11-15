# 20_preprocess.R — clean text & tokens
source("R/00_setup.R")
out_dir <- here::here("data","processed")
dat <- readr::read_rds(file.path(out_dir,"goemotions_raw.rds"))
tidy_labels <- readr::read_rds(file.path(out_dir,"goemotions_tidy_labels.rds"))

#check
dim(dat)
head(dat$text)
dim(tidy_labels)
table(tidy_labels$emotion)


clean_text <- function(x){
  x %>%
    stringr::str_replace_all("http[s]?://\\S+"," ") %>%
    stringr::str_replace_all("&amp;","&") %>%
    stringr::str_replace_all("[\\r\\n]+"," ") %>%
    stringr::str_squish() %>%
    tolower()
}

dat <- dat %>%
  dplyr::mutate(
    text_clean = clean_text(text),
    n_chars = nchar(text_clean),
    n_words = stringr::str_count(text_clean, "\\S+")
  )

#check if Removes URLs, HTML entities, and line breaks.
#Converts to lowercase and removes extra spaces.
#Calculates number of characters (n_chars) and number of words (n_words)

head(dat$text)
head(dat$text_clean)
summary(dat$n_words)
summary(dat$n_chars)


# tokenization + lemmatization
toks <- tibble::tibble(id = dat$id, text_clean = dat$text_clean) %>%
  tidytext::unnest_tokens(token, text_clean, token="words") %>%
  dplyr::anti_join(tidytext::stop_words, by=c("token"="word")) %>%
  dplyr::mutate(lemma = textstem::lemmatize_words(token))

#check
#Splits text into tokens (words).
#Removes stopwords (common words like “the”, “and”).
#Converts tokens to lemmas (base forms: running → run).

toks %>% filter(id==dat$id[1])
head(toks)

# 4 Filter out very short or empty texts
dat_filtered <- dat %>% filter(n_words > 2)

# 5 Quanteda corpus and DFM
corp_filt <- corpus(dat_filtered, text_field = "text_clean", docid_field = "id")
dfm_filt <- dfm(tokens(corp_filt, remove_punct = TRUE))

# 6 Lexical diversity
lexdiv_filt <- textstat_lexdiv(dfm_filt, measure = c("TTR","C"))

# Check summary
summary(lexdiv_filt$TTR)
summary(lexdiv_filt$C)

# 7 Handle problematic docs (NA or 0)
problem_docs <- lexdiv_filt$document[is.na(lexdiv_filt$C)]
problem_docs

# Option A: Remove from dataset
# dat_filtered <- dat_filtered %>% filter(!id %in% problem_docs)
# lexdiv_filt <- lexdiv_filt %>% filter(!document %in% problem_docs)

# Option B: Set minimal lexical diversity for problematic docs
lexdiv_filt$C[is.na(lexdiv_filt$C)] <- 0

# 8 Merge lexical diversity metrics back into dat_filtered
dat_filtered <- dat_filtered %>%
  left_join(lexdiv_filt %>% select(document, TTR, C), 
            by = c("id" = "document"))

# Add flag for documents that originally had NA C
dat_filtered <- dat_filtered %>%
  mutate(C_flag = ifelse(C == 0, TRUE, FALSE))

# Check merged data
head(dat_filtered)
summary(dat_filtered$C)
table(dat_filtered$C_flag)

# 9 Save processed data
write_rds(dat_filtered, file.path(out_dir,"goemotions_clean_filtered.rds"), compress="gz")
write_rds(toks, file.path(out_dir,"tokens.rds"), compress="gz")
write_rds(lexdiv_filt, file.path(out_dir,"lexdiv.rds"), compress="gz")

message("Preprocessing complete.")
