# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

source(file = "functions/lda-functions.R")
source(file = "functions/lda-initialize.R")

files <- texts.list(articles.metadata)
nbr.textes <- nrow(files)

# Transformation des textes en lemmes

tag.corpus <- NULL
if (file.exists(tag.corpus.file)) {
  message("Loading cache…")
  tag.corpus <<- readRDS(tag.corpus.file)
} else {
  message("Working on the whole corpus…")
  tag.corpus <<- lemmatize.textfiles(files)
  saveRDS(object = tag.corpus, file = tag.corpus.file)
}

treetagger <- lapply(tag.corpus, function(x) {
  id <- x$id
  taggedText(x$tagger) %>%
    tbl_df() %>%
    # Assignation de l'identifiant du document
    mutate(docid = id) %>%
    # Si aucun lemme n'est défini (cas des noms propres), alors prendre le mot initial
    mutate(lemmes = ifelse(lemma == "<unknown>", token, lemma))
}) %>%
  ldply() %>%
  tbl_df()

# Récupération des lemmes

lemmes0 <- treetagger %>%
  # Récupération des noms communs et propros, ainsi que des verbes
  filter(str_sub(tag,1,3) %in% c("NAM","NOM","VER","ADJ","ABR")) %>%
  # Suppression des nombres et des points
  mutate(lemmes = gsub("[0-9.]", "", lemmes, perl = TRUE)) %>%
  # Suppression des termes de moins de 3 caractères
  filter(nchar(lemmes) >= 3) %>%
  rename(term = lemmes) %>%
  tfidf()

#-- NGrams

ngrams0.file <- "cache/ngrams0.rds"
nbr.max.ngram <- 2

ngrams0 <- NULL
if (file.exists(ngrams0.file)) {
  message("Loading cache…")
  ngrams0 <<- readRDS(ngrams0.file)
} else {
  message("Working on the whole corpus…")
  ngrams0 <<- mclapply(X = tag.corpus, FUN = function(x) {
    id <- x$id
    taggedText(x$tagger) %>%
      tbl_df() %>%
      mutate(DOCID = id) %>%
      ngram(max.ngram = nbr.max.ngram)
  }, mc.cores = nbCores) %>%
    ldply() %>%
    tbl_df() %>%
    tfidf()
  saveRDS(object = ngrams0, file = ngrams0.file)
}

# Filtrage

ngrams <- ngrams0 %>%
  filter(ndoc >= 5, ndoc < 0.95*nbr.textes)

lemmes <- lemmes0 %>%
  filter(ndoc >= 5, ndoc < 0.95*nbr.textes)

lemmes.et.ngrams <- lemmes %>%
  bind_rows(ngrams)

# length(ngrams$term %>% unique)
# lemmes %>% arrange(ndoc)
# lemmes  %>% arrange(-ndoc)
# lemmes$ndoc %>% table
# lemmes %>% filter(ndoc == 4)
# quantile(round(lemmes$tfidf,1), probs = seq(0,1,0.01))
# ngrams %>% ungroup %>% select(term) %>% unique %>% sample_n(10) 

quantile(round(lemmes.et.ngrams$tfidf), probs = seq(0, 1, 0.01))

ngrams %>% ungroup %>% arrange(-tfidf) %>% select(term) %>% unique %>% as.data.frame() %>% slice(1:1000)

tfidf.median <- median(lemmes0$tfidf)


