# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

source(file = "functions/lda-functions.R")
source(file = "functions/lda-initialize.R")

files <- texts.list(articles.metadata)
nbr.textes <- nrow(files)

# Transformation des textes en lemmes

tag.corpus <- retrieve.or.cache(
  cache.file = tag.corpus.file, 
  f = function() lemmatize.textfiles(files)
)

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
  filter(str_sub(tag,1,3) %in% c("NAM","NOM","ADJ","ABR")) %>%
  # Suppression des termes de moins de 3 caractères
  filter(nchar(lemmes) >= 3) %>%
  rename(term = lemmes)

#-- NGrams

ngrams0 <- retrieve.or.cache(
  cache.file = ngrams0.file,
  f = function() mclapply(X = tag.corpus, FUN = function(x) {
    id <- x$id
    taggedText(x$tagger) %>%
      tbl_df() %>%
      mutate(DOCID = id) %>%
      ngram()
  }, mc.cores = nbCores) %>%
    ldply %>%
    tbl_df
) 

# Filtrage

lemmes.et.ngrams0 <- lemmes0 %>%
  bind_rows(ngrams0) %>%
  tfidf

lemmes.et.ngrams <- lemmes.et.ngrams0 %>%
  filter(ndoc >= 5, ndoc < 0.95*nbr.textes)

# length(lemmes.et.ngrams$term %>% unique)
# m <- median(lemmes.et.ngrams$tfidf)
# quantile(round(lemmes$tfidf,1), probs = seq(0,1,0.01))
# lemmes.et.ngrams %>% ungroup %>% filter(tfidf > m) %>% select(term) %>% unique %>% sample_n(10) 
# lemmes.et.ngrams %>% filter(tfidf > m) %>% arrange(-nchar(term))
# lemmes %>% arrange(ndoc)
# lemmes  %>% arrange(-ndoc)
# lemmes$ndoc %>% table
# lemmes %>% filter(ndoc == 4)

quantile(round(ngrams$tfidf), probs = seq(0, 1, 0.01))

ngrams %>% ungroup %>% arrange(-tfidf) %>% select(term) %>% unique %>% as.data.frame() %>% slice(1:1000)

tfidf.median <- median(lemmes0$tfidf)


