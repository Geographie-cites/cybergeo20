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
  rename(term = lemmes) %>%
  select(docid, term)

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

lemmes.et.ngrams0 <- retrieve.or.cache(
  cache.file = lemmes.et.ngrams0.file,
  f = function() lemmes0 %>%
    bind_rows(ngrams0) %>%
    tfidf
)

lemmes.et.ngrams0 %>% nrow
quantile(round(lemmes.et.ngrams0$tfidf,1), probs = seq(0,1,0.01))
m <- median(lemmes.et.ngrams0$tfidf)

lemmes.et.ngrams <- lemmes.et.ngrams0 %>%
  ungroup %>%
  filter(ndoc >= 5, ndoc < 0.95*nbr.textes) %>%
  filter(tfidf > m)

lemmes.et.ngrams %>% nrow 
lemmes.et.ngrams %>% select(term) %>% unique %>% nrow
lemmes.et.ngrams %>% select(term) %>% unique %>% sample_n(10) 
lemmes.et.ngrams %>% arrange(-nchar(term)) %>% select(term) %>% unique %>% as.data.frame %>% slice(1:100)

vocabulaire <- lemmes.et.ngrams$term %>% unique

doc0 <- lemmes.et.ngrams %>%
  select(docid, term, tfidf) %>%
  mutate(termidx = match(term, vocabulaire) - 1)

texts.ids <- lemmes.et.ngrams$docid %>% unique

doc <- lapply(texts.ids, function(id){
  f0 <- doc0 %>%
    filter(docid == id) %>%
    mutate(tfidf = round(tfidf)) %>%
    select(termidx, tfidf) %>%
    arrange(termidx)
  rbind(f0$termidx, f0$tfidf)
})

texts <- ldaformat2dtm(doc, vocabulaire)

folding <- sample(1:nbrFolds, nrow(texts), replace = TRUE)
prog <- expand.grid(
  rep = 1:nbrReplications,
  model = models,
  k = k.list, 
  fold = 1:nbrFolds
)
prog$id <- 1:nrow(prog)

simulation.results <- retrieve.or.cache(
  cache.file = simulation.results.file,
  f = function() rbind.fill(foreach(id = prog$id) %dopar% validation.croisee(id))
)
