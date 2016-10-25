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
  filter(str_sub(tag,1,3) %in% c("NAM","NOM","ADJ","ABR","VER")) %>%
  # Suppression des termes de moins de 3 caractères
  filter(nchar(lemmes) >= 3) %>%
  rename(term = lemmes) %>%
  select(docid, term)

lemmes.et.ngrams0 <- lemmes0 %>% tfidf()

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

lemmes.et.ngrams <- lemmes.et.ngrams0 %>% ungroup %>%
  filter(ndoc >= 5, ndoc < 0.95*nbr.textes) %>%
  select(docid, term, tfidf)

#################
# Import
lemmes.et.ngrams <- read.table(file = "ngrams.csv", header = T, sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE) %>% 
  tbl_df() %>%
  rename(term = ngram) %>% 
  arrange(docid, term)%>%
  select(docid, term, tfidf)
##################

lemmes.et.ngrams <- lemmes.et.ngrams %>%
  group_by(docid) %>%
  summarise(m = median(tfidf)) %>%
  right_join(lemmes.et.ngrams, by = c("docid" = "docid")) %>%
  filter(tfidf > m) %>%
  select(-m)

lemmes.et.ngrams %>% nrow 
lemmes.et.ngrams %>% select(term) %>% unique %>% nrow
lemmes.et.ngrams %>% select(term) %>% unique %>% sample_n(10) 
lemmes.et.ngrams %>% arrange(-nchar(term)) %>% select(term) %>% unique %>% as.data.frame %>% slice(1:100)

################

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

# model <- LDA(texts, k = 20)

simulation.results <- retrieve.or.cache(
  cache.file = simulation.results.file,
  f = function() rbind.fill(foreach(id = prog$id) %dopar% validation.croisee(id))
)

# Perplexity
ggplot(simulation.results, aes(k, perplexity)) +
  geom_point() +
  labs(
    x = "Nombre de thématiques",
    y = "Perplexité",
    title = "Evolution de la perplexité\nselon le nombre de thématiques\npendant la validation croisée"
  )

# Alpha
# The lower α the higher is the percentage of documents which are assigned to one single topic with a high probability
# Cela implique que les documents consistent un nombre limité de thèmes
ggplot(simulation.results, aes(k, alpha)) +
  geom_point() + #+ xlim(0,100)
  facet_wrap(~ rep)

# Entropie
# Higher values indicate that the topic distributions are more evenly spread over the topics.
g.ent <- ggplot(simulation.results, aes(k, entropie)) +
  geom_point() +
  labs(
    x = "Nombre de thématiques",
    y = "Entropie"
  )
g.ent + labs(title = "Evolution de l'entropie\nselon le nombre de thématiques\npendant la validation croisée")

k0<-20
#-- Résultats finaux
a <- simulation.results %>%
  filter(k == k0) %>%
  group_by(model) %>%
  summarise(alpha = mean(alpha))
alpha <- a$alpha
model <- LDA(texts, k = k0, control = list(alpha = alpha))
saveRDS(model, file=paste("~/Sync/Shared/model-", k0, ".rds", sep = ""))

