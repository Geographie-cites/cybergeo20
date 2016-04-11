# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

source(file = "functions/lda-functions.R")
source(file = "functions/lda-initialize.R")

# Transformation des textes en lemmes

tag.corpus <- NULL
if (file.exists(tag.corpus.file)) {
  message("Loading cache…")
  tag.corpus <<- readRDS(tag.corpus.file)
} else {
  message("Working on the whole corpus…")
  files <- texts.list(articles.metadata)
  tag.corpus <<- lemmatize.textfiles(files)
  saveRDS(object = tag.corpus, file = tag.corpus.file)
}

# Récupération des lemmes

lemmes <- lapply(tag.corpus, function(x) {
  id <- x$id
  taggedText(x$tagger) %>%
    tbl_df() %>%
    mutate(DOCID = id)
}) %>%
  ldply() %>%
  tbl_df()

ng <- NULL
system.time({
  ng <<- lemmes %>% 
    slice(1:100000) %>%
    ngram(max.ngram = 2)# %>%
    #tfidf
})

saveRDS(object = ng, file = "cache/ng.rds")
