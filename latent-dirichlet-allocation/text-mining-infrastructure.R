crude <- VCorpus(
  DirSource(system.file("texts", "crude", package = "tm")),
  readerControl = list(reader = readReut21578XMLasPlain)
)
acq <- VCorpus(
  DirSource(system.file("texts", "acq", package = "tm")),
  readerControl = list(reader = readReut21578XMLasPlain)
)

acq10 <- acq[[10]]
as.character(acq10)
acq10st <- stemDocument(acq10)
as.character(acq10st)

acq <- tm_map(acq, stripWhitespace) # Elimination des espaces superflus
acq <- tm_map(acq, content_transformer(tolower)) # Mise en minuscule
acq <- tm_map(acq, removeWords, stopwords("english"))
acq <- tm_map(acq, stemDocument)

as.character(acq[[1]])

library(wordnet)
initDict()
