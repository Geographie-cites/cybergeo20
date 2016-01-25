library(tm)
library(SnowballC)

txt <- system.file("texts", "txt", package = "tm")
ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language = "lat"))
docs <- c("This is a text.", "This another one.")
VCorpus(VectorSource(docs))

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain))
inspect(reuters[19:20])
meta(reuters[[19]])
idx <- meta(reuters, "id") == '708'
reuters <- reuters[idx]
lapply(reuters, as.character)
reuters <- tm_map(reuters, stripWhitespace) # Elimination des espaces superflus
reuters <- tm_map(reuters, content_transformer(tolower)) # Mise en minuscule
reuters <- tm_map(reuters, removeWords, stopwords("english"))
tm_map(reuters, stemDocument)

dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])
findFreqTerms(dtm, 5)
findAssocs(dtm, "opec", 0.8)
inspect(removeSparseTerms(dtm, 0.4))

one <- reuters[['708']]
meta(one)
one <- stripWhitespace(one)

#one <- tm_map(one, content_transformer(tolower))
one <- stemDocument(one)
as.character(one)
