library(tm)
library(SnowballC)
library(plyr)
library(stringr)
library(lubridate)
library(dplyr)

#-- Chargement du corpus
corpus_path <- "../../Data/cybergeo-articles-1996-2015/texts/"
corpus <- VCorpus(
  DirSource(corpus_path)
)

#-- Méta-données du corpus
articles <- read.table(
  "cybergeo.csv", 
  sep = ";", 
  quote = "", 
  comment.char = "", 
  header = TRUE
) %>% 
  tbl_df() %>%
  rename(titre = title_en, auteurs = authors) %>%
  mutate(
    annee = str_sub(date, 1, 4),
    trimestre = quarter(parse_date_time(date, "ymd"), with_year = TRUE),
    langue = tolower(langue)
  ) %>%
  select(id, date, trimestre, annee, langue, auteurs, titre) %>%
  arrange(date, auteurs)

#-- 62 textes n'ont pas de langues attribuées
#-- Beaucoup de textes sont tronqués
table(articles$langue)

#-- Affectation des méta-données au corpus
apply_meta <- function(x) {
  num <- as.numeric(str_extract(meta(x, "id"), "^[^_]+"))
  article <- articles %>% filter(id == num)
  meta(x, "id") <- num
  meta(x, "origin") <- "Cybergeo"
  meta(x, "author") <- article$auteurs
  meta(x, "datetimestamp") <- article$date
  meta(x, "language") <- if(is.null(article$langue)) { "fr" } else { article$langue }
  meta(x, "heading") <- article$titre
  return(x)
}

fr <- corpus %>%
  tm_map(apply_meta) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_filter(function(x) { meta(x, "language") == "fr" }) %>%
  tm_map(removeWords, stopwords("fr")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)
