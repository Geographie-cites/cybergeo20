library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(wordcloud)

pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")
#pattern_list <- c("g[ée]ograph")

#setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/regexp'))

#-- Loading data --------------------------------------------------------------

terms <- read.table(
  "terms.csv", 
  sep = ";", 
  quote = "", 
  comment.char = "", 
  header = TRUE,
  stringsAsFactors = FALSE
) %>% 
  tbl_df() %>%
  mutate(
    article_id = id,
    id = row_number()
  ) %>%
  select(id, article_id, term, count)

sentences <- read.table(
  "sentences.csv", 
  sep = "|", 
  quote = "", 
  comment.char = "", 
  header = TRUE,
  stringsAsFactors=FALSE
) %>% 
  tbl_df() %>%
  mutate(
    article_id = id,
    id = row_number()
  )

articles <- read.table(
  "../Data/raw/cybergeo.csv", 
  sep = ",", 
  quote = "\"", 
  comment.char = "", 
  header = TRUE
) %>% 
  tbl_df() %>%
  rename(titre = title_en, auteurs = authors) %>%
  mutate(citation = paste(sep = ". ", auteurs, substr(date,1,4), titre)) %>%
  select(id, date, citation, langue)

gc()

#-- Functions -----------------------------------------------------------------

terms_matched <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, terms$term, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      mutate(pattern = pattern) %>%
      bind_rows(data)
  }
  data <- data %>%
    left_join(terms, by = c("id")) %>%
    arrange(id, pattern)
  return(data)
} 

titles_matched <- function(patterns) {
  citations <- terms_matched(patterns) %>%
    select(article_id) %>%
    unique() %>%
    left_join(articles, by = c("article_id" = "id")) %>%
    arrange(date) %>%
    select(citation)
  return(citations$citation)
}

phrases <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, sentences$sentence, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      bind_rows(data)
  }
  data <- data %>%
    left_join(sentences, by = c("id")) %>%
    select(sentence)
  return(data$sentence)
}

terms_matched_cloud <- function(patterns) {
  terms_matched(patterns) %>%
    group_by(term) %>%
#    summarise(articles = n_distinct(article_id), terms = sum(count))
    summarise(articles = sum(count))
}

articles_matched <- function(patterns) {
  terms_matched(patterns) %>%
    group_by(article_id, pattern) %>%
    summarise(count = sum(count)) %>%
    left_join(articles, by = c("article_id" = "id")) %>%
    mutate(ym = str_sub(date, 1, 4)) %>%
    group_by(ym, pattern) %>%
    summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
    ungroup() %>%
    mutate(date = parse_date_time(ym, "%y")) %>%
    select(date, pattern, articles, terms)
}

chronogram <- function(patterns) {
  ggplot(articles_matched(patterns), aes(date, articles)) +
    geom_bar(stat = "identity") +
    facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
    labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
}

cloud <- function(patterns) {
  words <- terms_matched_cloud(patterns)
  wordcloud(
    words$term,
    words$articles,
    scale = c(10,1),
    rot.per = 0
  ) 
}
