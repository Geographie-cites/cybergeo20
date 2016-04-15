# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

library(plyr)
library(stringr)
library(lubridate)
library(dplyr)
library(koRpus)
library(stringi)
library(tidyr)
library(doParallel)
library(topicmodels)
library(slam)
library(tm)
library(ggplot2)
library(pdc)
library(wordcloud)

#-- Gestion du cache

retrieve.or.cache <- function(cache.file, f, force = FALSE) {
  if (file.exists(cache.file) && force == FALSE) {
    message("Loading cache…")
    var <- readRDS(cache.file)
    return(var)
  } else {
    message("Computing…")
    var <- f()
    saveRDS(object = var, file = cache.file)
    return(var)
  }
}

#-- Chargement des données

get.articles.metadata <-
  function(articles.metadata) {
    read.table(
      articles.metadata,
      sep = ",",
      header = TRUE
    ) %>%
      tbl_df() %>%
      rename(
        titre = title,
        auteurs = authors
      ) %>%
      filter() %>%
      select(id, date, langue, TypeDocument, Disponibilite, auteurs, titre) %>%
      mutate(
        langue = tolower(langue),
        date = as.Date(date)
      ) %>%
      arrange(date, auteurs)
  }

texts.list <- function(articles.metadata) 
  get.articles.metadata(articles.metadata) %>%
  filter(langue == "fr", TypeDocument == "Article", Disponibilite == "Texte intégral") %>%
  select(id) %>%
  mutate(
    name = paste(id, "_text.txt", sep = ""),
    path = paste(textPath, name, sep = "/"),
    size = file.size(path)
  ) %>%
  filter(size > 400)


#-- Part-of-Speech Tagging + Lemmatisation

lemmatize.textfiles <- function(files)
  foreach(id = files$id) %dopar% {
    file <- files$path[files$id == id]
    tagged.text <- lemmatize(file)
    return(list(id = id, tagger = tagged.text))
  }

lemmatize <-
  function(file.path, lang = "fr", encoding = "UTF-8", preset = "fr-utf8")
    treetag(
      file = file.path,
      treetagger = "manual",
      lang = lang,
      encoding = encoding,
      TT.tknz = FALSE,
      TT.options = list(path = "~/treetagger", preset = preset),
      stopwords = tm::stopwords(lang),
      stemmer = SnowballC::wordStem
    )

#-- Récupération des lemmes



#-- NGrams

ngram <- function(tab, max.ngram = 1000) {
  alls <- NULL
  id <- NULL
  deep <- 0
  for (i in 1:nrow(tab)) {
    if (is.null(id) || tab$DOCID[i] != id) {
      deep <- i
      id <- tab$DOCID[i]
    }
    if (tab$wclass[i] %in% c("fullstop", "punctuation")) {
      deep <- i + 1
    } else {
      new <- i
      deep <- max(1, deep, i - max.ngram + 1)
      for (j in deep:i) {
        if (
          nchar(tab$token[j]) >= 3 &&
          nchar(tab$token[i]) >= 3 &&
          str_sub(tab$tag[j], 1, 3) %in% c("NAM","NOM","ADJ","ABR") &&
          str_sub(tab$tag[i], 1, 3) %in% c("NAM","NOM","ADJ","ABR") &&
          j != i
        ) {
          texte <- paste(tab$token[j:i], collapse = " ")
          if (is.null(alls)) {
            alls <- data_frame(docid = id, term = texte)
          } else {
            alls <- rbind(alls, data_frame(docid = id, term = texte))
          }
        }
      }
    }
  }
  return(alls)
}

tfidf <- function(df0) {
  N <- df0 %>% select(docid) %>% distinct %>% nrow
  df1 <- df0 %>%
    select(docid, term) %>%
    group_by(docid, term) %>%
    summarise(tf = n())
  df2 <- df1 %>%
    group_by(term) %>%
    summarise(ndoc = n_distinct(docid)) %>%
    mutate(idf = log2(N / ndoc))
  df3 <- df1 %>%
    left_join(df2, by = c("term" = "term")) %>%
    mutate(tfidf = tf * idf)
  return(df3)
}

#-- Validation croisée

validation.croisee <- function(id) {
  model <- prog$model[prog$id == id]
  k <- prog$k[prog$id == id]
  rep <- prog$rep[prog$id == id]
  fold <- prog$fold[prog$id == id]
  training <- LDA(texts[folding != fold,], k = k)
  saveRDS(training, file = paste("~/Sync/Shared/training-model-", id, ".rds", sep = ""))
  testing <- LDA(texts[folding == fold,], model = training, control = list(estimate.beta = FALSE))
  d <- data.frame(
    id = id,
    rep = rep,
    model = model,
    k = k,
    fold = fold,
    alpha = training@alpha,
    perplexity = perplexity(testing, texts[folding == fold,], use_theta = FALSE),
    entropie = mean(apply(posterior(training)$topics, 1, function(z)
      - sum(z * log(z))))
  )
  return(d)
}

#-- Statistiques

distribution <- function(terms, xlab, ylab, title, legend.title) {
  t <- table(terms)
  terms.frequency <- data_frame(
    term = factor(names(t)),
    tf = as.numeric(t)
  ) %>%
    arrange(tf) %>%
    mutate(
      m = max(tf),
      v = ifelse(tf < m / 2,-0.5, 1.5)
    )
  ggplot(
    terms.frequency, 
    aes(x = term, y = tf, fill = term, label = tf, vjust = v)
  ) +
    geom_bar(stat = "identity") +
    labs(
      y = ylab,
      x = xlab,
      title = title,
      fill = legend.title
    ) +
    geom_text() +
    theme(axis.text.x = element_blank())
}

#-- Graphiques

series.temporelles.publications <- function() {
  ts.articles <- get.articles.metadata(articles.metadata) %>%
    select(date) %>%
    mutate(date = as.Date(cut(date, breaks = "1 month"))) %>%
    group_by(date) %>%
    summarise(counts = n())
  ggplot(
    ts.articles, 
    aes(x = date, y = counts)
  ) + 
    geom_line() + 
    scale_x_date()+
    labs(
      y = "Nombre de documents", 
      x = "Années", 
      title = "Évolution mensuelle des publications\nde la revue Cybergeo"
    )
}

distribution.des.langues <- function(articles) {
  t <- table(articles$langue)
  langues <- data_frame(lang = factor(names(t)),
                        count = as.numeric(t)) %>%
    arrange(count) %>%
    mutate(m = max(count),
           v = ifelse(count < m / 2,-0.5, 1.5))
  g <-
    ggplot(langues, aes(
      x = lang, y = count, fill = lang, label = count, vjust = v
    )) +
    geom_bar(stat = "identity") +
    #coord_flip() +
    labs(
      y = "Nombre de documents",
      x = "Langues",
      title = "Distribution de la langue d'écriture\ndes publications de la revue Cybergeo",
      fill = "Langues"
    ) +
    geom_text()
  return(g)
}