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

#-- Validation croisée

validation.croisee <- function(id) {
  model <- prog$model[prog$id == id]
  k <- prog$k[prog$id == id]
  rep <- prog$rep[prog$id == id]
  fold <- prog$fold[prog$id == id]
  FILE <- paste("VEM_", k, "_", fold, ".rda", sep = "")
  training <- LDA(texts[folding != fold,], k = k)
  testing <-
    LDA(texts[folding == fold,], model = training, control = list(estimate.beta = FALSE))
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