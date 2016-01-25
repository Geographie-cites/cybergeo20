library(topicmodels)
library(lattice)
library(slam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pdc)

dtm <- DocumentTermMatrix(fr, control = list(minWordLength = 3))
dim(dtm)
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
dtm <- dtm[, term_tfidf > 0.003] # This measure allows to omit terms which have low frequency as well as those occurring in many documents. We only include terms which have a tf-idf value of at least 0.1 which is a bit less than the median and ensures that the very frequent terms are omitted.
dtm <- dtm[row_sums(dtm) > 0,]
summary(col_sums(dtm))
dim(dtm)

k <- 20
SEED <- 2010
jss_TM <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
)

# Alpha : Plus il est petit, plus les documents sont assignés qu'à un seul thème
sapply(jss_TM[1:2], slot, "alpha")
# Entropie : Plus la valeur est haute, plus les topics sont distribuées uniformément
sapply(jss_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
Topic <- topics(jss_TM[["VEM"]], 1)
Topic
Terms <- terms(jss_TM[["VEM"]], 3)
Terms

methods <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
DF <- data.frame(posterior = unlist(lapply(jss_TM, function(x) apply(posterior(x)$topics, 1, max))),
                 method = factor(rep(methods,
                                     each = nrow(posterior(jss_TM$VEM)$topics)), methods))
print(histogram(~ posterior | method, data = DF, col = "white", as.table = TRUE,
                xlab = "Probability of assignment to the most likely topic",
                ylab = "Percent of total", layout = c(4, 1)))


freq <-  data_frame(topic_id = Topic, article_id = as.integer(names(Topic))) %>%
  left_join(articles, by = c("article_id" = "id")) %>%
  select(annee, topic_id) %>%
  arrange(annee, topic_id) %>%
  group_by(annee, topic_id) %>%
  summarise(counts = n())

ggplot(freq, aes(annee, counts)) + geom_bar(stat = "identity") + facet_grid(topic_id ~ ., scales = "free_y", space = "free_y")

m <- freq %>%
  mutate(counts = as.numeric(counts)) %>%
  spread(topic_id, counts) %>%
  select(-annee) %>%
  as.matrix()

#-- Choix des paramètres de la classification
mine <- entropyHeuristic(m, t.max = 8)
plot(mine)

clust <- pdclust(m)
clust
plot(clust)
plot(clust, labels = colnames(m))
