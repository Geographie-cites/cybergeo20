# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

#-- Initialisation

# Localisation des fichiers
data.path <- "../Data/raw"
articles.filename <- "cybergeo.csv"
texts.folder <- "texts"

# Filtrage des lemmes
nbr.max.ngram <- 3

# Validation croisée
models <- c("LDA")
nbrReplications <- 1
nbrFolds <- 10
k.list <- c(2, 5, 10, 20, seq(25, 36, 2), 50)

# Modèle final
k0 <- 20                   # Sélection du nombre de thématiques après validation croisée
threshold <- 0.00005       # Probabilité minimale d'affichage du lemme
ts.lag.max <- 8            # Pour le graphique d'entropie
nbr.dimension <- 3         # Évolution déduite sur une fenêtre de 3 temps
ts.lag <- 8                # Décalage temporel permis

# Cache
cache.path <- "~/Sync/cache"
tag.corpus.filename <- "tag-corpus"
ngrams0.filename <- "ngrams0"

# Finalisation de l'initialisation
nbCores <- ceiling(detectCores()/2)
registerDoParallel(cores=nbCores)
articles.metadata <- paste(data.path, articles.filename, sep = "/")
textPath <- paste(data.path, texts.folder, sep = "/")
tag.corpus.file <- paste(cache.path, "/", tag.corpus.filename, ".rds", sep = "")
ngrams0.file <- paste(cache.path, "/", ngrams0.filename, ".rds", sep = "")
