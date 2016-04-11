# Author: Pierre-Olivier Chasset (LISER, Géographie-cités)
# License: CC BY 3.0 FR http://creativecommons.org/licenses/by/3.0/fr/
# Contact: pierre-olivier.chasset@liser.lu

#-- Initialisation

# Localisation des fichiers
abstract.filename <- "input/27465.txt"
data.path <- "../Data/raw"
articles.filename <- "cybergeo.csv"
texts.folder <- "texts"

# Filtrage des lemmes
tag.to.keep <- c("NAM","NOM","VER","ADJ")
token_to_delete <- c("c", "d", "l", "n", "qu", "s", "Aujourd", "km2")

# Validation croisée
models <- c("LDA")
nbrReplications <- 1
nbrFolds <- 10
k.list <- c(2, 5, 10, 20, seq(25, 36, 2), 50, 100, 200)

# Modèle final
k0 <- 20                   # Sélection du nombre de thématiques après validation croisée
threshold <- 0.00005       # Probabilité minimale d'affichage du lemme
ts.lag.max <- 8            # Pour le graphique d'entropie
nbr.dimension <- 3         # Évolution déduite sur une fenêtre de 3 temps
ts.lag <- 8                # Décalage temporel permis

# Cache
cache.path <- "cache"
tag.corpus.filename <- "tag-corpus"

# Finalisation de l'initialisation
nbCores <- ceiling(detectCores()/2)
registerDoParallel(cores=nbCores)
articles.metadata <- paste(data.path, articles.filename, sep = "/")
textPath <- paste(data.path, texts.folder, sep = "/")
tag.corpus.file <- paste(cache.path, "/", tag.corpus.filename, ".rds", sep = "")
