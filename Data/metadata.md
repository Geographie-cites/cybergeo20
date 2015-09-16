# Description des fichiers de données


## dossier `raw`

Contains a (more user-friendly) version of the raw sql cybergeo base (dump from Lodel platform).
 - cybergeo.csv : basic data on all publications in cybergeo : 
	* id : unique id
	* title_en : english title (may be vo if translated = 0 )
	* keywords_en : english keywords (parse by ",")
	* authors : authors (idem)
	* date
	* langue
	* translated : boolean, paper in english or title/kw are translated (some old references were not)
 - texts : contains abstracts and raw texts (un-html-ized), as txt files, filename : ID_[text,abstract].txt


## dossier `bib`

Various bib/ris files extracted from the base - use preferently raw data that has not been preprocessed.

 

## dossier `statsvisu`

- fichier RData
- contient des informations d'identification des articles (titre, auteur, date, type de publication) et le nombre de visualisations associées
- source : revues.org
- le champ "Annee" donne l'année correspondant au cumul de visualisations
- le champ "UNIQID" est un identifiant unique de publication qui permet de suivre les évolutions dans le temps
