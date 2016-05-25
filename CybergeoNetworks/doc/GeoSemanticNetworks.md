## User guide

### Description
Hierarchical clustering of countries based on the aggregated semantic profile of their articles in Cybergeo.

### License

GNU GPLv3

### Data

Semantic profiles come from the analysis of Cybergeo keywords, Cybergeo texts and citation-neighbours' keywords presented within this app.

------
### Parameters

#### Semantic Profile

The analysis can be performed on three types of semantic profiles: 1. Cybergeo keywords grouped into 10 communities, 2. Cybergeo terms extracted from the full-texts and groupes into 10 themes, 3. the communities formed by the keywords of the papers neighbouring Cybergeo articles in the citation network. The first set of themes indicates how articles are promoted and classified by their authors. The second set indicates the semantic areas of the words actually used to write the articles. The third set refers to the inclusion of the article in a social network of research and the semantic areas in which the paper is used, cited or which the paper uses and cites.

#### Set of countries

The analysis can be performed on two types of countries: 1. the countries to which the authors are affiliated, 2. the countries studied in the article. 

#### Number of clusters

The user can choose the level of disaggregation of the analysis by determining the number of clusters to be extracted from the hierarchical clustering.

------

### Hierarchical Clustering

After aggregating the semantic profile by country (using the mean frequency), the clustering method is performed on standardised data. It uses a euclidian distance and the Ward criteria (function 'hclust' and method 'ward.D2' in R).
