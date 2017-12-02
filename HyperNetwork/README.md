# Repository for "Exploration of an Interdisciplinary Scientific Landscape"

## Data

Raw and processed data available at (http://dx.doi.org/10.7910/DVN/VU2XKT)

## Source

Source code in `Models`

### Data Collection

In `Models/DataCollection`

`citationNetwork.jar` constructs the citation network from an initial corpus (ris, bib or csv). Requires to run `torpool.jar` in background

`abstractSetRetriever.jar` collects abstracts (requires a Mendeley API key)

### Text Mining

In `Models/Semantic`

Keywords extraction and relevance estimation as tasks in `main.py`

### Network construction and analysis

In `Models/Analysis`

Main script in `main.R`

Citation network analysis in `citationNWAnalysis.R`

Semantic network analysis in `semanalysis.R`
