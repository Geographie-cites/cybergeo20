
# data prep for shiny app

# source semexport.R

datadir=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Cybergeo20/data/')

# PB : cybnames has 927 rows ; here 885
# with kws : 584

citation_cybergeodata<-as.tbl(cybergeo[nchar(cybergeo$SCHID)>0,])

# TODO : correct authors (first ;) - idem keywords ? -- not needed

save(citation_cybergeodata,file=paste0(datadir,'citation_cybergeodata.RData'))


# export citation nw

library(RSQLite)
db = dbConnect(SQLite(),paste0(datadir,"CitationNetwork.sqlite3"))

# write attributes in edgelist to avoid double lookup when loading data
#  (optimizing time performance at the price of disk memory size)
from=head_of(gcitation,E(gcitation));to=tail_of(gcitation,E(gcitation))
edges = data.frame(
  from=from$name,to=from$name,
  fromtitle=from$title,totitle=to$title,
  fromyear=from$year,toyear=to$year,
  fromcyb=from$cyb,tocyb=to$cyb
)

dbWriteTable(db,"edges",edges)
