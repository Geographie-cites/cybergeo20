library(latticeExtra)
data(mtcars)
x  <- t(as.matrix(scale(mtcars)))
dd.row <- as.dendrogram(hclust(dist(x)))
row.ord <- order.dendrogram(dd.row)

dd.col <- as.dendrogram(hclust(dist(t(x))))
col.ord <- order.dendrogram(dd.col)

library(lattice)

levelplot(x[row.ord, col.ord],
          aspect = "fill",
          scales = list(x = list(rot = 90)),
          colorkey = list(space = "left"),
          legend =
            list(right =
                   list(fun = dendrogramGrob,
                        args =
                          list(x = dd.col, ord = col.ord,
                               side = "right",
                               size = 10)),
                 top =
                   list(fun = dendrogramGrob,
                        args =
                          list(x = dd.row,
                               side = "top",
                               size = 10))))
