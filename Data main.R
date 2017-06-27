rm(list=ls())
library(quantmod)
library(dplyr)

ticker <- c("XLY", "XLP", "XLE", "XLF", "XLV","XLRE",   
            "XLI", "XLB", "XLK", "XLU", "SPY") 
sectors = c('disc','stpl','engy','fin','care',"estat",
            'indst','matrl','tech','utlty','mkt')
sdate='2016-01-01'
Data = getSymbols(ticker,from=sdate)
Prices = do.call(merge,lapply(ticker, function(x) Ad(get(x))))
names(Prices)=ticker
ret = Prices%>%log%>%diff%>%na.omit

library(bnlearn)
retdf=as.data.frame(ret)
names(retdf)=sectors
retdf%>%gs%>%plot

library(igraph)
ig = retdf%>%gs%>%as.graphNEL%>%igraph.from.graphNEL
plot(ig)

library(visNetwork)
vg = toVisNetworkData(ig)
nodes = vg$nodes
nodes$size = 25
nodes$shape='circle'
edges = vg$edges
visNetwork(nodes,edges,physics=T,idToLabel=F)%>%visEdges(arrows='to')

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")
# library(pcalg)
# suffStat <- list(C = cor(retdf), n = nrow(retdf))
# pc.gmG <-  pc(suffStat, indepTest = gaussCItest, alpha = 0.01,labels = sectors)
# plot(pc.gmG, main = "")
# iplotPC(pc.gmG)


# library(igraph)
# library(visNetwork)
# cormat=cor(ret)
# diag(cormat) = 0
# graph = graph.adjacency(cormat, weighted=TRUE,mode='lower')
# graph = delete.edges(graph, E(graph)[ weight < 0.65])
# # plot(graph,edge.label = round(E(graph)$weight,2),vertex.size=setNames(25,'a'))
# # Interactive Plot
# vg = toVisNetworkData(graph)
# nodes = vg$nodes
# nodes$size = 25
# nodes$shape='circle'
# edges = vg$edges
# edges$label = round(edges$weight,2)
# visNetwork(nodes,edges,physics=T,idToLabel=F)

# library(mgm)
# mgmmod = mgm(ret,type=rep('g',ncol(ret)),lev=rep(1,ncol(ret)))
# ##  Note that signs of edge weights (if defined) are stored in fitobject$signs. See ?mgm for more info.
# ig=graph.adjacency(mgmmod$pairwise$wadj,mode='lower',weighted=T)
# ig=delete.edges(ig, E(ig)[ weight <= 0.11])
# plot(ig)
# vg=toVisNetworkData(ig)
# nodes=vg$nodes
# nodes$label = names(ret)
# nodes$shape='circle'
# edges=vg$edges
# edges$label = round(edges$weight,2)
# visNetwork(nodes,edges)
