rm(list=ls())
library(quantmod)
library(Quandl)
library(bnlearn)
library(dplyr)
library(dygraphs)
library(igraph)
library(graph)
#bnlearn directed graph model
qdl= scan('QuandlAPI.txt',what="character", sep=NULL)
curns = c('GBP','JPY','EUR','MXN','INR','CNY','CAD','AUD','CHF')
fxdat = Quandl(code=paste('CUR/',curns,sep=''),start_date=as.Date('2016-01-01'),api_key=qdl,type='xts')
names(fxdat)=curns
ret = fxdat%>%log%>%diff%>%na.omit%>%as.data.frame
# gsmod = cextend(gs(ret))
gsmod = gs(ret)
plot(gsmod)
bn.fit(gsmod,ret)
ig = igraph.from.graphNEL(as.graphNEL(gsmod))
plot(ig)

#MGM/correlation network
#leaflet worldmap


rm(list=ls())
library(quantmod)
library(dplyr)
library(pcalg)
ticker <- c("XLY", "XLP", "XLE", "XLF", "XLV","XLRE",   
            "XLI", "XLB", "XLK", "XLU", "SPY") 
sectors = c('disc','stpl','engy','fin','care',"estat",
            'indst','matrl','tech','utlty','mkt')
sdate='2016-01-01'
Data = getSymbols(ticker,from=sdate)
Prices = do.call(merge,lapply(ticker, function(x) Ad(get(x))))
names(Prices)=ticker
ret = Prices%>%log%>%diff%>%na.omit
corGraph(ret%>%as.data.frame)%>%plot
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

library(mgm)
mgmfit = mgm(ret,type=rep('g',ncol(ret)),level=rep(1,ncol(ret)),verbatim=F,pbar=F)
mgmmat = mgmfit$pairwise$wadj
ig = graph.adjacency(mgmmat,mode='lower',weighted = T)
V(ig)$name=sectors
plot(ig)
cormat = cor(ret)
ig = graph.adjacency(cormat,diag = F,weighted=T,mode='lower')
ig=delete.edges(ig, E(ig)[ weight <= 0.6])
V(ig)$name=sectors
plot(ig,edge.label = round(E(ig)$weight,2))

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")
library(pcalg)
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


rm(list=ls())
# https://juliasilge.com/blog/using-tidycensus/
library(tidyverse)
library(tidycensus)
census_api_key(scan('c:/Users/Sida.Yang/Documents/R/CensusAPI.txt',what="character", sep=NULL))
State_pop <- get_acs(geography = "county", 
                     variables = "B01003_001", 
                     state = "CA",
                     geometry = TRUE) 
library(leaflet)
library(stringr)
library(sf)
# v15 <- load_variables(2015, "acs5", cache = TRUE)

pal <- colorQuantile(palette = "viridis", domain = State_pop$estimate, n = 10)

State_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Population percentiles",
            opacity = 1)


pal <- colorNumeric(palette = "plasma", 
                    domain = State_pop$estimate)
State_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "County Populations",
            opacity = 1)


HS_value <- get_acs(geography = "tract", 
                     variables = "B25077_001", 
                     state = "IL",
                     # county = "King County",
                     geometry = TRUE)

pal <- colorNumeric(palette = "viridis", 
                    domain = HS_value$estimate)

HS_value %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste('$',round(HS_value$estimate/1000),'K',sep=''),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median Home Value",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)





IC_value <- get_acs(geography = "tract", 
                    variables = "B19013_001", 
                    state = "WA",
                    county = "King County",
                    geometry = TRUE)

pal <- colorNumeric(palette = "viridis", 
                    domain = IC_value$estimate)

IC_value %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste('$',round(IC_value$estimate/1000),'K',sep=''),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Household Income",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


# rm(list=ls())
# library(Rssa)
# # Decomposition stage
# s <- ssa(co2, L = 120)
# plot(s)
# plot(s,type="vector")
# plot(s,type="paired")
# plot(s,type="wcor")
# # Reconstruction stage
# # Grouping from looking at the W Cor matrix
# # The results are the reconstructed series r$F1, r$F2, and r$F3
# recon <- reconstruct(s, groups = list(c(1,4), c(2, 3), c(5, 6)))
# # Calculate the residuals
# res <- residuals(recon)
# plot(recon, type = "cumsum")
# plot(wcor(s, groups = list(c(1,4), c(2,3), c(5, 6))))
# plot(recon)
# 
# library(quantmod)
# library(dplyr)
# mkt = getSymbols('MSFT',auto.assign = F)
# adp = Ad(mkt)
# v = Vo(mkt)%>%apply.weekly(sum)%>%ts(start=c(2007,1),frequency=52)
# plot(v)
# vdc = ssa(v[,1],L=120)
# plot(vdc)
# plot(vdc,type='vector')
# plot(vdc,type='paired')
# plot(vdc,type='wcor')
# recon <- reconstruct(vdc, groups = c(1,2,3))
# plot(recon,type='cumsum')
rm(list=ls())
# https://juliasilge.com/blog/using-tidycensus/
library(tidyverse)
library(tidycensus)
census_api_key(scan('c:/Users/Sida.Yang/Documents/R/CensusAPI.txt',what="character", sep=NULL))
library(leaflet)
library(stringr)
library(sf)
censusdat = get_acs(geography = "tract", 
        variables = 'B25077_001', 
        state = "WA",
        county = "King County",
        geometry = TRUE)%>%na.omit
censusdat = get_acs(geography = "tract", 
                    variables = 'B01002_001', 
                    state = "MO",
                    county = "Jackson County",
                    geometry = TRUE)%>%na.omit
pal <- colorNumeric(palette = "viridis", 
                    domain = censusdat$estimate)
censusdat %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste(censusdat$estimate),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Household Income",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


gmrent = 'B25031_001'
mocval = 'B25077_001'
ilb = 'C18120_002'
ilbe = 'C18120_003E'
ilbu = 'C18120_006E'
minc= 'B19013_001'
mage = 'B01002_001E'

inlab = get_acs(geography = "tract", 
                            variables = ilb, 
                            state = "WA",
                            county = "King County",
                            geometry = TRUE)%>%na.omit
inlabun = get_acs(geography = "tract", 
                  variables = ilbu, 
                  state = "WA",
                  county = "King County",
                  geometry = TRUE)%>%na.omit

unrate = inlab
unrate$estimate = inlabun$estimate/inlab$estimate
censusdat = unrate
