library(dplyr)
library(bnlearn)
library(igraph)
library(stagedtrees)
library(qgraph)

source("search_order.R")

pdag <- function(best,indexes,...){
  el <- matrix(ncol = 2)
  for (i in indexes){
  pl <- as_parentslist(best[[i]])
  G <- graph_from_adjacency_matrix(as_adj_matrix(pl))
  el <- rbind(el,as_edgelist(G))}
  el <- unique(el)[-1,]
  qgraph(el,...)
}

data <- read.csv("D_SN147.dat")
d <- data %>% select("ETA", "SESSO","STCIV","ISTR","COND","RIP","HHFAM","SPOCON","AMICI2","BICALC","FUMO","TELE","LIBRI","PAESAGGIO","VOTOVI","FUTUASP","FIDUCIA")


d <- as.data.frame(apply(d,2, as.factor))


try <- d %>% select("FIDUCIA","SESSO","SPOCON","AMICI2","VOTOVI") 
try <- na.omit(try)

#try <- subset(try, FUTUASP != "4")
try <- subset(try, AMICI2 != "3")

levels(try$VOTOVI) <- c("low","low","high","low","low","low","low","medium","medium","high","high")

#try <- subset(try, FUTUASP != "4")

best <- all_orders(data = try, alg = stages_bhc, lambda = 0, join_unobserved = FALSE)
bics <- sapply(best,BIC)
plot(bics)
indexes <- which(bics == min(bics))

pdag(best,indexes)


try <- d %>% select("COND","SESSO","SPOCON","AMICI2","VOTOVI") 
try <- na.omit(try)

#try <- subset(try, FUTUASP != "4")
try <- subset(try, AMICI2 != "3")

levels(try$VOTOVI) <- c("low","low","high","low","low","low","low","medium","medium","high","high")


best <- all_orders(data = try, alg = stages_bhc, lambda = 0, join_unobserved = FALSE)
bics <- sapply(best,BIC)
plot(bics)


indexes <- which(bics <= sort(bics)[4])
pdag(best,indexes)
