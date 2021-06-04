library(stagedtrees)
library(bnlearn)
source("search_order.R")
source("bhc_cstree.R")

method_sevt <- function(data, search, alg = stages_bhc, lambda = 0, join_unobserved = TRUE, ...){
  time <- system.time(out <- search(data = data, alg = alg, lambda = lambda, 
                            join_unobserved = join_unobserved, ...))
  order <- stagedtrees:::sevt_varnames(out)
  p <- sort(order, index = TRUE)$ix
  return(list(order = p, model = out, bic = BIC(out), time = time))
}

method_bn <- function(data, alg = bnlearn::hc, ...){
  time <- system.time(bn <- alg(data))
  p <- sort(bnlearn::node.ordering(bn), index = TRUE)$ix
  return(list(order = p, model = as_sevt(bn, values = lapply(data, levels)), bic = -2*BIC(bn, data), time = time))
}


methods <- list(
  greedy_kmeans = function(data){method_sevt(data, search_greedy, stages_kmeans, 1, TRUE, k = 2)},
  best_kmeans = function(data){method_sevt(data, search_dynamic, stages_kmeans, 1, TRUE, k = 2)},
  greedy_hclust = function(data){method_sevt(data, search_greedy, stages_hclust, 1, TRUE, k = 2)},
  best_hclust = function(data){method_sevt(data, search_dynamic, stages_hclust, 1, TRUE, k = 2)},
  greedy_bhc = function(data){method_sevt(data, search_greedy, stages_bhc, 0, TRUE)},
  best_bhc = function(data){method_sevt(data, search_dynamic, stages_bhc, 0, TRUE)},
  greedy_csbhc = function(data){method_sevt(data, search_greedy, stages_csbhc, 0, TRUE)},
  best_csbhc = function(data){method_sevt(data, search_dynamic, stages_csbhc, 0, TRUE)},
  best_bj = function(data){method_sevt(data, search_dynamic, stages_bj, 0, TRUE)},
  bn_hc = function(data){method_bn(data, alg = bnlearn::hc)},
  bn_tabu = function(data){method_bn(data, alg = bnlearn::tabu)},
  bn_mmhc = function(data){method_bn(data, alg = bnlearn::mmhc)},
  bn_h2pc = function(data){method_bn(data, alg = bnlearn::h2pc)},
  random = function(data){list(order = sample(1:ncol(data)))}
)
