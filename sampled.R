library(PerMallows)

source("sampling_sevt.R")
source("methods.R")
source("CID.R")

ps <- c(2, 3, 4, 5) ## num variables
Ns <- c(100, 250, 500, 1000, 2000, 3000, 5000, 10000) ## sample size
ks <- c(2, 3, 4) ## complexity (stages per stratum or prob_edges = k / (p-1))
ls <- c(2,3,4) ## maximum number of levels per variable 

sampling_methods_names <- c("random_sevt")   #, "random_bnsevt")
sam_meth <- list(
  random_sevt = random_sevt,
  random_bnsevt = random_bnsevt
)

## select here whoch methods to run
methods_name <- c( "best_kmeans" , 
                  "best_bhc" ,  
                  "bn_tabu" , 
                  "bn_mmhc"
                  )  

## subset methods to run
methods <- methods[methods_name]

for (nm in sampling_methods_names){
  for (k in ks){
    for (l in ls){
      for (p in ps){
        for (N in Ns){
          message(nm, " k:", k, " l:", l, " p:", p, " N:", N)
          pb <- txtProgressBar(style = 3)
          results <- lapply(1:100,function(i) {
            true <- sam_meth[[nm]](n = p, k, l)
            DD <- as.data.frame(sample_from(true, nsim = N)[,sample(1:p)])
            res <- sapply(methods, function(mth) mth(DD),USE.NAMES = TRUE, simplify = FALSE)
            dis <- sapply(res, function(x) distance(x$order), USE.NAMES = TRUE)
            bics <- sapply(res, function(x) ifelse(is.null(x$bic), NA, x$bic))
            cids <- sapply(res, function(x) int_dist(true, x$model)$dist)
            times <- sapply(res, function(x) ifelse(is.null(x$time), NA, x$time))
            setTxtProgressBar(pb, i/ 100)
            return(list(distance = dis, bic = bics, time = times, res = res, cid = cids, true = true, data = DD))
          })
          dir.create(paste0("results/",  nm, "/"), recursive = TRUE, showWarnings = FALSE)
          saveRDS(results, file = paste0("results/",  nm, "/","p", p, "N", N, "k", k, 'l', l, "results.rds"))
          close(pb)
        }
      }
    }
  }
}
