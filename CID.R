CID <- function(object1, object2){
  vs1 <- names(object1$tree)
  vs2 <- names(object2$tree)
  wrong <- list()
  for (v in vs1){
    if (is.null(object1$stages[[v]])) object1$stages[[v]] <- "1"
    wrong[[v]] <- sapply(object1$stages[[v]], function(x) 0)
    stages2 <- unique(object2$stages[[v]])
    if (is.null(stages2)) stages2 <- "1"
    ## get all paths with stages 
    j1 <- which(names(object1$tree) == v)
    j2 <- which(names(object2$tree) == v)
    both <- intersect(vs1[1:(j1-1)], vs2[1:(j2-1)])
    if (j1 > 1) paths1 <- expand.grid(object1$tree[(j1-1):1])
    if (j1 == 1) paths1 <- NA
    if (is.null(dim(paths1))) dim(paths1) <- c(1,1)
    for (s2 in stages2){
      paths2 <- get_path(object2, v, s2)
      if (is.null(dim(paths2))) dim(paths2) <- c(1,length(paths2))
      whichpaths1 <- c(apply(paths2,1, function(x){
        which(apply(paths1, 1, function(y) all(y[both] == x[both])))
      }))
      stages1 <- object1$stages[[v]][whichpaths1]
      for (id in whichpaths1){
        if (length(unique(stages1)) > 1){
          wrong[[v]][id] <- wrong[[v]][id] + 1   
        }
      }
    }
    wrong[[v]] <- ifelse(wrong[[v]] > 0, 1, 0)
  }
  return(list(wrong = wrong, dist = sum(sapply(wrong, mean))))
}
