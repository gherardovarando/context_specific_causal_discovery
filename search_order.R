all_orders <- function(data, alg = stages_bhc, 
                       join_unobserved = TRUE, lambda = 0, ...){
  if (is.data.frame(data)){
    vv <- colnames(data)
  }
  if (is.table(data)){
    vv <- names(dimnames(data))
  }
  allres <- combinat::permn(vv, fun = function(order){
    alg(full(data, order =  order, 
             join_unobserved = join_unobserved, lambda = lambda))
  })    
  return(allres)
}


search_all <- function(data, alg = stages_bhc, 
                       search_score = BIC,
                       join_unobserved = TRUE, lambda = 0, ...){
  if (is.data.frame(data)){
    vv <- colnames(data)
  }
  if (is.table(data)){
    vv <- names(dimnames(data))
  }
  allres <- combinat::permn(vv, fun = function(order){
    alg(full(data, order =  order, 
             join_unobserved = join_unobserved, lambda = lambda))
  })    
  scores <- sapply(allres, search_score)
  return(allres[[which.min(scores)]])
}


sevt_add <- function(object, var, data, join_unobserved = TRUE){
  if (is.data.frame(data)) {
    data <- table(data[, c(names(object$tree), var)])
  }
  path <- names(object$tree)
  tt <- apply(data, MARGIN = c(path, var), sum)
  ctable <- ftable(tt, col.vars = var, row.vars = path)
  tmp <- sevt(data, full = TRUE, order = c(path, var))
  object$tree <- tmp$tree
  object$stages[[var]] <- tmp$stages[[var]]
  object$ctables[[var]] <- ctable
  object$prob[[var]] <- lapply(seq_along(object$stages[[var]]), function(ix){
    tt <- ctable[ix, ]
    names(tt) <- object$tree[[var]]
    n <- sum(tt)
    tt <- (tt + object$lambda)
    tt <- tt / sum(tt)
    tt[is.nan(tt)] <- NA
    attr(tt, "n") <- n
    return(tt)
  })
  names(object$prob[[var]]) <- object$stages[[var]]
  if (join_unobserved){
    ix <- rowSums(ctable) == 0
    if (any(ix)){
      object$stages[[var]][ix] <- object$name_unobserved[1]
      p.unobserved <- object$prob[[var]][ix][[1]]
      object$prob[[var]][ix] <- NULL
      object$prob[[var]][[object$name_unobserved[1]]] <- p.unobserved
    }  
  }
  object$ll <- NULL ## force recompute log-likelihood
  object$ll <- logLik(object)
  return(object)
}

### should use github version in branch main
search_greedy <- function(data, alg = stages_bhc, search_score = BIC, lambda = 0, 
                          join_unobserved = TRUE, ...){
  if (is.data.frame(data)){
    vs <- colnames(data)
  }
  if (is.table(data)){
    vs <- names(dimnames(data))
  }
  ## initialize best
  best <- full(data, order = vs[1], lambda = lambda, join_unobserved = join_unobserved)
  ## check all other possible first variable
  if (length(vs) < 2) return(best)
  for (v in vs){
    tmp <- full(data, order = v, lambda = lambda, join_unobserved = join_unobserved)
    #print(score(tmp))
    if (search_score(tmp) < search_score(best)){
      best <- tmp
    }
  }
  object <- best
  ## add the best one by one 
  svs <- vs[!(vs %in% names(object$tree))]
  for (i in seq_along(vs)[-1]){
    #done <- FALSE
    best <- alg(sevt_add(object, svs[1], data, join_unobserved = join_unobserved),
                scope = svs[1], ...)
    for (v in svs[-1]){
      tmp <- alg(sevt_add(object, v, data, join_unobserved = join_unobserved), 
                 scope = v, ...)
      if (search_score(tmp) < search_score(best)){
        best <- tmp
        #done <- TRUE
      }  
    }
    #if (!done) break
    object <- best
    svs <- vs[!(vs %in% names(object$tree))]     
  }
  return(object)
}

bls <- function(data, left, new, alg, search_score = BIC, 
                lambda, join_unobserved, ...){
  m <- full(data = data, order = left, 
            join_unobserved = join_unobserved,
            lambda = lambda)
  s <- search_score(m)
  m <- alg(sevt_add(m, new, data, join_unobserved = join_unobserved), scope = new, ...)
  return(search_score(m) - s)
}





search_dynamic <- function(data, alg = stages_bhc, search_score = BIC, lambda = 0, 
                           join_unobserved = TRUE, ...){
  if (is.data.frame(data)){
    vs <- colnames(data)
  }
  if (is.table(data)){
    vs <- names(dimnames(data))
  }
  ## initialize scores with 1 variables
  scores <- sapply(vs, FUN = function(vv){
    search_score(full(data, order = vv, join_unobserved = join_unobserved, 
                      lambda = lambda))
  }, USE.NAMES = TRUE )
  sinks <- vs
  names(sinks) <- vs
  for (i in seq_along(vs)[-1]){
    sets <- combn(vs,i)
    tmp <- apply(sets, MARGIN = 2, FUN = function(W){
      sapply(W, function(v){
        scores[paste(W[W!=v],collapse="-")] + bls(data, W[W!=v], v, alg, 
                                                  search_score, 
                                                  lambda = lambda,
                                                  join_unobserved = join_unobserved, 
                                                  ...)
      })
    })
    new_sinks <- sapply(seq(ncol(sets)), function(i) sets[which.min(tmp[,i]),i])
    nam <- apply(sets, MARGIN = 2, FUN = paste, collapse = "-")
    sinks[nam] <- new_sinks
    scores[nam] <- apply(tmp, 2, min)
  }
  left <- paste(vs, collapse = "-")
  order <- c()
  for (i in length(vs):1){
    order[i] <- sinks[left]
    left <- paste(vs[!(vs %in% order)], collapse = "-")
  }
  object <- alg(full(data, order = order, join_unobserved = join_unobserved, 
                     lambda = lambda), ...)
  return(object)
}

