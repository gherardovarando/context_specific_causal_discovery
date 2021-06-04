random_sevt <- function(n, k = 2, l = 2){
  tree <- sapply(paste0("X", seq(n)), function(x){
    nl <- 1 + sample(l -1, size = 1)
    as.character(1:nl)
  }, USE.NAMES = TRUE, simplify = FALSE)
  model <- sevt(tree)
  model$stages <- lapply(model$stages, FUN = function(stages){
    paste0(sample(1:k, size = length(stages), replace = TRUE))
  })
  model$prob <- list()
  model$prob <- lapply(seq_along(model$stages), function(is){
    stages <- model$stages[[is]]
    ls <- model$tree[[is+1]]
    sapply(unique(stages), FUN = function(s){
      p <- runif(length(ls)) 
      p <- p / sum(p)
      names(p) <- ls
      attr(p, "n") <- 1
      return(p)
    }, simplify = FALSE, USE.NAMES = TRUE)
  })
  names(model$prob) <- names(model$stages)
  p <- runif(length(model$tree[[1]])) 
  p <- p / sum(p)
  names(p) <- model$tree[[1]]
  attr(p, "n") <- 1
  model$prob <- c(list("X1" = list("1" = p)), model$prob)
  return(model)
}



random_bnsevt <- function(n, k = 2, l = 2){
  vs <- paste0("X", seq(n))
  values <- sapply(vs, function(v){
    nl <- 1 + sample(l -1, size = 1)
    as.character(1:nl)
  } , USE.NAMES = TRUE, simplify = FALSE)
  bn <- bnlearn::random.graph(vs, 
                              method = "ordered", 
                              prob = k / (n-1))
  model <- as_sevt(bn, values = values)
  model$prob <- lapply(seq_along(model$stages), function(is){
    stages <- model$stages[[is]]
    ls <- model$tree[[is+1]]
    sapply(unique(stages), FUN = function(s){
      p <- runif(length(ls)) 
      p <- p / sum(p)
      names(p) <- ls
      attr(p, "n") <- 1
      return(p)
    }, simplify = FALSE, USE.NAMES = TRUE)
  })
  names(model$prob) <- names(model$stages)
  p <- runif(length(model$tree[[1]])) 
  p <- p / sum(p)
  names(p) <- model$tree[[1]]
  attr(p, "n") <- 1
  model$prob <- c(list("X1" = list("1" = p)), model$prob)
  return(model)
}
