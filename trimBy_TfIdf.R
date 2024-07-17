dfm_trimabs <- function(x, min) {
  
  maxvals <- sapply(
    
    split
    (dfmat3@x, featnames(dfmat3)[as(x, "dgTMatrix")@j + 1]),
    max
  )
  
  dfm_keep(x, names(maxvals)[maxvals >= min])
}

  