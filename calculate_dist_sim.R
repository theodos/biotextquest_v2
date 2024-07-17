calculate_dist_sim <- function(dfm, metric = "cosine") {
  switch(metric,
         "cosine" = textstat_simil(dfm, margin = "documents", method = "cosine",  min_simil=0.5),
         "jaccard" = textstat_simil(dfm, margin = "documents", method = "jaccard"),
         "euclidean" = textstat_dist(dfm, margin = "documents", method = "euclidean")
         )
}