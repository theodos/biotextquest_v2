# General comment about distances/similarities:
#----------------------------------------------
# R’s various clustering functions work with distances, not similarities.
# We convert cosine similarity to cosine distance by subtracting it from 1.
# This works because cosine similarity is bound between 0 and 1.

source('calculate_dist_sim.r')
#source('as_label.R')

make_clustering <- function(dfm, clustering_algo = "kmeans", 
                            clustering_param = 2, dist_sim_param = "cosine") 
  {
  
  switch(clustering_algo,
         "kmeans" = kmeans_clustering(dfm, clustering_param),
         "mcl" = mcl_clustering(dfm, clustering_param, dist_sim_param)
         )
}


# Functions for clustering algorithms.
# Each one must return a data frame with a pmid and cluster column

kmeans_clustering <- function(dfm, clustering_param) {
  km_out <- stats::kmeans(dfm, centers = clustering_param )
  colnames(km_out$centers) <- featnames(dfm)
  df_clustering <- broom::augment(km_out, data = convert(dfm, to="data.frame"))
  df_clustering_trun <- df_clustering %>% select(doc_id,.cluster)
  colnames(df_clustering_trun) <- c("pmid","cluster")
  results <- list()
  results$model <- km_out
  results$df <- df_clustering_trun
  return(results)
}

mcl_clustering <- function(dfm, clustering_param, dist_sim_param) {
  # Calculate distance/similarity similarity
  dist_sim <- calculate_dist_sim(dfm, metric = dist_sim_param)
  mcl_model <- mcl(dist_sim, inflation = clustering_param, max.iter = 1000, 
                   addLoops = FALSE)
  
  relabeled_clusters <- factor(mcl_model$Cluster,labels = c(1:length(unique(mcl_model$Cluster))))
  
  
  df_results <- data.frame("pmid" = docnames(dfm), "cluster" = relabeled_clusters)
  # debugging
  #print(mcl_model$Cluster)
  results <- list()
  results$model <- mcl_model
  results$df <- df_results
  
  return(results)
}
