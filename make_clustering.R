# General comment about distances/similarities:
#----------------------------------------------
# R’s various clustering functions work with distances, not similarities.
# We convert cosine similarity to cosine distance by subtracting it from 1.
# This works because cosine similarity is bound between 0 and 1.

source('calculate_dist_sim.R')
#source('as_label.R')

make_clustering <- function(dfm, clustering_algo = "kmeans", 
                            clustering_param = 2, dist_sim_param = "cosine") 
  {
  
  runjs("console.log('InCluster 1')")
  
  switch(clustering_algo,
         "kmeans" = kmeans_clustering(dfm, clustering_param),
         "mcl" = mcl_clustering(dfm, clustering_param, dist_sim_param),
         "louvain" = louvain_clustering(dfm, clustering_param, dist_sim_param),
         "top2vec" = top2vec_clustering(dfm, clustering_param, dist_sim_param)
         )

}


# Functions for clustering algorithms.
# Each one must return a data frame with a pmid and cluster column

kmeans_clustering <- function(dfm, clustering_param) {
  dfm <- dfm$article_dfm_forClustering
  runjs("console.log('InCluster 2.1.1')")
  runjs("document.getElementById('myBar').style.width = '50%';")
  set.seed(1) #Added because kmeans picks a random number of tokens as centers every time
  km_out <- stats::kmeans(dfm, centers = clustering_param ) #Εδώ πεθαίνει η extract/ Εκτός αν ανοίξω τα advanced και δούλεψε;;;Πιθανώς κάτι με το switch (input$clustering_param)
  runjs("console.log('InCluster 2.1.2')")
  colnames(km_out$centers) <- featnames(dfm)
  runjs("console.log('InCluster 2.1.3')")
  runjs("document.getElementById('myBar').style.width = '70%';")
  df_clustering <- broom::augment(km_out, data = convert(dfm, to="data.frame"))
  runjs("console.log('InCluster 2.1.4')")
  df_clustering_trun <- df_clustering %>% select(doc_id,.cluster)
  runjs("console.log('InCluster 2.1.5')")
  colnames(df_clustering_trun) <- c("pmid","cluster") #OLD
  #colnames(df_clustering) <- c("pmid","cluster") #ME
  runjs("console.log('InCluster 2.1.6')")
  results <- list()
  results$model <- km_out
  results$df <- df_clustering_trun #OLD
  #results$df <- df_clustering #ME
  runjs("console.log('InCluster 2.1.7')") #Έβαλα αυτό και δούλεψε η pubmed
  runjs("document.getElementById('myBar').style.width = '90%';")
  return(results)
}

mcl_clustering <- function(dfm, clustering_param, dist_sim_param) {
  dfm <- dfm$article_dfm_forClustering
  # Calculate distance/similarity similarity
  dist_sim <- calculate_dist_sim(dfm, metric = dist_sim_param) #ΑΥΤΟ ΘΕΛΩ ΓΙΑ ΤΟ IGRAPH με μετατροπές
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

louvain_clustering <- function(dfm, clustering_param, dist_sim_param) {
  dfm <- dfm$article_dfm_forClustering
  runjs("console.log('Louvain 1')")
  dist_sim <- calculate_dist_sim(dfm, metric = dist_sim_param)
  runjs("console.log('Louvain 2 (Post-cosine')")
  articlesIgraph <- graph_from_data_frame(dist_sim, vertices=NULL, directed=FALSE)
  runjs("console.log('Louvain 3')")
  #Louvain
  resultsLouvain <-cluster_louvain(articlesIgraph, weights = NULL, resolution = clustering_param)
  runjs("console.log('Louvain 4')")
  #create df, Should be done better
  
  pmid <- c()
  counter <- 1
  cluster <- c()
  for (comn in communities(resultsLouvain)) {
    print(comn)
    for (id in comn) {
      print(id)
      pmid <- append(pmid, id)
      cluster <- append(cluster, counter)
    }
    counter <- counter+1
  }
  runjs("console.log('Louvain 5')")
  results <- list()
  #results$df <- data.frame(pmid, cluster)
  results$df <- tibble(pmid, cluster)
  runjs("console.log('Louvain 6')")
  return(results)
}

top2vec_clustering <- function(dfm, clustering_param, dist_sim_param){
  runjs("console.log('InCluster 4.1')")

  dfm <- dfm$untokenizedExtract
  runjs("console.log('InCluster 4.11')")
  extractTokens <- gsub("\\.", " ", dfm$New)
  if (length(extractTokens)>0){
    runjs("console.log('extractTokens>0')")
  }else{
    runjs("console.log('extractTokens is empty')")
  }
  runjs("console.log('InCluster 4.12')")
  docIds <- dfm$pmid

  #Set reticulate python version to 3.8
  reticulate::use_python("/usr/bin/python3.8")
  
  #crete top2vec model
  top2vec <- reticulate::import("top2vec")
  runjs("console.log('InCluster 4.2')")
  
  # #Sklearn newsgroups
  # sklearnDatasets <- reticulate::import("sklearn.datasets")
  # sklearnDatasets$fetch_20newsgroups
  # 
  # newsgroups <- sklearnDatasets$fetch_20newsgroups(subset='all', remove=c('headers', 'footers', 'quotes'))
  # 
  # print(length(newsgroups$data))
  # 
  # model <- top2vec$Top2Vec(newsgroups$data[1:500], speed="fast-learn")
  # model$get_num_topics()
  # if (model$get_num_topics()>0){
  #   runjs("console.log('More than 0 topics')")
  # }else{
  #   runjs("console.log('Top2Vec is not working')")
  # }
  # #End of sklearn
  
  
  details <- reticulate::py_discover_config()
  if (details$version == "3.8"){
    runjs("console.log('Version: 3.8')")
  }else{
    runjs("console.log('Other version')")
  }
  if (length(extractTokens)<50) {
    runjs("alert('Not enough articles for clustering with top2vec')")
  }
  model <- top2vec$Top2Vec(extractTokens, speed="fast-learn")
  runjs("console.log('InCluster 4.3')")
  
  clusteredDocs <- data.frame(as.character(docIds), model$doc_top)
  names(clusteredDocs) <- c("pmid", "cluster")
  runjs("console.log('InCluster 4.4')")
  
  results<-list()
  results$df <- as_tibble(clusteredDocs)
  runjs("console.log('InCluster 4.5')")
  results$model <- model$model
  runjs("console.log('InCluster 4.6')")
  return(results)
}
