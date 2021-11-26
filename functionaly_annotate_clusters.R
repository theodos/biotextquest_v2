functionaly_annotate_clusters <- function(dfm, clustering_results) {
  grouped_dfm <- dfm_group(dfm, groups = clustering_results$cluster)
  df <- convert(grouped_dfm, to = "data.frame")
 
  
noclusters <- nrow(df)
greater_than_freq <- function(x) {
    x > 2
}

tsv_url <- "http://tagger.jensenlab.org/GetHTML"

for (i in noclusters) {
  cluster <- df[i,sapply(df[i,], greater_than_freq)]
  query_params <- list(document = str_c(colnames(cluster),collapse=" "), entity_types ='-2 -25 -26 -27',
                                           format='tsv'
                       )
  tsv_data <- GET(tsv_url, query = query_params)
  results <- gsub(x = content(tsv_data, as = 'text'), pattern = "</body>.*", replacement = "")
  #print(str(cluster))
}

  return(results)
  }

