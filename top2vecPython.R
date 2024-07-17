top2vec <- reticulate::import("top2vec")
numba <-reticulate:: import("numba")

source('make_clustering.R')

#py_config()
# py_discover_config()

get_extract_data <- function(my_query, entities, maxArticles=10000) {
  
  #print(is.numeric(my_query))
  splittedQuery <- strsplit(my_query, " +")
  
  maxDocs = maxArticles
  
  print(my_query)
  my_query<-paste(my_query, "1:2019[dp]", sep = " ")
  
  #Get all pmids
  res <- get_pubmed_ids(my_query)
  print("Work")
  
  #runjs("console.log('After query 1')")
  
  test <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
                "db=pubmed&&term=", res$OriginalQuery, "&usehistory=n&retmax=",maxDocs, sep = "")
  
  #runjs("console.log('After query 2')")
  
  res_url <- url(test,open="rb", encoding = "UTF8")
  on.exit(close(res_url))
  idXML <- readLines(res_url, warn = FALSE, encoding = "UTF8")
  collect_ids <- list()
  for (i in 1:length(idXML)) {
    if (grepl("^(\\t){0,1}<Id>", idXML[i])) {
      xx <- custom_grep(idXML[i], tag = "Id", format = "char")
      collect_ids[[length(collect_ids) + 1]] <- as.character(xx[1])
    }
  }
  myIDlist <- as.character(do.call(c, collect_ids))
  
  
  #Connect and get from extract db
  con <- dbConnect(RMySQL::MySQL(), dbname = "extract", user="theodos", password="89Lilu#1")
  #df <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),")")) %>% group_by(PMID) %>% summarize(Type=paste(EntityType, collapse="."), New=paste(Word, collapse="."),  Id=paste(ID, collapse=".")) %>% rename(pmid=PMID)
  
  #Check which entity types are required by the user
  if (length(entities)>0 && "0>" %in% entities) {
    editedEntities <- entities[ !entities == "0>"]
    print(length(editedEntities))
    #Check if there are other entities
    if (length(editedEntities)>0) {
      allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),") AND (entityType>0 OR entityType IN (",toString(editedEntities),"))"))
    }else{
      allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),") AND entityType>0"))
    }
    #allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),") AND (entityType>0 OR entityType IN (",toString(entities),"))"))
    
  }else if (length(entities)>0) {
    allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),") AND entityType IN (",toString(entities),")"))
  }else{
    allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),")"))
  }
  #allData <- dbGetQuery(conn = con, paste("SELECT * FROM EntityWords WHERE pmid IN (",toString(myIDlist),") AND entityType IN (",toString(entities),")"))
  print(colnames(allData))
  
  #Get Word-Type
  wordTypeTable <- allData[, c('Word', 'EntityType')]
  print(wordTypeTable)
  
  #Format allData for clusternig etc
  df <- allData %>% group_by(PMID) %>% summarize(Type=paste(EntityType, collapse="."), New=paste(Word, collapse="."),  Id=paste(ID, collapse=".")) %>% rename(pmid=PMID)

  
  #Get one column df$columnName
  #extract_corpus <- corpus(df, text_field='New', docid_field = 'pmid')
  extract_corpus <- corpus(df, text_field='New', docid_field = 'pmid')
  corpus_tokens <- extract_corpus %>% tokenize_regex(pattern = "\\.")
  articles_dfm <- dfm(tokens(corpus_tokens))
  #articles_dfm <- dfm(extract_corpus)
  #print('Articles dfm')
  #print(articles_dfm)
  #print(colSums(articles_dfm))
  
  #Change column names for term- frequency table
  sumVec <- colSums(articles_dfm)
  termFreq <- data.table::as.data.table(sumVec, keep.rownames = TRUE)
  colnames(termFreq) <- c('Term', 'Frequency')
  
  results <- list()
  #results$articles_corpus <- articles_corpus
  #results$corpus_tokens <- corpus_tokens
  results$articles_dfm <- articles_dfm
  results$term_frequency <- termFreq
  results$wordTypeTb <- wordTypeTable
  results$untokenizedExtract <- df
  
  return(results)
}

#extractResults <- get_extract_data("cancer", c())
extractResults <- get_extract_data("(anterior-posterior AND drosophila) OR (dorsal-ventral AND drosophila) 1:2001/03[dp]", c())

#Top2Vec model
spacedDocs <- gsub("\\.", " ", extractResults$untokenizedExtract$New)
print(spacedDocs)
#vocab <- top2vec$build_vocab(extractResults$untokenizedExtract$New)
model <- top2vec$Top2Vec(spacedDocs, speed="fast-learn")
#model <- top2vec$Top2Vec(extractResults$untokenizedExtract$New)
model$get_num_topics()
model$get_topic_sizes()
model$doc_top


clusteredDocs <- data.frame(extractResults$untokenizedExtract$pmid, model$doc_top)
names(clusteredDocs) <- c("pmid", "cluster")

results<-list()
results$df <- as_tibble(clusteredDocs)
results$model <- model$model
print(results$df)

#Run the function from make_clustering.R
top2vec_clustering <- function(dfm, clustering_param, dist_sim_param){
  
  dfm <- dfm$untokenizedExtract
  extractTokens <- gsub("\\.", " ", dfm$New)
  docIds <- dfm$pmid
  
  #Set reticulate python version to 3.8
  reticulate::use_python("/usr/bin/python3.8")
  
  #crete top2vec model
  top2vec <- reticulate::import("top2vec")
  
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
  
  
  # details <- reticulate::py_discover_config()
  # if (details$version == "3.8"){
  # }else{
  # }
  
  model <- top2vec$Top2Vec(extractTokens, speed="fast-learn")

  
  clusteredDocs <- data.frame(docIds, model$doc_top)
  names(clusteredDocs) <- c("pmid", "cluster")
  
  results<-list()
  results$df <- as_tibble(clusteredDocs)
  results$model <- model$model
  
  print(results)
}

top2vec_clustering(extractResults, "", "")

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

killDbConnections()

#Sklearn newsgorups
# sklearnDatasets <- reticulate::import("sklearn.datasets")
# sklearnDatasets$fetch_20newsgroups
# 
# newsgroups <- sklearnDatasets$fetch_20newsgroups(subset='all', remove=c('headers', 'footers', 'quotes'))
# 
# print(length(newsgroups$data))
# 
# model <- top2vec$Top2Vec(newsgroups$data[1:500], speed="fast-learn")

