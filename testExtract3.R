library(RMySQL)
library(dplyr)
library(quanteda)
library(tokenizers)
library(reticulate)
library(mongolite)
get_extract_data <- function(my_query, entities, maxArticles=10000) {
  
  #print(is.numeric(my_query))
  splittedQuery <- strsplit(my_query, " +")
  
  maxDocs = maxArticles
  
  print(my_query)
  #my_query<-paste(my_query, "1:2019[dp]", sep = " ")
  
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
  con <- dbConnect(RMySQL::MySQL(), dbname = "extract_2023", user="theodos", password="89Lilu#1")
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
  wordTypeIDTable <- allData[, c('Word', 'EntityType', 'Ext_db_id')]
  print(wordTypeIDTable)
  
  #Format allData for clusternig etc
  df <- allData %>% group_by(PMID) %>% summarize(Type=paste(EntityType, collapse="."), New=paste(Word, collapse="."),  Id=paste(ID, collapse=".")) %>% rename(pmid=PMID)
  #print(nrow(df))
  
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
  results$wordTypeIDTb <- wordTypeIDTable
  results$untokenizedExtract <- df
  
  return(results)
}

#get_extract_data("pavlopoulos g [AU]", c())
#get_extract_data("cancer 1:2019[dp]", c(">0"))

#source('make_clustering.R')


kmeans_clustering <- function(dfm, clustering_param) {
  dfm <- dfm$article_dfm_forClustering
  #runjs("console.log('InCluster 2.1.1')")
  #runjs("document.getElementById('myBar').style.width = '50%';")
  set.seed(1) #Added because kmeans picks a random number of tokens as centers every time
  km_out <- stats::kmeans(dfm, centers = clustering_param ) #Εδώ πεθαίνει η extract/ Εκτός αν ανοίξω τα advanced και δούλεψε;;;Πιθανώς κάτι με το switch (input$clustering_param)
  #runjs("console.log('InCluster 2.1.2')")
  colnames(km_out$centers) <- featnames(dfm)
  #runjs("console.log('InCluster 2.1.3')")
  #runjs("document.getElementById('myBar').style.width = '70%';")
  df_clustering <- broom::augment(km_out, data = convert(dfm, to="data.frame"))
  #runjs("console.log('InCluster 2.1.4')")
  df_clustering_trun <- df_clustering %>% select(doc_id,.cluster)
  #runjs("console.log('InCluster 2.1.5')")
  colnames(df_clustering_trun) <- c("pmid","cluster") #OLD
  #colnames(df_clustering) <- c("pmid","cluster") #ME
  #runjs("console.log('InCluster 2.1.6')")
  results <- list()
  results$model <- km_out
  results$df <- df_clustering_trun #OLD
  #results$df <- df_clustering #ME
  #runjs("console.log('InCluster 2.1.7')") #Έβαλα αυτό και δούλεψε η pubmed
  #runjs("document.getElementById('myBar').style.width = '90%';")
  return(results)
  #return(km_out$centers)
}

top2vec_clustering <- function(dfm, clustering_param, dist_sim_param){
  #runjs("console.log('InCluster 4.1')")
  
  dfm <- dfm$untokenizedExtract
  #runjs("console.log('InCluster 4.11')")
  extractTokens <- gsub("\\.", " ", dfm$New)
  if (length(extractTokens)>0){
    #runjs("console.log('extractTokens>0')")
  }else{
    #runjs("console.log('extractTokens is empty')")
  }
  #runjs("console.log('InCluster 4.12')")
  docIds <- dfm$pmid
  #print(docIds)
  
  #Set reticulate python version to 3.8
  reticulate::use_python("/usr/bin/python3.8")
  
  #crete top2vec model
  top2vec <- reticulate::import("top2vec")
  #runjs("console.log('InCluster 4.2')")
  
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
    #runjs("console.log('Version: 3.8')")
  }else{
    #runjs("console.log('Other version')")
  }
  
  model <- top2vec$Top2Vec(extractTokens, speed="fast-learn")
  #runjs("console.log('InCluster 4.3')")
  
  clusteredDocs <- data.frame(as.character(docIds), model$doc_top)
  names(clusteredDocs) <- c("pmid", "cluster")
  #print(clusteredDocs)
  #runjs("console.log('InCluster 4.4')")
  
  results<-list()
  results$df <- as_tibble(clusteredDocs)
  #runjs("console.log('InCluster 4.5')")
  results$model <- model$model
  #runjs("console.log('InCluster 4.6')")
  return(results)
}

get_pubmed_data <- function(query, remote=FALSE) {
  
  if(remote) {
    #Get the pubmed ids for the query
    pmids <- get_pubmed_ids(query)
    
    # Retrieve pubmed data in XML format given the PMIDs
    pubmed_data_xml <- easyPubMed::fetch_pubmed_data(pmids)
    
    # Store Pubmed Records as elements of a list
    all_xml <- easyPubMed::articles_to_list(pubmed_data_xml)
    
    # convert xml PubMed documents to data frame
    df_of_articles <- do.call(rbind, lapply(all_xml, easyPubMed::article_to_df, 
                                            max_chars = -1, getAuthors = FALSE))
  }
  # retrieve from local repository
  else {
    pubmedDB = mongo(collection="articles", db="pubmed2")
    #runjs("console.log('localPub 1')")
    #print(allDocs)
    
    #Rentrez web history
    #runjs("console.log('localPub 1.1')")
    
    #Web history, problem: saves one history every 24h
    # pmidsWeb<-entrez_search(db="pubmed", term = query, use_history=TRUE)$web_history
    # #print(pmidsWeb)
    # runjs("console.log('localPub 1.2')")
    # query_summ <- entrez_summary(db="pubmed", web_history=pmidsWeb)
    # #print(query_summ)
    # #pmids <-extract_from_esummary(query_summ, "uid")
    # runjs("console.log('localPub 1.3')")
    # 
    # # Get the names of the esummary records
    # esummary_names <- names(query_summ)
    # 
    # # Extract the IDs from the esummary names
    # pmids <- gsub("`", "", esummary_names)
    
    pmids<-entrez_search(db="pubmed", term = query, retmax=10000)$ids
    #pmids<-as.character(pmids)
    # Print the list of IDs
    #print(pmids)
    
    #query <- paste('{"pmid": { "$in":[',pmids,']}}',sep = "")#{pmid: { $in: ['A', 'D']}}
    query <- '{"pmid": { "$in": ['
    idStr<-paste("\"",pmids[1],"\"",sep="")
    for (id in pmids[2:length(pmids)]) {
      idStr <- paste(idStr, ", \"", id, "\"", sep="")
    }
    query <- paste(query, idStr, ']}}', sep="")
    #query <- '{"pmid": { "$in": ["25448298", "25100685", "23866856"]}}'
    #print(query)
    localRes <- pubmedDB$find(query)
    print(names(localRes))
    print(localRes$pubdate)
    years<-(localRes$pubdate)
    #localResultsDf <- as.data.frame(localRes)
    #print(localResultsDf)
    
    allDocs <-data.frame(matrix(ncol = 4, nrow = length(localRes$pmid)))
    columnNames <- c("pmid", "year", "title", "abstract")
    colnames(allDocs) <- columnNames
    print(colnames(allDocs))
    
    allDocs$pmid<-localRes$pmid
    allDocs$year<-years
    allDocs$title<-localRes$title
    allDocs$abstract<-localRes$abstract
    
    #for (id in pmids$IdList){
    # for (id in pmids){
    #   #runjs("console.log('localPub 2')")
    #   print(typeof(id))
    #   #runjs("console.log('localPub 2.1')")
    #   #print(toString(id))
    #   #query <- paste('{"pmid":"',toString(id),'"}',sep = "")
    #   query <- paste('{"pmid":"',id,'"}',sep = "")
    #   #runjs("console.log('localPub 2.2')")
    #   #query <- gsub("/", "", query)
    #   print(query)
    #   article <- pubmedDB$find(query)
    #   #runjs("console.log('localPub 3')")
    #   #print(article)
    #   if(nrow(article)>0) {
    #     allDocs[nrow(allDocs) + 1,] <- c(article$pmid,article$pubdate,article$title,article$abstract)
    #   }
    #   #runjs("console.log('localPub 4')")
    # }
    df_of_articles <- allDocs
    print(colnames(df_of_articles))
    print(df_of_articles$year)
    #runjs("console.log('localPub 5')")
  }
  
  return(df_of_articles)
}
df_of_articles <- get_pubmed_data("theodosiou t [au]")


allResults<-list()
allResults$article_dfm_forClustering <- extractResults$articles_dfm
allResults$articles_dfm <- extractResults$term_frequency
allResults$wordTypeTb <- extractResults$wordTypeIDTb #this can be omitted/used directly
allResults$untokenizedExtract <- extractResults$untokenizedExtract

clustering_results <- kmeans_clustering(allResults, clustering_param = 2)$df
#clustering_results <-top2vec_clustering(allResults, "", "")$df
df_of_articles <- get_pubmed_data("(anterior-posterior AND drosophila) OR (dorsal-ventral AND drosophila) 1:2001/03[dp]")
#extractResults <- get_extract_data("(anterior-posterior AND drosophila) OR (dorsal-ventral AND drosophila) 1:2001/03[dp]", c())
#extractResults <- get_extract_data('human [MeSH Terms] AND ("Prophase"[MeSH Terms] OR "Prometaphase"[MeSH Terms] OR "Metaphase"[ MeSH Terms] OR "Anaphase"[MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms] OR "G2 Phase"[MeSH Terms)) AND ("2000/01/01"[PDAT]: "2013/01/01"[PDAT])', c("1"))
#extractResults <- get_extract_data('human [MeSH Terms] AND ("G1 Phase"[MeSH Terms]) NOT ("S Phase"[MeSH Terms] OR "DNA Replication"[MeSH Terms] OR "G2 Phase"[MeSH Terms] OR "Prophase" [MeSH Terms] OR "Prometaphase" [MeSH Terms] OR "Metaphase"[MeSH Terms] OR "Anaphase"[MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis" [MeSH Terms]) AND ("2000/01/01"[PDAT]: "2013/01/31"[PDAT])', c("1"))
#extractResults <- get_extract_data('human [MeSH Terms] AND ("S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "G2 Phase"[MeSH Terms] OR "Prophase"[MeSH Terms] OR "Prometaphase"[ MeSH Terms] OR "Metaphase" [MeSH Terms] OR "Anaphase [MeSH Terms] OR "Telophase" [MeSH Terms] OR "Cytokinesis" [MeSH Terms]) AND ("2000/01/01"[PDAT]: "2013/01/01"[PDAT])', c("1")) 
#extractResults <- get_extract_data('human [MeSH Terms] AND ("G2 Phase" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms] OR "Prophase" [MeSH Terms] OR "Prometaphase"[ MESH Terms] OR "Metaphase"[MeSH Terms] OR "Anaphase" [MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis [MeSH Terms]) AND ("2000/01/01" [PDAT]: "2013/01/01"[PDAT])', c("1"))

#NO PDAT
#extractResults <- get_extract_data('human [MeSH Terms] AND ("Prophase"[MeSH Terms] OR "Prometaphase"[MeSH Terms] OR "Metaphase"[ MeSH Terms] OR "Anaphase"[MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms] OR "G2 Phase"[MeSH Terms))', c("1"))
#extractResults <- get_extract_data('human [MeSH Terms] AND ("G1 Phase"[MeSH Terms]) NOT ("S Phase"[MeSH Terms] OR "DNA Replication"[MeSH Terms] OR "G2 Phase"[MeSH Terms] OR "Prophase" [MeSH Terms] OR "Prometaphase" [MeSH Terms] OR "Metaphase"[MeSH Terms] OR "Anaphase"[MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis" [MeSH Terms])', c("1"))
#extractResults <- get_extract_data('human [MeSH Terms] AND ("S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "G2 Phase"[MeSH Terms] OR "Prophase"[MeSH Terms] OR "Prometaphase"[ MeSH Terms] OR "Metaphase" [MeSH Terms] OR "Anaphase [MeSH Terms] OR "Telophase" [MeSH Terms] OR "Cytokinesis" [MeSH Terms])', c("1")) 
extractResults <- get_extract_data('human [MeSH Terms] AND ("G2 Phase" [MeSH Terms]) NOT ("G1 Phase"[MeSH Terms] OR "S Phase"[MeSH Terms] OR "DNA Replication" [MeSH Terms] OR "Prophase" [MeSH Terms] OR "Prometaphase"[ MESH Terms] OR "Metaphase"[MeSH Terms] OR "Anaphase" [MeSH Terms] OR "Telophase"[MeSH Terms] OR "Cytokinesis [MeSH Terms])', c("1"))

write.csv(extractResults["wordTypeIDTb"], file = "my_list.csv")


#Table
print(clustering_results)
print(colnames(df_of_articles))
head(clustering_results$pmid, 3)
head(df_of_articles$pmid, 3)
setdiff(df_of_articles$pmid, clustering_results$pmid)
setdiff(clustering_results$pmid, clustering_results2$pmid)

counts<-table(clustering_results[, 2])
countDf <- as.data.frame.table(counts)
colnames(countDf) <-c("Cluster", "Total articles")
print(countDf)

# df_with_cluster <- inner_join(clustering_results,
#                               df_of_articles[,c("pmid", "year", "title", "abstract")],
#                               by="pmid")
# 
# pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
# df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
# # DT::datatable(df_with_cluster,
# #               filter = 'top',
# #               extensions = 'Buttons',
# #               options = list(dom = 'Blfrtip',
# #                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
# #                              lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
# #                              pageLength = 15),
# #               escape = c(3,4,5,6)
# # )# DT::datatable
#   
# print(df_with_cluster)

#Different results in clustering
dfm1 <- kmeans_clustering(allResults, clustering_param = 2)
setdiff(dfm1, dfm2)

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

killDbConnections()