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
  print(df)

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

#get_extract_data("pavlopoulos g [AU]", c("0>"))

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

killDbConnections()