# get_pubmed_data <- function(query, remote=FALSE) {
#   
#   if(remote) {
#   #Get the pubmed ids for the query
#   pmids <- get_pubmed_ids(query)
#   
#   # Retrieve pubmed data in XML format given the PMIDs
#   pubmed_data_xml <- easyPubMed::fetch_pubmed_data(pmids)
#   
#   # Store Pubmed Records as elements of a list
#   all_xml <- easyPubMed::articles_to_list(pubmed_data_xml)
#   
#   # convert xml PubMed documents to data frame
#   df_of_articles <- do.call(rbind, lapply(all_xml, easyPubMed::article_to_df, 
#                                           max_chars = -1, getAuthors = FALSE))
#   }
#   # retrieve from local repository
#   else {
#     pubmedDB = mongo(collection="articles", db="pubmed2")
#     runjs("console.log('localPub 1')")
#     allDocs <-data.frame(matrix(ncol = 4, nrow = 0))
#     columnNames <- c("pmid", "year", "title", "abstract")
#     colnames(allDocs) <- columnNames
#     #print(allDocs)
#     
#     #Rentrez web history
#     runjs("console.log('localPub 1.1')")
#     
#     #Web history, problem: saves one history every 24h
#     # pmidsWeb<-entrez_search(db="pubmed", term = query, use_history=TRUE)$web_history
#     # #print(pmidsWeb)
#     # runjs("console.log('localPub 1.2')")
#     # query_summ <- entrez_summary(db="pubmed", web_history=pmidsWeb)
#     # #print(query_summ)
#     # #pmids <-extract_from_esummary(query_summ, "uid")
#     # runjs("console.log('localPub 1.3')")
#     # 
#     # # Get the names of the esummary records
#     # esummary_names <- names(query_summ)
#     # 
#     # # Extract the IDs from the esummary names
#     # pmids <- gsub("`", "", esummary_names)
#     
#     pmids<-entrez_search(db="pubmed", term = query, retmax=10000)$ids
#     
#     # Print the list of IDs
#     print(pmids)
#     
#     #for (id in pmids$IdList){
#     for (id in pmids){
#       runjs("console.log('localPub 2')")
#       print(typeof(id))
#       runjs("console.log('localPub 2.1')")
#       #print(toString(id))
#       #query <- paste('{"pmid":"',toString(id),'"}',sep = "")
#       query <- paste('{"pmid":"',id,'"}',sep = "")
#       runjs("console.log('localPub 2.2')")
#       #query <- gsub("/", "", query)
#       #print(query)
#       article <- pubmedDB$find(query)
#       runjs("console.log('localPub 3')")
#       print(article)
#       if(nrow(article)>0) {
#         allDocs[nrow(allDocs) + 1,] <- c(article$pmid,article$pubdate,article$title,article$abstract)
#       }
#       runjs("console.log('localPub 4')")
#     }
#     df_of_articles <- allDocs
#     runjs("console.log('localPub 5')")
#   }
#   
#   return(df_of_articles)
# }
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
    pubmedDB = mongo(collection="articles", db="pubmed2", url="mongodb://172.17.0.1")
    runjs("console.log('localPub 1')")
    
    pmids<-entrez_search(db="pubmed", term = query, retmax=10000)$ids
    #pmids<-as.character(pmids)
    runjs("console.log('localPub 2')")
    #print(pmids)
    
    #query <- paste('{"pmid": { "$in":[',pmids,']}}',sep = "")#{pmid: { $in: ['A', 'D']}}
    query <- '{"pmid": { "$in": ['
    idStr<-paste("\"",pmids[1],"\"",sep="")
    for (id in pmids[2:length(pmids)]) {
      idStr <- paste(idStr, ", \"", id, "\"", sep="")
    }
    query <- paste(query, idStr, ']}}', sep="")
    #query <- '{"pmid": { "$in": ["25448298", "25100685", "23866856"]}}'
    print(query)
    localRes <- pubmedDB$find(query)
    runjs("console.log('localPub 3')")
    years<-(localRes$pubdate)
    #print(names(localRes))
    #localResultsDf <- as.data.frame(localRes)
    #print(localResultsDf)
    
    allDocs <-data.frame(matrix(ncol = 4, nrow = length(localRes$pmid)))
    columnNames <- c("pmid", "year", "title", "abstract")
    colnames(allDocs) <- columnNames
    
    allDocs$pmid<-localRes$pmid
    allDocs$year<-years
    allDocs$title<-localRes$title
    allDocs$abstract<-localRes$abstract
    
    runjs("console.log('localPub 4')")
    
    df_of_articles <- allDocs
    #print(df_of_articles)
    runjs("console.log('localPub 5')")
  }
  
  return(df_of_articles)
}
