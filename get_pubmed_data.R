get_pubmed_data <- function(pmids, remote=TRUE) {
  
  if(remote) {
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
  
  }
  
  return(df_of_articles)
}