make_dfm <- function(df_of_articles, title = TRUE, rmstopwords = TRUE, 
                     rmnumbers = TRUE, stemming = TRUE) 
  {
  
  # if you also want title in the analysis else use only abstract
  if(title) {
  df <- unite(df_of_articles,col="text",title:abstract)
  articles_corpus <- corpus(df, docid_field = 'pmid', text_field = 'text')
  }
  else {
    articles_corpus <- corpus(df, docid_field = 'pmid', text_field = 'abstract')
  }
  
  corpus_tokens <- tokens(articles_corpus, remove_punct = T, remove_symbols = F,
                          remove_numbers = F, remove_url = T, 
                          remove_separators = F, split_hyphens = F)
  articles_dfm <- dfm(corpus_tokens)
  
  if(rmstopwords){
    articles_dfm <- dfm_remove(articles_dfm,pattern = stopwords('english'))
  }
  
  if(rmnumbers){
    articles_dfm <- dfm_remove(articles_dfm, pattern = c("^\\d+\\.*,*-*\\d*$", "^\\W$"), 
                               valuetype = "regex")
  }
  
  if(stemming) {
    articles_dfm <- dfm_wordstem(articles_dfm)
  }
  
  articles_dfm <- dfm_trim(articles_dfm, min_docfreq = ndoc(articles_dfm)/100, max_docfreq = (ndoc(articles_dfm)*98)/100)
  
  results <- list()
  results$articles_corpus <- articles_corpus
  results$corpus_tokens <- corpus_tokens
  results$articles_dfm <- articles_dfm
  
  return(results)
}