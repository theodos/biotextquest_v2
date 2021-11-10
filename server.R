
# Define server logic required to draw a histogram
function(input, output, session) {
  
 #load PMIDs from a file --- Implemented in a later version 
  # data_pmids <- reactive({
  #   req(input$file_pd)
  #   
  #   ext <- tools::file_ext(input$file_pd$name)
  #   switch(ext,
  #          csv = vroom::vroom(input$file_pd$datapath, delim = ","),
  #          tsv = vroom::vroom(input$file_pd$datapath, delim = "\t"),
  #          txt = vroom::vroom(input$file_pd$datapath, delim = "\t"),
  #          validate("Invalid file; Please upload a .csv or .tsv/txt file")
  #   )
  # })
  
  # retrievePMIDs <- reactive ({
  #   query <- input$query
  #   query_result <- get_pubmed_ids(query)
  #   fetch_all_pubmed_ids(query_result)
  # })
  # 
  
    
output$algo_params <- renderUI({
  
  switch(input$clustering_algo,
         "kmeans" = numericInput("kmeans_noclusters", "Number of clusters:", 2),
         "mcl" = numericInput("mcl_inflation", "Inflation:", 2.2)
         )
 
})
  
 observeEvent(input$run_analysis, {
   
   show_modal_progress_line(
     color = "#DF0101",
     duration = 900,
     easing = "easeOut",
     text = "Starting Analysis"
   )
   
     query <- input$query
     
     update_modal_progress(
       value = 2,
       text = "Step 1: Retrieve  PubMed documents."
     )
     
     my_query <- get_pubmed_ids(query)
     #pmids <- fetch_all_pubmed_ids(query_result)
     # Fetch data
     my_abstracts_xml <- fetch_pubmed_data(my_query)  
     
     # Store Pubmed Records as elements of a list
     all_xml <- articles_to_list(my_abstracts_xml)
     
     update_modal_progress(
       value = 3,
       text = "Step 2: Converting retrieved PubMed documents to table format"
     )
     # convert xml PubMed documents to data frame
     df_of_articles <- do.call(rbind, lapply(all_xml, article_to_df, 
                                             max_chars = -1, getAuthors = FALSE))
     
     # output to DataTable the first 100 retrieved documents
   output$query_results <- DT::renderDataTable({
     # Show an excerpt of the results. Only 100 first documents
     df_of_articles[,c("pmid", "year", "title", "abstract")] %>% head(n=100)
     
   }) # output to DataTable the retrieved documents
   
   update_modal_progress(
     value = 4,
     text = "Step 3: Tokenization of retrieved PubMed documents words"
   )
   it_train = itoken(df_of_articles$abstract, preprocessor = prep_fun, tokenizer = tok_fun, ids = df_of_articles$pmid, progressbar = FALSE)
   vocab = create_vocabulary(it_train)
   
   update_modal_progress(
     value = 5,
     text = "Step 4: Representing each PubMed document as a vector of terms"
   )
   vectorizer = vocab_vectorizer(vocab)
   dtm_train = create_dtm(it_train, vectorizer)
   
   update_modal_progress(
     value = 6,
     text = "Step 5: Calculating for each term its TF.IDF weight"
   )
   
   tfidf = TfIdf$new()
   dtm_train_tfidf = fit_transform(dtm_train, tfidf)
   
   update_modal_progress(
     value = 7,
     text = paste("Step 6: Calculating similarity matrix between docuemnts based on the ", input$similarity, " metric")
   )
   
   d1_d2_jac_sim = sim2(dtm_train_tfidf, dtm_train_tfidf, method = "jaccard", norm = "none")
   
   update_modal_progress(
     value = 8,
     text = paste("Step 7: Computing clusters based on the ", input$clustering_algo, " algorithm")
   )
   
   #clusters = kmeans(d1_d2_jac_sim, 3)$cluster
   output$cluster_results <- DT::renderDataTable({
     clustering_model <- kmeans(d1_d2_jac_sim, 3)
     df_clustering <- fortify(clustering_model)
     df_clustering$pmid <- rownames(df_clustering)
     df_with_cluster <- inner_join(df_clustering, 
                                   df_of_articles[,c("pmid", "year", "title", "abstract")], 
                                   by='pmid')
     pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
     df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
     DT::datatable(df_with_cluster, 
                   filter = 'top',
                   extensions = 'Buttons',
                   options = list(dom = 'Blfrtip', 
                                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
                                  pageLength = 15),
                   escape = c(2,4,5,6)
                   )# DT::datatable
     
   }) # output to DataTable the retrieved documents
   
   remove_modal_progress()
   
 }) #observerEvent run_analysis
  
  
  
  

  
}

