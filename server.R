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
  
  
source('get_pubmed_data.R')
source('make_dfm.R')
source('make_clustering.R')
source('functionaly_annotate_clusters.R')
  
# Depending on the selected clustering algorithm show the relevant only to the
# specific algorithm parameters.
output$algo_params <- renderUI({
  
  switch(input$clustering_algo,
         "kmeans" = numericInput("clustering_param", "Number of clusters:", 2, step = 1, min = 2),
         "mcl" = numericInput("clustering_param", "Inflation:", 2.2, step = 0.1, min = 1.5, max = 4),
         "hier_agglo" = numericInput("clustering_param", "Number of clusters:", 2, step = 1, min = 2)
         )
 
})

# Reset the input parameters
observeEvent(input$reset_inputs, {
   reset("query")
   reset("advanced")
})  

# Toggle the text in the Show Advanced button. If the advanced parameters
# are shown the button should write "Hide Advanced". "Show Advanced" if the
# Advacned Parameters are hidden.
observeEvent(input$toggle_advanced, {
   shinyjs::toggle(id = "advanced", anim = TRUE)
   runjs("
          var showButton = document.getElementById('toggle_advanced');
          if(showButton.innerHTML == 'Show Advanced') {
            showButton.innerHTML = 'Hide Advanced';
          }else{
            showButton.innerHTML = 'Show Advanced';
          }
          ")
})

# Hide or show stemming & entity types in the parameters depending on the
# selected database. For Extract the entity types should be shown, but not the
# stemming. The opposite for Pubmed.
observeEvent(input$targetdb, {
   if(input$targetdb == "extract") {
      shinyjs::hide(id = "stemming")
      shinyjs::show(id = "entity_types_parameters")
   }
   else {
      shinyjs::show(id = "stemming")
      shinyjs::hide(id = "entity_types_parameters")
   }
})

# Code to have the same entity choices everywhere as in the parameters given at
# the start of the analysis. The user must not be able to see entity types
# that he/she has not included in the parameters of the analysis.
entity_choices = reactive({
   mydata <- input$entity_types_parameters
   entity_choices_global[entity_choices_global %in% mydata]
})

observe({
   if(is.null(input$entity_types_parameters)){
      updateSelectInput(session, "entity_types_plots",
                        choices = entity_choices_global
      )
   }
   else {
   updateSelectInput(session, "entity_types_plots",
                     choices = entity_choices(),
                     selected = entity_choices()
                     )
   }
   })


# When the analysis button is pressed run the following code to perform the
# analysis
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
    
     # fetch data and save them in a data frame
     df_of_articles <- get_pubmed_data(my_query)
     
     # output to DataTable the first 100 retrieved documents
   output$query_results <- DT::renderDataTable({
     # Show an excerpt of the results. Only 100 first documents
     df_of_articles[,c("pmid", "year", "title", "abstract")] %>% head(n=100)
   }) # output to DataTable the retrieved documents
   
   update_modal_progress(
     value = 4,
     text = "Step 3: Vectorization of retrieved PubMed documents"
   )
   
     articles_dfm <- make_dfm(df_of_articles, stemming = input$stemming)$articles_dfm
     
     output$biomed_terms <- DT::renderDataTable({
       DT::datatable(textstat_frequency(articles_dfm),
                     options = list(lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All'))
                     ) #options
       ) #DT::datatable
     })
     
     # Calculate TF IDF 
     articles_dfm_tfidf <- dfm_tfidf(articles_dfm) # %>% round(digits = 3)
     
     # clustering results is a data frame with pmid & cluster
     # The results is a named list. The "df" contains the data frame 
     # with columns "pmid" & "cluster". The "model" contains the model of the
     # clustering.
   clustering_results <- make_clustering(articles_dfm_tfidf, 
                                         input$clustering_algo, 
                                         clustering_param = input$clustering_param, 
                                         dist_sim_param = input$similarity)$df
   
   update_modal_progress(
     value = 8,
     text = paste("Step 7: Computing clusters based on the ", input$clustering_algo, " algorithm")
   )
   
  
  
   output$cluster_results <- DT::renderDataTable({
     df_with_cluster <- inner_join(clustering_results,
                                   df_of_articles[,c("pmid", "year", "title", "abstract")],
                                   by="pmid")
     pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
     df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
     DT::datatable(df_with_cluster,
                   filter = 'top',
                   extensions = 'Buttons',
                   options = list(dom = 'Blfrtip',
                                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
                                  pageLength = 15),
                   escape = c(3,4,5,6)
     )# DT::datatable
     
   }) # output to DataTable the retrieved documents
   
   
  
   output$tagcloud <- renderUI({
      HTML(functionaly_annotate_clusters(articles_dfm, clustering_results))
   })
   
   output$tagcloud_plot <- renderPlot({
      articles_dfm %>% dfm_group(groups = clustering_results$cluster) %>%
         dfm_trim(min_termfreq = 2, verbose = FALSE) %>% 
         textplot_wordcloud(comparison = TRUE, max_words = 900)
   })

   output$most_freq_terms <- renderPlot({
      dfm_weight <- articles_dfm %>% dfm_weight(scheme = "prop")
      # Calculate relative frequency by cluster
      freq_weight <- textstat_frequency(dfm_weight, n = 15, 
                                        groups = clustering_results$cluster)
      ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
         geom_point() +
         facet_wrap(~ group, scales = "free") +
         coord_flip() +
         scale_x_continuous(breaks = nrow(freq_weight):1,
                            labels = freq_weight$feature) +
         labs(x = NULL, y = "Relative frequency")
   })
   
   remove_modal_progress()
   
 }) #observerEvent run_analysis
  
  
  }