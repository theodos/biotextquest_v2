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
  #source('trimBy_TfIdf.R')
  source('testExtract2.R')
  source('handle_long_extract.R')
  
  #Logs
  #track_usage(storage_mode = store_json(path = "logs/"))
  
  #Accept cookies
  observeEvent(input$navbarPage, {
    # if(input$navbarPage == "Home"){
    #   #runjs("alert('This website uses cookies to ensure you get the best experience on our website.');")
    # }
  })
  
  # Depending on the selected clustering algorithm show the relevant only to the
  # specific algorithm parameters.
  output$algo_params <- renderUI({
    
    switch(input$clustering_algo,
           "louvain" = sliderInput("clustering_param", "Resolution", 1, step = 0.1, min = 0, max = 10),
           "kmeans" = numericInput("clustering_param", "Number of clusters:", 3, step = 1, min = 2),
           "mcl" = numericInput("clustering_param", "Inflation:", 1.8, step = 0.1, min = 1.5, max = 4),
           #"top2vec" = numericInput("clustering_param", "Number of clusters:", 2, step = 1, min = 2),
           "top2vec" = "",
           "hier_agglo" = numericInput("clustering_param", "Number of clusters:", 2, step = 1, min = 2),
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
    # runjs("
    #      var showButton = document.getElementById('toggle_advanced');
    #      if(showButton.innerHTML == 'Show Advanced') {
    #         showButton.innerHTML = 'Hide Advanced';
    #       }else{
    #         showButton.innerHTML = 'Show Advanced';
    #       }
    #       ")
  })
  
  # observe({
  #   print(input$navbarPage)
  # })
  
  observeEvent(input$navbarPage, {
    if(input$navbarPage == "Start"){
      shinyjs::toggle(id = "advanced", anim = FALSE)
      #runjs("console.log('At start')
            #var showButton = document.getElementById('toggle_advanced');
            #showButton.innerHTML = 'Show Advanced';
            #document.getElementById('advanced').hidden = true ")
    }
  })
  
  # Toggle the text in the Uncheck all button. If the entities in the results/wordcloud tab
  # are shown the button should write "Uncheck all". Else "Check all"
  observeEvent(input$toggle_entityColor, {
    runjs("
          var showButton = document.getElementById('toggle_entityColor');
          var allCheckboxes = document.getElementById('entityCheckboxes').getElementsByClassName('entityColor');
          if(showButton.innerHTML == 'Uncheck all') {
            showButton.innerHTML = 'Check all';
            
            for (checkbox of allCheckboxes) {
              checkbox.checked = false;
            };
          }else{
            showButton.innerHTML = 'Uncheck all';
            
            for (checkbox of allCheckboxes) {
              checkbox.checked = true;
            };
          }
          ")
  })
  
  # Hide or show stemming & entity types in the parameters depending on the
  # selected database. For Extract the entity types should be shown, but not the
  # stemming. The opposite for Pubmed.
  observeEvent(input$targetdb, {
    
    shinyjs::hide(id = "stemming")
    shinyjs::show(id = "entity_types_parameters")
    
    
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
    
    #Check that query is not empty
    req(input$query)
    
    #Redirect to results
    updateTabsetPanel(session, "navbarPage", selected = "resultTab")
    
    #Show progressBar
    runjs("var modal = document.getElementById('myModal');
          modal.style.display = 'block';")
    
    #Terminate all previous connections to extract DB
    killDbConnections()
    
    # show_modal_progress_line(
    #   color = "#DF0101",
    #   duration = 900,
    #   easing = "easeOut",
    #   text = "Starting Analysis"
    # )
    
    
    query <- input$query
    
    runjs("document.getElementById('myBar').style.width='10%';")
    
    #TODO put the df_of_articles creation here and if >200 split and merge here
    #TODO make sure each id only appears once after the split and merges
    #TODO increase progress bar on 'long extract parts'
    
    #Check if pubmed or extract is selected
    #TODO optimize the if-else, assign only the correct value to each variable and then use the
    # same code for both options
    
    allResults <- list()
    
    #processedDf <- function() {
      #if (input$targetdb == "extract"){
        runjs("console.log('Passed 0')")
        
        runjs("document.getElementById('myBar').style.width = '20%';")
        
        #Check the length of the query
        splitted_query <- strsplit(query, " +")
        totalWords <- sapply(splitted_query, length)
        
        # observe({
        #   if(totalWords<24){
        #     showNotification("Not enough papers found. Please reload the page", type="error")
        #   }
        # })
        
        #If it is too long split it before processing
        if (totalWords > 200){
        #if (totalWords < 0){
          runjs("console.log('It is above 200 ids')")
          
          allSubstrings <- vector()
          for (str in splitted_query) {
            allSubstrings <-append(allSubstrings, str)
          }
          
          wordIdx <- 0
          partialList <- vector()
          allPartialLists <- list()
          for (str in allSubstrings) {

            #if (wordIdx<200 || wordIdx > 200) {
            if (wordIdx<200) {
            #if (wordIdx<20) {
              #runjs("console.log('Got here')")
              partialList <- append(partialList, str)
            }else{
              allPartialLists <- append(allPartialLists, list(partialList))
              
              if (length(partialList)>100) {
                runjs("console.log('partialList > 100')")
              }else{
                runjs("console.log('partialList < 100')")
              }
              
              partialList <- vector()
              partialList <- append(partialList, str)
              wordIdx <- 0
            }
            wordIdx <- wordIdx+1
          }
            
            runjs("console.log('Long extract part 1')")
            
            if (length(allPartialLists)>= 1) {
              runjs("console.log('allPartialLists >= 1')")
            }else{
              runjs("console.log('allPartialLists < 1')")
            }
            
            #Combine data
            
            #NEW data combination 
            combined_data <- list()
            list_of_articles <- list()
            #columns = c("pmid", "year", "title", "abstract")
            #df_of_articles <- data.frame(matrix(nrow = 0, ncol = length(columns)))
            
            killDbConnections()
            
            for (list in allPartialLists) {
              runjs("console.log('elem in allPartialLists')")
              id_string <- paste(list, collapse = " ")
              partialData <- get_extract_data(id_string, input$entity_types_parameters, input$maxDocs)
              runjs("console.log('Long extract part 1.1')")
              combined_data <- c(combined_data, partialData)
              runjs("console.log('Long extract part 1.2')")
              
              partialPubmedData <- get_pubmed_data(id_string, remote=FALSE)
              #df_of_articles <- merge(x = df_of_articles, y = partialPubmedData, all=TRUE)
              #df_of_articles <- c(list_of_articles, partialPubmedData)
              list_of_articles <- rbind(list_of_articles, partialPubmedData)
            }
            #extractResults <- get_long_extract_data
            #totalData <- do.call(rbind, combined_data)  # Using rbind
            
            # Print the final data frame 
            #print(combined_data)
            #print(list_of_articles)
            runjs("console.log('Long extract part 2')")
            extractResults <- combined_data
            df_of_articles <- data.frame(pmid = list_of_articles$pmid, year = list_of_articles$year,  title = list_of_articles$title,  abstract = list_of_articles$abstract)
            
            
            # #totalData <- data.frame()
            # columns= c("ID","PMID","Word","EntityType") 
            # totalData = data.frame(matrix(nrow = 0, ncol = length(columns))) 
            # df_of_articles <- data.frame(matrix(nrow = 0, ncol = length(columns))) #The column names are wrong, but the number is correct
            # 
            # # assign column names
            # colnames(totalData) = columns
            

            #Here it gets different from pubmed to extract
    #         for (list in allPartialLists) {
    #           runjs("console.log('elem in allPartialLists')")
    #           partialData <- get_partial_extract(toString(list), input$entity_types_parameters)
    #           
    #           #This is for the clustered documents of the split and merge
    #           partialIds <- get_pubmed_ids(toString(list))
    #           partialPubmedData <- get_pubmed_data((partialIds), remote=FALSE)
    # 		killDbConnections()
    #           if (nrow(partialData)>100) {
    #             runjs("console.log('partialData > 100')")
    #           }else{
    #             runjs("console.log('partialData < 100')")
    #           }
    #           
    #           totalData <- merge(x = totalData, y = partialData, all = TRUE)
    #           df_of_articles <- merge(x = df_of_articles, y = partialPubmedData, all=TRUE)
    #         }
    #         
    #         if (nrow(totalData)>10) {
    #           runjs("console.log('totalData > 10')")
    #         }else{
    #           runjs("console.log('totalData < 10')")
    #         }
    #         
    #        if ("EntityType" %in% colnames(totalData)){
    #          runjs("console.log('EntityType exists')")
    #        }else{
    #          runjs("console.log('EntityType does not exist')")
    #        }
    #         
    #         runjs("console.log('Long extract part 2')")
    #         
    #         #Get info for wordclouds etc
    #         extractResults <- get_long_extract_data(totalData)
    #         
    #         runjs("console.log('Long extract part 3')")
            
        }else{
          extractResults <- get_extract_data(query, input$entity_types_parameters, input$maxDocs)
          runjs("console.log('It is below 200 ids')")
          
          #Fetch pubmed data (abstracts etc)
          #my_query <- get_pubmed_ids(query)
          
          # fetch data and save them in a data frame
          df_of_articles <- get_pubmed_data(query)
        }
       
        runjs("document.getElementById('myBar').style.width = '20%';")  
        
        #Για την extract βάζω τον πίνακα με το colsum όπως είναι
        colnames(extractResults$term_frequency)[which(names(extractResults$term_frequency) == "term_frequency")] #<- "newColName"
        runjs("console.log('Before Biomed')")  
        output$biomed_terms <- DT::renderDataTable({
          # DT::datatable(as.data.frame(extractResults$term_frequency),
          #               extensions = 'Buttons',
          #               options = list(buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          #                              lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All'))
          #               ) #options
          DT::datatable(as.data.frame(extractResults$term_frequency),
                        filter = 'top',
                        extensions = 'Buttons',
                        options = list(dom = 'Blfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                       lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')))

          ) #DT::datatable
        })
        runjs("console.log('After Biomed')")
        allResults$article_dfm_forClustering <- extractResults$articles_dfm
        allResults$articles_dfm <- extractResults$term_frequency
        allResults$wordTypeTb <- extractResults$wordTypeIDTb #this can be omitted/used directly
        allResults$untokenizedExtract <- extractResults$untokenizedExtract
        
        #results <- list()
        #results$article_dfm_forCluster <- extractResults$articles_dfm
        #results$articles_dfm_terms <- extractResults$term_frequency
        
        #Possible problems here as clustering_results is created further down, but this output is called
        # later, so ok for now
        runjs("document.getElementById('myBar').style.width = '30%';")
        # output$cluster_results <- DT::renderDataTable({
        #   df_with_cluster <- inner_join(clustering_results,
        #                                 df_of_articles[,c("pmid", "year", "title", "abstract")],
        #                                 by="pmid")
        #   #clustering_results
        #   pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
        #   df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
        #   DT::datatable(df_with_cluster,
        #                 filter = 'top',
        #                 extensions = 'Buttons',
        #                 options = list(dom = 'Blfrtip',
        #                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        #                                lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
        #                                pageLength = 15),
        #                 escape = c(3,4,5,6)
        #   )# DT::datatable
        # 
        # }) # output to DataTable the retrieved documents
        
        observeEvent(input$reloadClouds, {
          runjs("console.log('Reloading Clouds...')")
          disable(id="reloadClouds")
          
          docIds <- docnames(allResults$article_dfm_forClustering)
          idsToRemove <- c()
          for(i in 1:length(docIds)) {
            if (!(docIds[i] %in% clustering_results$pmid)){
              idsToRemove <- append(idsToRemove, i)
              print(i)
            }
          }
          
          runjs("console.log('In render plot: After trim')")
          if (length(idsToRemove)>0) {
            trimmedDfm <-allResults$article_dfm_forClustering[-idsToRemove,]
            session$sendCustomMessage(type = "wordType", allResults$wordTypeTb)
            session$sendCustomMessage(type = "Handler", textstat_frequency(trimmedDfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
          }else{
            session$sendCustomMessage(type = "wordType", allResults$wordTypeTb)
            session$sendCustomMessage(type = "Handler", textstat_frequency(allResults$article_dfm_forClustering %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
          }
          
          runjs("console.log('In render plot')")
          runjs("console.log('Custom message sent')")
          runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")
          enable(id="reloadClouds")
        })
        
        output$tagcloud_plot <- renderPlot({
          #session$sendCustomMessage(type = "wordType", allResults$wordTypeTb)
          
          runjs("console.log('In render plot: Before trim')")
          #Trim the dfm if the post-cluster dfm is smaller

          docIds <- docnames(allResults$article_dfm_forClustering)
          idsToRemove <- c()
          for(i in 1:length(docIds)) {
            if (!(docIds[i] %in% clustering_results$pmid)){
              idsToRemove <- append(idsToRemove, i)
              print(i)
            }
          }

          runjs("console.log('In render plot: After trim')")
          if (length(idsToRemove)>0) {
            trimmedDfm <-allResults$article_dfm_forClustering[-idsToRemove,]
            session$sendCustomMessage(type = "wordType", allResults$wordTypeTb)
            session$sendCustomMessage(type = "Handler", textstat_frequency(trimmedDfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
          }else{
            session$sendCustomMessage(type = "wordType", allResults$wordTypeTb)
            session$sendCustomMessage(type = "Handler", textstat_frequency(allResults$article_dfm_forClustering %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
            }
          
          #session$sendCustomMessage(type = "Handler", textstat_frequency(allResults$article_dfm_forClustering %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
          runjs("console.log('In render plot')")
          runjs("console.log('Custom message sent')")
          runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")

        })
        
        #return(results)
  
      # }else{
      #   
      #   # update_modal_progress(
      #   #   value = 2,
      #   #   text = "Step 1: Retrieve  PubMed documents."
      #   # )
      #   
      #   runjs("document.getElementById('myBar').style.width = '20%;'")
      #   
      #   
      #   #Check the length of the query
      #   splitted_query <- strsplit(query, " +")
      #   totalWords <- sapply(splitted_query, length)
      #   
      #   #If it is too long split it before processing
      #   if (totalWords > 200){
      #     runjs("console.log('It is above 200 ids')")
      #     
      #     allSubstrings <- vector()
      #     for (str in splitted_query) {
      #       allSubstrings <-append(allSubstrings, str)
      #     }
      #     
      #     wordIdx <- 0
      #     partialList <- vector()
      #     allPartialLists <- list()
      #     for (str in allSubstrings) {
      #       
      #       #if (wordIdx<200 || wordIdx > 200) {
      #       if (wordIdx<200) {
      #         runjs("console.log('Got here')")
      #         partialList <- append(partialList, str)
      #       }else{
      #         allPartialLists <- append(allPartialLists, list(partialList))
      #         
      #         if (length(partialList)>10) {
      #           runjs("console.log('partialList > 10')")
      #         }else{
      #           runjs("console.log('partialList < 10')")
      #         }
      #         
      #         partialList <- vector()
      #         partialList <- append(partialList, str)
      #         wordIdx <- 0
      #       }
      #       wordIdx <- wordIdx+1
      #     }
      #     
      #     runjs("console.log('Long pubmed part 1')")
      #     
      #     if (length(allPartialLists)>= 1) {
      #       runjs("console.log('allPartialLists >= 1')")
      #     }else{
      #       runjs("console.log('allPartialLists < 1')")
      #     }
      #     
      #     #Combine data
      #     #totalData <- data.frame()
      #     columns= c("ID","PMID","Word","EntityType") 
      #     totalData = data.frame(matrix(nrow = 0, ncol = length(columns))) 
      #     
      #     # assign column names
      #     colnames(totalData) = columns
      #     
      #     #Here it gets different form pubmed to extract
      #     for (list in allPartialLists) {
      #       runjs("console.log('elem in allPartialLists')")
      #       partialIds <- get_pubmed_ids(toString(list))
      #       
      #       partialData <- get_pubmed_data(partialIds)
      #       
      #       if (nrow(partialData)>100) {
      #         runjs("console.log('partialData > 100')")
      #       }else{
      #         runjs("console.log('partialData < 100')")
      #       }
      #       
      #       totalData <- merge(x = totalData, y = partialData, all = TRUE)
      #     }
      #     
      #     if (nrow(totalData)>10) {
      #       runjs("console.log('totalData > 10')")
      #     }else{
      #       runjs("console.log('totalData < 10')")
      #     }
      #     
      #     if ("EntityType" %in% colnames(totalData)){
      #       runjs("console.log('EntityType exists')")
      #     }else{
      #       runjs("console.log('EntityType does not exist')")
      #     }
      #     
      #     runjs("console.log('Long pubmed part 2')")
      #     
      #     #Get info for wordclouds etc
      #     #pubmedResults <- get_long_extract_data(totalData)
      #     
      #     runjs("console.log('Long pubmed part 3')")
      #     df_of_articles <- totalData
      #     
      #   }else{
      #   
      #     my_query <- get_pubmed_ids(query)
      #     
      #     # fetch data and save them in a data frame
      #     df_of_articles <- get_pubmed_data(my_query)
      #     
      #     # output to DataTable the first 100 retrieved documents
      #     output$query_results <- DT::renderDataTable({
      #       # Show an excerpt of the results. Only 100 first documents
      #       df_of_articles[,c("pmid", "year", "title", "abstract")] %>% head(n=100)
      #     }) # output to DataTable the retrieved documents
      #     
      #     # update_modal_progress(
      #     #   value = 4,
      #     #   text = "Step 3: Vectorization of retrieved PubMed documents"
      #     # )
      #   }
      #   allResults$articles_dfm <- make_dfm(df_of_articles, stemming = input$stemming)$articles_dfm
      #   
      #   output$biomed_terms <- DT::renderDataTable({
      #     DT::datatable(textstat_frequency(allResults$articles_dfm)[,c(1,2,4)],#H αγκύλη μόνο στην pubmed
      #                   options = list(lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All'))
      #                   ) #options
      #     ) #DT::datatable
      #   })
      #   
      #   runjs("console.log('Pubmed part 1')")
      #   
      #   runjs("document.getElementById('myBar').style.width = '30%';")
      #   # Calculate TF IDF 
      #   allResults$articles_dfm_tfidf <- dfm_tfidf(allResults$articles_dfm) # %>% round(digits = 3)
      #   runjs("console.log('Pubmed part 1.5')")
      #   allResults$article_dfm_forClustering <- allResults$articles_dfm_tfidf
      #   
      #   runjs("console.log('Pubmed part 2')")
      #   
      #   #clustering_results <- make_clustering(article_dfm_forClustering, 
      #                                         #input$clustering_algo, 
      #                                         #clustering_param = input$clustering_param, 
      #                                         #dist_sim_param = input$similarity)$df
      #   
      #   #output$tagcloud_plot <- renderPlot({
      #     #session$sendCustomMessage(type = "Handler", textstat_frequency(articles_dfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
      #     #runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")
      #     
      #   #})
      #   
      #   #results <- list()
      #   #results$article_dfm_forCluster <- articles_dfm_tfidf
      #   #results$articles_dfm_terms <- articles_dfm
      #   
      #   #return(results)
      #   
      #   # output$cluster_results <- DT::renderDataTable({
      #   #   df_with_cluster <- inner_join(clustering_results,
      #   #                                 df_of_articles[,c("pmid", "year", "title", "abstract")],
      #   #                                 by="pmid")
      #   #   pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
      #   #   df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
      #   #   DT::datatable(df_with_cluster,
      #   #                 filter = 'top',
      #   #                 extensions = 'Buttons',
      #   #                 options = list(dom = 'Blfrtip',
      #   #                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #   #                                lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
      #   #                                pageLength = 15),
      #   #                 escape = c(3,4,5,6)
      #   #   )# DT::datatable
      #   # 
      #   # }) # output to DataTable the retrieved documents
      #   
      #   
      #   
      #   
      #   output$tagcloud_plot <- renderPlot({
      #     
      #     #Trim the dfm if the post-cluster dfm is smaller
      #     # docIds <- docnames(allResults$article_dfm_forClustering)
      #     # idsToRemove <- c()
      #     # for(i in 1:length(docIds)) {
      #     #   if (!(docIds[i] %in% clustering_results$pmid)){
      #     #     idsToRemove <- append(idsToRemove, i)
      #     #     print(i)
      #     #   }
      #     # }
      #     # 
      #     # trimmedDFm <-allResults$article_dfm_forClustering[-idsToRemove,]
      #     
      #     session$sendCustomMessage(type = "Handler", textstat_frequency(allResults$articles_dfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
      #     runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")
      # 
      #   })
      # }#else
    #}
    
    runjs("document.getElementById('myBar').style.width = '40%';")
    output$clusterSummary <- renderText({
      analyzed <- length(clustering_results$pmid)
      available <- length(df_of_articles[,"pmid"])
      if (analyzed<available) {
        available<-analyzed
      }
      paste("Total articles analyzed: ", analyzed,"| Available locally:", available)
    })
    
    output$summaryTable <- DT::renderDataTable({
      counts<-table(clustering_results[, 2])
      countDf <- as.data.frame.table(counts)
      colnames(countDf) <-c("Cluster", "Total articles")
      DT::datatable(countDf,
                    escape = c(1),
                    rownames= FALSE
      )#DT::datatable
    })
    
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
    
    
    #article_dfm_forClustering <- processedDf$article_dfm_forCluster
    #articles_dfm <- processedDf$articles_dfm_terms
    
    # clustering results is a data frame with pmid & cluster
    # The results is a named list. The "df" contains the data frame 
    # with columns "pmid" & "cluster". The "model" contains the model of the
    # clustering.
    # clustering_results <- make_clustering( allResults$article_dfm_forClustering, 
    #                                       input$clustering_algo, 
    #                                       clustering_param = input$clustering_param, 
    #                                       dist_sim_param = input$similarity)$df
    
    clustering_results <- make_clustering( allResults, 
                                           input$clustering_algo,
                                           clustering_param = input$clustering_param,
                                           dist_sim_param = input$similarity)$df

    runjs("console.log('Main 1')")
    
    update_modal_progress(
      value = 8,
      text = paste("Step 7: Computing clusters based on the ", input$clustering_algo, " algorithm")
    )
    
    runjs("document.getElementById('myBar').style.width = '100%';")
    
    runjs("modal.style.display = 'none';")
    
    
    #Probably is not working with extract because it wants to show articles
    # output$cluster_results <- DT::renderDataTable({
    #   df_with_cluster <- inner_join(clustering_results,
    #                                 df_of_articles[,c("pmid", "year", "title", "abstract")],
    #                                 by="pmid")
    #   pmidurl <- function(x) { paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/',x,'" target=_blank>',x,'</a>',sep='') }
    #   df_with_cluster$pmid<-sapply(df_with_cluster$pmid,FUN = pmidurl)
    #   DT::datatable(df_with_cluster,
    #                 filter = 'top',
    #                 extensions = 'Buttons',
    #                 options = list(dom = 'Blfrtip',
    #                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                                lengthMenu = list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')),
    #                                pageLength = 15),
    #                 escape = c(3,4,5,6)
    #   )# DT::datatable
    #   
    # }) # output to DataTable the retrieved documents
    
    #output$tagcloud_plot <- renderPlot({
    #articles_dfm %>% dfm_group(groups = clustering_results$cluster) %>%
    #dfm_trim(min_termfreq = 2, verbose = FALSE) %>% 
    #textplot_wordcloud(comparison = TRUE, max_words = 900)
    #})
    
    #I added this
    #session$sendCustomMessage(type = "Handler", textstat_frequency(articles_dfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
    
    # output$tagcloud_plot <- renderPlot({
    #   session$sendCustomMessage(type = "Handler", textstat_frequency(allResults$articles_dfm %>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster))
    #   runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")
    #   
    # })
    
    #output$tagcloud_plot <- renderPlot({
      #session$sendCustomMessage(type = "Handler", textstat_frequency(as.data.frame(allResults$articles_dfm)%>% dfm_weight(), n=input$totalWords, groups = clustering_results$cluster)) #Do I need textstat for extract? I also need clustering results
      #runjs("for (var cluster of allClusters) {buildWordcloud(cluster)}")
    #})
    
    runjs("console.log('Main 2')")
    
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
    runjs("console.log('Main 3')")
    
    remove_modal_progress()
    
    
  }) #observerEvent run_analysis
  
}
