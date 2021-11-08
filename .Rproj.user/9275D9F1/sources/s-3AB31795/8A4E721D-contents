# Define UI for application that draws a histogram
ui <-
  navbarPage(
    title = "BioTextQuest v2.0",
    windowTitle = "Welcome to version 2.0 of BioTestQuest",
    theme = shinytheme("flatly"),
    tabPanel(
      "Home",
      
      div(includeHTML("www/description.html")),
      themeelector()
      
    ),
    #tabPanel Start
    tabPanel(
      "Start",
      # Application title
      h3("Basic Parameters of the Analysis"),
      hr(),
      fluidRow(
        column(3),
        column(6,
               textInput("query", label = h3("Query"), 
                         placeholder = "Enter your query here...",
                         width = '100%'),
               bsTooltip("query", 
                         title = "The keywords you enter will be used to query PubMed and retrieve relevant documents",
                         placement = "right", options = list(container = "body"))
               ),
        column(3)
      ),
      fluidRow(
        column(3),
        column(6,
                 wellPanel(
                 radioButtons("targetdb", "Use terms from database:",
                              choices = c("Pubmed" = "pubmed",
                                "Extract" = "extract"
                                ),
                              inline = TRUE
                              ),
                 bsTooltip("targetdb", 
                 title = "Use either terms from the PubMed retrieved or use terms from the Extract database extracted for the retrieved documents",
                           placement = "right", options = list(container = "body")),
                 
                 radioButtons("aspects", "PubMed aspects to use:",
                              choices = c("abstracts" = "abstracts",
                                          "MeSH terms" = "mesh",
                                          "Both" = "both"),
                              inline = TRUE
                 ),
                 # alternative can use bsButton and bsPopover
                 bsTooltip("aspects", 
                           title = "Use words from abstract or only MeSH terms or both",
                           placement = "right", options = list(container = "body"))
                 )
               
              
               ),
        column(3)
      ),
      # Row for buttons Run Analysis and Reset
      fluidRow(
        column(3),
        column(6, align = "center",
               actionButton("run_analysis", "Run analysis",class = "btn-primary"),
               actionButton("reset_input", "Reset inputs", class = "btn-warning")
               ),
        column(3)
      ),# Row for buttons Run Analysis and Reset
      
      #Row for Advanced parameters
      fluidRow(
        column(3),
        column(6,
               tags$h3("Advanced parameters"),
               wellPanel(
                 checkboxInput("stemming", label = "Remove common word suffixes (Stemming)",
                               FALSE),
                 bsTooltip("stemming", 
                           title = "Porter stemming",
                           placement = "right", options = list(container = "body")
                           ),
                 selectInput("similarity", "Similarity matrix:",
                             c("Cosine" = "cos",
                               "Jaccard similarity" = "jaccard",
                               "Euclidean" = "euclidean",
                               "Relaxed Word Movers" = "rwmd",
                               "BM25" = "bm25")
                             ),
                 bsTooltip("similarity", 
                           title = "Select the metric to use for calculating the distance/similarity between the retrieved documents",
                           placement = "right", options = list(container = "body")),
                 selectInput("clustering_algo", "Clustering algorithm:",
                             c("Kmeans" = "kmeans",
                               "MCL" = "mcl")
                             ),
                 numericInput("threshold", "Threshold:", 0.05,
                              min = 0, max = 1, step = 0.01)
               )
        ),
        column(3)
      )
      
      
      
      
    ),# tabPanel Start
    
    tabPanel("Results",
             titlePanel("Results of the Analysis"),
             tabsetPanel(id="results_tabs", type="tabs",
                         tabPanel(id="query_results", title = "The results of the Query",
                                  DT::dataTableOutput("query_results")
                                  ), # tabPanel query_results
                         tabPanel(id="clusters", title = "The clustering results",
                                  DT::dataTableOutput("cluster_results")
                                  ) #tabPanel clusters
                         ) #tabsetPanel
             ), #tabPanel Results
    
    tabPanel("Help",
             titlePanel("Help for Proteosign ver. 2.0"))
    
    
  ) # navbarPage
