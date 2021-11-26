# Define UI
ui <- tagList(
  useShinyjs(debug=TRUE),
  navbarPage(
    title = "BioTextQuest v2.0",
    windowTitle = "Welcome to version 2.0 of BioTestQuest",
    theme = shinytheme("flatly"),
    tabPanel(
      "Home",
      div(column(width=3), column(width = 6, includeHTML("www/description.html"), align="center"), column(width=3)),
      themeelector()

    ),
    #tabPanel Start
    tabPanel(
      "Start",
      # Application title
      h3("Parameters of the Analysis", style="text-align:center"),
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
      
      # Row for buttons Run Analysis and Reset
      fluidRow(
        column(3),
        column(6, align = "center",
               actionButton("run_analysis", "Run analysis",class = "btn-primary", style="margin-right:12px"),
               actionButton("reset_inputs", "Reset inputs", class = "btn-warning", style="margin-right:12px"),
               actionButton("toggle_advanced", "Show Advanced", class="btn-secondary")
               ),
        column(3)
      ),# Row for buttons Run Analysis and Reset
      
      #Row for Advanced parameters
      shinyjs::hidden(
      div(id="advanced",
      fluidRow(
        column(3),
        column(6,
               tags$h3("Advanced parameters"),
               
               wellPanel(
                 radioButtons(inputId = "targetdb", "Use terms from database:",
                              choices = c("Extract" = "extract",
                                          "Pubmed" = "pubmed"
                                          
                              ),
                              inline = TRUE
                 ), #radioButtons
                 bsTooltip("targetdb", 
                           title = "Use either terms from the PubMed retrieved or use terms from the Extract database extracted for the retrieved documents",
                           placement = "right", options = list(container = "body")),
                 
                 # radioButtons("aspects", "PubMed aspects to use:",
                 #              choices = c("abstracts" = "abstracts",
                 #                          "MeSH terms" = "mesh",
                 #                          "Both" = "both"),
                 #              inline = TRUE
                 # ),
                 # alternative can use bsButton and bsPopover
                 # bsTooltip("aspects", 
                 #           title = "Use words from abstract or only MeSH terms or both",
                 #           placement = "right", options = list(container = "body")),
                 selectInput("entity_types_parameters", "Choose extract entities:",
                             multiple = TRUE,
                             choices = c("All" = "",
                                         "PubChem Compound identifiers" = "pubchem",
                                         "NCBI Taxonomy entries" = "ncbi",
                                         "Gene Ontology biological process terms" = "go_biological", 
                                         "Gene Ontology cellular component terms" = "go_cellular",
                                         "Gene Ontology molecular function terms" = "go_molecular",
                                         "BRENDA Tissue Ontology terms" = "brenda",
                                         "Disease Ontology terms" = "disease",
                                         "Environment Ontology terms" = "enviroment")
                 ), #selectInput
                 bsTooltip("entity_types_parameters", 
                           title = "Choose the entity types you want to use from extract",
                           placement = "right", options = list(container = "body")
                 ),
                 checkboxInput("stemming", label = "Remove common word suffixes (Stemming)",
                               FALSE),
                 bsTooltip("stemming", 
                           title = "Porter stemming",
                           placement = "right", options = list(container = "body")
                           ),
                 selectInput("similarity", "Similarity matrix:",
                             c("Cosine" = "cosine",
                               "Jaccard similarity" = "jaccard",
                               "Euclidean" = "euclidean")
                             ),
                 bsTooltip("similarity", 
                           title = "Select the metric to use for calculating the distance/similarity between the retrieved documents",
                           placement = "right", options = list(container = "body")),
                 selectInput("clustering_algo", "Clustering algorithm:",
                             c("K-means" = "kmeans",
                               "MCL" = "mcl",
                               "Hierarchical: agglomerative" = "hier_agglo")
                             ),
                 uiOutput("algo_params")
                # numericInput("threshold", "Threshold:", 0.05,
                 #             min = 0, max = 1, step = 0.01),
                # bsTooltip("threshold", 
                #           title = "The threshold under which the documntes are considered to have no relation (values: 0...1), e.g. 0.05",
                #           placement = "right", options = list(container = "body"))
               )
        ),
        column(3)
      ) #fluidrow
      ) #div
      ) # shinyjs::hidden
      
      
      
      
    ),# tabPanel Start
    
    tabPanel("Results",
             titlePanel("Results of the Analysis"),
             tabsetPanel(id="results_tabs", type="tabs",
                         tabPanel(id="query_results", title = "First 100 Documents retrieved",
                                  DT::dataTableOutput("query_results")
                                  ), # tabPanel query_results
                         tabPanel(id="docs_clusters", title = "Clustered documents",
                                  DT::dataTableOutput("cluster_results")
                                  ), #tabPanel clusters
                         tabPanel(id="terms_clusters", title="Clusters of terms of the documents",
                                  sidebarPanel(
                                    selectInput("entity_types_plots", "Choose entities:",
                                                multiple = TRUE,
                                                choices = ""
                                                )#selectInput
                                  ), # sidebarPanel
                                  mainPanel(
                                  htmlOutput("tagcloud"),
                                  h3("Wordcloud divided into clusters"),
                                  plotOutput("tagcloud_plot", height = "600px"),
                                  h3("Most frequent terms per cluster"),
                                  plotOutput("most_freq_terms", height = "900px")
                                  ) # mainPanel
                                  #DT::dataTableOutput("clusters_of_terms")
                                  ), #tabPanel terms_clusters
                         tabPanel(id="biomed_terms", title="Biomedical terms",
                                  DT::dataTableOutput("biomed_terms")
                                  )
                         ) #tabsetPanel
             ), #tabPanel Results
    
    tabPanel("Help",
             titlePanel("Help for BioTextQuest ver. 2.0"))
    
    
  ) # navbarPage
) # tagList
