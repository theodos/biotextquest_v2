# Define UI
ui <- tagList(
  useShinyjs(debug=TRUE),
  navbarPage(
    title = "BioTextQuest v2.0",
    windowTitle = "Welcome to version 2.0 of BioTestQuest",
    theme = shinytheme("flatly"),
    id = "navbarPage",
    tabPanel(
      "Home",
      div(column(width=3), column(width = 6, includeHTML("www/description3.html"), align="center"), column(width=3)),
      themeelector()

    ),
    
    #It worked here, might not be the best place to put it however. This gets the data
    # for the wordcloud from the server.
    #tags$script(src = "wordcloud2.js"),
    #tags$script(src = "generateCloud.js"),
    
    #tabPanel Start
    tabPanel(
      "Start",
      # Application title
      div(id="myModal",
          HTML("
            <div class='modal-content'>
              <!--<span class='close'>&times;</span>-->
              <div id='myProgress'>
                <div id='myBar'></div>
              </div>
            </div>
          ")
      ),
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
                         placement = "right", options = list(container = "body")),
               tags$p("The query must return more than 25 articles for proper results", style="font-style: italic; color: grey; text-align:center;")
               ),
        column(3)
      ),
      
      # Row for buttons Run Analysis and Reset
      fluidRow(
        column(3),
        column(6, align = "center",
               actionButton("run_analysis", "Run analysis",class = "btn-primary", style="margin-right:12px"),
               actionButton("reset_inputs", "Reset inputs", class = "btn-warning", style="margin-right:12px"),
               actionButton("toggle_advanced", "Advanced", class="btn-secondary")
               ),
        column(3)
      ),# Row for buttons Run Analysis and Reset
      
      #Row for Advanced parameters
      #shinyjs::hidden(
      div(id="advanced",
      fluidRow(
        column(3),
        column(6,
               tags$h3("Advanced parameters"),
               
               wellPanel(
                 tags$p("If you want to use the old version of BioTextQuest please follow the ", a("link", href="http://bioinformatics.med.uoc.gr/biotextquest")),
                 # radioButtons(inputId = "targetdb", "Use terms from database:",
                 #              choices = c("Extract" = "extract",
                 #                          "Pubmed" = "pubmed"
                 #                          
                 #              ),
                 #              inline = TRUE
                 # ), #radioButtons
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
                             # choices = c("All" = "",
                             #             "PubChem Compound identifiers" = "pubchem",
                             #             "NCBI Taxonomy entries" = "ncbi",
                             #             "Gene Ontology biological process terms" = "go_biological",
                             #             "Gene Ontology cellular component terms" = "go_cellular",
                             #             "Gene Ontology molecular function terms" = "go_molecular",
                             #             "BRENDA Tissue Ontology terms" = "brenda",
                             #             "Disease Ontology terms" = "disease",
                             #             "Environment Ontology terms" = "enviroment")
                             choices = c("All" = "",
                                           "Genes/Proteins of a specific organism" = "0>",
                                           "PubChem Compound identifiers" = "-1",
                                           "NCBI Taxonomy entries" = "-2",
                                           "Gene Ontology biological process terms" = "-21",
                                           "Gene Ontology cellular component terms" = "-22",
                                           "Gene Ontology molecular function terms" = "-23",
                                           "BRENDA Tissue Ontology terms" = "-25",
                                           "Disease Ontology terms" = "-26",
                                           "Environment Ontology terms" = "-27")
                 ), #selectInput
                 bsTooltip("entity_types_parameters", 
                           title = "Choose the entity types you want to use from extract",
                           placement = "right", options = list(container = "body")
                 ),
                 # checkboxInput("stemming", label = "Remove common word suffixes (Stemming)",
                 #               TRUE),
                 # bsTooltip("databaseChoice", 
                 #           title = "Select which version of PubMed will be used",
                 #           placement = "right", options = list(container = "body")),
                 # radioButtons("databaseChoice", "Pubmed database:",
                 #             c("Local" = FALSE,
                 #               "Online" = TRUE)
                 #             #"Hierarchical: agglomerative" = "hier_agglo")
                 # ),
                 bsTooltip("stemming", 
                           title = "Porter stemming",
                           placement = "right", options = list(container = "body")
                           ),
                 selectInput("similarity", "Similarity matrix:",
                             c("Cosine" = "cosine",
                               "Jaccard" = "jaccard",
                               "Euclidean" = "euclidean")
                             ),
                 bsTooltip("similarity", 
                           title = "Select the metric to use for calculating the distance/similarity between the retrieved documents",
                           placement = "right", options = list(container = "body")),
                 selectInput("clustering_algo", "Clustering algorithm:",
                             c("K-means (>25 articles)" = "kmeans",
                               "MCL (>120 articles)" = "mcl",
                               "Louvain (>120 articles)" = "louvain",
                               "Top2Vec (>120 articles)" = "top2vec")
                               #"Hierarchical: agglomerative" = "hier_agglo")
                             ),
                 uiOutput("algo_params"),
                 bsTooltip("algo_params", 
                           title = "Check the Help tab for details",
                           placement = "right", options = list(container = "body")),
                 sliderInput("maxDocs", "Total papers:", value=10000, step = 1000, min = 1000, max=10000),
                 bsTooltip("maxDocs", 
                           title = "The maximum  number of papers that will be analyzed",
                           placement = "right", options = list(container = "body")),
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
      #) # shinyjs::hidden
      
      
      
      
    ),# tabPanel Start
    
    tabPanel("Results",
             value = "resultTab",
             titlePanel("Results of the Analysis"),
             tabsetPanel(id="results_tabs", type="tabs",
                         #tabPanel(id="query_results", title = "First 100 Documents retrieved",
                                  #DT::dataTableOutput("query_results")
                                  #), # tabPanel query_results
                         tabPanel(id="docs_clusters", title = "Clustered documents",
                                  textOutput("clusterSummary"),
                                  DT::dataTableOutput('summaryTable'),
                                  DT::dataTableOutput("cluster_results")
                                  ), #tabPanel clusters
                         # tabPanel(id="docs_clusters_graph", title = "Graphs of clustered documents",
                         #          DT::dataTableOutput("cluster_results")
                         # ), #tabPanel clusters
                         tabPanel(id="terms_clusters",value="wordcloudPanel", title="Wordclouds",
                                  #sidebarPanel(
                                    #selectInput("entity_types_plots", "Choose entities:",
                                                #multiple = TRUE,
                                                #choices = ""
                                                #)#selectInput
                                  #), # sidebarPanel
                                  mainPanel(
                                  div( id='wordcloudPanel',
                                    h3("Wordcloud divided into clusters"),
                                    actionButton("reloadClouds", "Reload Wordclouds", class = "btn-info", style="margin-right:30%"),
                                    
                                    div( id='entityCheckboxes',
                                    # Input: Slider for the number of words in each cloud
                                    numericInput(inputId = "totalWords",
                                                  label = "Total words in cloud",
                                                  min = 10,
                                                  max = 500,
                                                  value = 50, width = '40%'),
                                      HTML("<input type='button' value='Download wordclouds' id='btnConvert'>"),
                                      HTML("<p id='focusCloud' style='margin-top:5%;'>Wordcloud in focus: Cluster 1</p>"),
                                      h4("Select Entity Types:"),
                                      #actionButton("toggle_entityColor", "Uncheck all", class="btn-secondary"),# TODO: this needs fixing
                                      HTML("
                                        <br>
                                        <input type='checkbox' class='entityColor' id='genesProteins' onclick='selectEntity()' checked>
                                        <label for='genesProteins'  style='color:#807689;' onmouseover='func()' onmouseout='func1()'>Genes/Proteins of a specific organism</label><br>
                                      "),
                                    ),
                                    plotOutput("tagcloud_plot")#, height = "1000px"),
                                    #div(id='terms_plot'),
                                    #div(
                                      #HTML("<h3 style='padding: 100px;'>Most frequent terms per cluster</h3>"),
                                      #h3("Most frequent terms per cluster"),
                                      #plotOutput("most_freq_terms", height = "900px") #TODO: Maybe on load after wordcloud
                                    #),
                                    ) # mainPanel
                                    #DT::dataTableOutput("clusters_of_terms")
                                  ), #wordcloud Div
                                    ), #tabPanel terms_clusters
                         tabPanel(id="biomed_terms", title="Biomedical terms",
                                  DT::dataTableOutput("biomed_terms")
                                  ),
                         ) #tabsetPanel
             ), #tabPanel Results
    
    tabPanel("Help",
             titlePanel("Help for BioTextQuest ver. 2.0"),
             div(includeHTML("www/Help_page/textQuest2.html"))
             #includeHTML("www/Help_page/textQuest.html")
    
    )
  ), # navbarPage
  
  #It worked here, might not be the best place to put it however. This gets the data
  # for the wordcloud from the server.
  tags$script(src = "wordcloud2.js"),
  tags$script(src = "generateCloud2.js"),
  tags$script(src = "html2canvas.js"),
  tags$script(src = "general3.js"),
  tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/jspdf/1.5.3/jspdf.min.js"),
  #tags$script(src="generateExtractCloud.js")
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  #tags$style(type="text/css", "body { overflow-y: scroll; }")
  
) # tagList

 # Copyright [2023] [copyright holder]
 # 
 # Licensed under the BSD 3-Clause License.
 # See https://opensource.org/license/bsd-3-clause/ for more information.
 