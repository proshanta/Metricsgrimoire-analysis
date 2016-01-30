shinyUI(fluidPage(
  titlePanel("Network Vertex Metrics Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose multiple CSV files from local drive, adjusting parameters if necessary',
                multiple = TRUE,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
      
        
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Degree",
                 plotOutput("Degree"),               
                 value = 1),
        tabPanel("Betweenness",
                 plotOutput("Betweenness"),
                 value=2),
        tabPanel("Clustering Coefficient",
                 plotOutput("Clusteringcoeff"),               
                 value = 3),
        tabPanel("Closeness",
                 plotOutput("Closeness"),
                 value=4),
        tabPanel("Eigen Centrality",
                 plotOutput("eigen"),
                 value=5),
        tabPanel("Page Rank",
                 plotOutput("pagerank"),
                 value=6),
        id="tabs1")
  )
)))
