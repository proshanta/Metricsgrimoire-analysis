library(shiny)

shinyServer(function(input, output) {
  
  output$Degree<- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      
      min <- 0
      
      plot(file$X,file$degree_centralization,lwd = 2, xlab = "Network",ylab="Degree Centraliztion", main = paste("Line plot of Degree Centrality"),col="coral",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  output$Betweenness<- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      
      min <- 0
      
      plot(file$X,file$betweenness_centralization,lwd = 2, xlab = "Network",ylab="Betweenness Centralization", main = paste("Line plot of Betweenness Centrality"),col="DarkRed",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  output$Closeness<- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      
      min <- 0
      
      plot(file$X,file$closeness_centralization,lwd = 2, xlab = "Network",ylab="Closeness Centralization", main = paste("Line plot of Closeness Centrality"),col="OrangeRed",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  output$Eigenvector<- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      
      min <- 0
      
      plot(file$X,file$eigenvector_centralization,lwd = 2, xlab = "Network",ylab="Eigenvector Centralization", main = paste("Line plot of Eigenvector Centrality"),col="MidnightBlue",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  
  output$cluster <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    for(i in 1:len)
    {
      
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      plot(file$X,file$global_clustcoeff,lwd = 2,xlab = "Network",ylab="Global Clustering Coefficient", main = paste("Line plot of Average Clustering Coefficient"),col="salmon",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }
  })
  output$Assortativity <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      plot(file$X,file$assortativity,lwd = 2,xlab = "Network",ylab="Assortativity", main = paste("Line plot of Assortativity" ),col="turquoise",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }    
  })
  output$diameter <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      plot(file$X,file$diameter,lwd = 2,xlab = "Network",ylab="Diameter", main = paste("Line plot of Diameter" ),col="Tomato",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }    
  })
  
  output$avgdegree <- renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$avg_degree,lwd = 2,xlab = "Network",ylab="Average Degree", main = paste("Line plot of Average Degree"),col="SeaGreen",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }    
  })
  
  output$modularity <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$modularity,lwd = 2,xlab = "Network",ylab="Modularity", main = paste("Line plot of Modularity"),col="sky blue",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }    
  })
  
  output$density <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$density,lwd = 2,xlab = "Network",ylab="Density", main = paste("Line plot of Density"),col="Black",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
      
    }    
  })
  
  output$avgsep <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$avg_path_length,lwd = 2,xlab = "Network",ylab="Average Separation", main = paste("Line plot of Average Separation "),col="BurlyWood",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  output$vc <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$vertices,lwd = 2,xlab = "Network",ylab="Vertices", main = paste("Line plot of Vertices "),col="BurlyWood",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
  output$ec <- renderPlot({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    for(i in 1:len)
    {
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      print(max)
      plot(file$X,file$edges,lwd = 2,xlab = "Network",ylab="edges", main = paste("Line plot of Edges "),col="BurlyWood",type="l")
      axis(side=1, at=seq(min,max,by=1))
      
    }    
  })
})
