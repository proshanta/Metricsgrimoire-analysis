shiny.maxRequestSize=30*1024^2
library(shiny)

shinyServer(function(input, output) {
  output$Degree <- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    #print(inFile$name)  
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0))
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
             quote=input$quote)
        
      
      hist(file$degree, xlab = i, col="RosyBrown", main=paste("Histogram for ",inFile$name[i]))
     
      
    }
    
    
  })
  
  output$Betweenness <- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    
    
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      hist(file$betweenness,xlab = i ,col="salmon",main=paste("Histogram for ",inFile$name[i]))
      
    }
  })
  output$Clusteringcoeff <- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      hist(file$clustcoeff,xlab = i,col="turquoise",main=paste("Histogram for ",inFile$name[i]))
      
    }    
  })
  
  output$Closeness<- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    
    
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      hist(file$closeness,xlab = i ,col="SeaGreen",main=paste("Histogram for ",inFile$name[i]))
      
    }    
  })
  
  output$eigen <- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    
    
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      hist(file$eigencentrality,xlab = i,col="sky blue",main=paste("Histogram for ",inFile$name[i]))
      
    }    
  })
  
  output$pagerank <- renderPlot(width=700 ,height=800 ,res=96,{
    
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
    
    
    
    par(mfrow = c(len,1) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      hist(file$pagerank,xlab = i,col="grey",main=paste("Histogram for ",inFile$name[i]))
      
    }    
  })
})
