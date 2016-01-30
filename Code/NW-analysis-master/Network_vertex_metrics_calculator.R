library('tools')
library('igraph')
library('Matrix')
library('blockmodeling')
choice<- readline(prompt="Enter your choice, 1. Directed Network\n2.Undirected graph\n")
directory <- readline(prompt="Enter The Folder Location: ")
filenames <- list.files(path=directory,pattern="*.net")
man<-typeof(filenames)
len=length(filenames)

prompt <- "Select the metrics you want to calculate (space-separated list) (Don't choose 1 and 2 for un-directed)\n
1.Indegree\n2.Outdegree\n3.Degree\n4.Betweenness\n5.Clustering Coeff\n6.Closeness\n7.Eigen Centrality\n8.PageRank \n"
EXP <- as.integer(strsplit(readline(prompt), " ")[[1]])
print(choice)
if (choice==1){
  
  
  for(i in 1:len)
  {
    
    ptr <- filenames[i]  
    temp<- file_path_sans_ext(ptr)
    #print(ptr)
    if(file.exists(ptr))
    {
      setwd(file.path(paste(directory)))
    }
    else
    {
      dir.create(file.path(paste(directory)))
      setwd(file.path(paste(directory)))
    }
    name <- paste(directory,ptr,sep="\\")
    x <- read.graph(name,format="pajek")
    
    labels<-vertex_attr(x,"id", index = V(x))
    degree_in <- degree(x,v=V(x),mode=c("in"))
    degree_out<-degree(x,v=V(x),mode=c("out"))
    degree<-degree(x,v=V(x),mode=c("total"))
    
    betweenness<-betweenness(x,v=V(x), directed=TRUE)
    
    closeness <- closeness(x, vids=V(x),mode=c("all"),normalized=TRUE)
    
    pagerank_temp<-page.rank(x,vids=V(x),directed=TRUE)
    
    pagerank<-pagerank_temp$vector
    
    clustcoeff <-transitivity(x,type=c("local"))
    
    eigencentrality_temp <-evcent(x,directed=TRUE)
    
    eigencentrality<-eigencentrality_temp$vector    
    
    file1=paste(temp,"-metrics",".csv",sep="")
    
    
    write.table(cbind(labels,degree_in,degree_out,degree,betweenness,clustcoeff,closeness,eigencentrality,pagerank), 
                file1, row.names = FALSE,
                col.names=c('labels','in_degree','out_degree','degree','betweenness','clustcoeff','closeness','eigencentrality','pagerank'),sep=",",na="0")
    
    
    file <- read.csv(file1)
    
    small <- subset(file,,EXP)
    
    write.csv(small,file1)
    
  }
}
if (choice==2)
{
  for(i in 1:len)
  {
    
    ptr <- filenames[i]  
    temp<- file_path_sans_ext(ptr)
    if(file.exists(ptr))
    {
      setwd(file.path(paste(directory)))
    }
    else
    {
      dir.create(file.path(paste(directory)))
      setwd(file.path(paste(directory)))
    }
    name <- paste(directory,ptr,sep="\\")
    x <- read.graph(name,format="pajek")
    
    labels<-vertex_attr(x,"id", index = V(x))
    degree_in <-degree(x,v=V(x))
    degree_out<-degree(x,v=V(x))
    degree <- degree(x,v=V(x))
    
    betweenness<-betweenness(x,v=V(x), directed=FALSE)
    
    closeness <- closeness(x, vids=V(x),normalized=TRUE)
    
    pagerank_temp<-page.rank(x,vids=V(x),directed=FALSE)
    
    pagerank<-pagerank_temp$vector
    
    clustcoeff <-2*transitivity(x,type=c("local"))
    
    eigencentrality_temp <-evcent(x, directed=FALSE)
    
    eigencentrality<-eigencentrality_temp$vector    
    
    file1=paste(temp,"-metrics",".csv",sep="")
    
    write.table(cbind(labels,degree_in,degree_out,degree,betweenness,clustcoeff,closeness,eigencentrality,pagerank), 
                file1, row.names = FALSE,
                col.names=c('labels','in_degree','out_degree','degree','betweenness','clustcoeff','closeness','eigencentrality','pagerank'),sep=",", na="0")
    
    
    file <- read.csv(file1)
    
    small <- subset(file,,EXP)
    small[is.na(small)] <- 0
    print (EXP)
    write.csv(small,file1)
    
  }
  
}

